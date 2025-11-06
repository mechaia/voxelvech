use {
    super::Scenario,
    core::mem,
    crate::{
        gfx::{Gfx, MeshSetHandle},
        physics::Physics,
        vehicle::{Vehicle, VehicleSound},
        SoundsHandler,
    },
    mechaia::{
        math::{Quat, Vec3},
        model::Collection,
        physics3d::{ColliderProperties, RigidBodyHandle, ShapeHandle},
        render::resource::camera::{CameraProjection, CameraView},
        script,
        util::{Arena, ArenaHandle, Transform},
    },
    std::{collections::HashMap, time::{Duration, Instant}},
};

const MAX_PHYSICS_STEPS: usize = 8;

pub struct PilScenario {
    physics: Physics,
    instances: Arena<ScriptInstance>,
    vehicles: Arena<Vehicle>,
    collections: Arena<Collection>,
    mesh_sets: Arena<MeshSetHandle>,
    rigid_bodies: Arena<RigidBodyHandle>,
    shapes: Arena<ShapeHandle>,
    physics_step_start: Instant,
    vehicle_sound: VehicleSound,
    camera_transform: Transform,
}

struct ScriptInstance {
    executable: script::Executable,
    instance: script::Instance,
    args: HashMap<Box<[u8]>, ScriptArg>,
    script_dir: Box<str>,
    waiting: bool,
}

enum ScriptArg {
    Int32(u32),
    Path(Box<[u8]>),
}

#[derive(Debug)]
struct Sys {
    id: u32,
    name: &'static str,
    inputs: &'static [&'static str],
    outputs: &'static [&'static str],
}

macro_rules! sys {
    [@ $prev:ident] => {};
    [@ $prev:ident $name:ident $($rest:ident)*] => {
        const $name: u32 = $prev + 1;
        sys![@ $name $($rest)*];
    };
    [@@ $name:ident $($rest:ident)*] => {
        const $name: u32 = script::Executable::RESERVED_MAX_ID + 1;
        sys![@ $name $($rest)*];
    };
    [## $($const:ident $name:literal [$($inputs:literal)*] [$($outputs:literal)*])*] => {
        const SYS_TABLE: &[Sys] = &[
            $(Sys { id: $const, name: $name, inputs: &[$($inputs,)*], outputs: &[$($outputs,)*] },)*
        ];
    };
    [$($const:ident $name:literal [$($inputs:literal)*] [$($outputs:literal)*])*] => {
        sys![@@ $($const)*];
        sys![## $($const $name [$($inputs)*] [$($outputs)*])*];
    };
}
sys![
    SYS_WAIT "game.wait" [] ["int1:0"]
    SYS_ARG_INT32 "game.arg.int32.path" [] ["int32:0"]
    SYS_ARG_PATH "game.arg.path.path" [] []
    SYS_GLTF_LOAD "game.gltf.load" [] ["game.collection:0"]
    //SYS_VEHICLE_SET_LINEAR_VELOCITY "game.vehicle.set.linear_velocity" ["game.vehicle:0" "vec3"] []
    //SYS_VEHICLE_SET_ANGULAR_VELOCITY "game.vehicle.set.angular_velocity" ["game.vehicle:0" "vec3"] []
    SYS_ADD_MESH_SET  "game.graphics.meshset.add" ["game.collection:0"] ["game.meshset:0"]
    SYS_GRAPHICS_DRAW_PUSH_TRANSFORM "game.graphics.draw.push_transform" ["transform:0" "fp32:0"] []
    SYS_GRAPHICS_DRAW_MESH "game.graphics.draw.mesh" ["game.meshset:0" "int32:0" "int32:1"] []
    SYS_INPUT_GET "game.input.get.path" [] ["fp32:0"]
    SYS_PHYSICS_RIGIDBODY_ADD_FIXED "game.physics.rigidbody.add.fixed" [] ["game.rigidbody:0"]
    SYS_PHYSICS_RIGIDBODY_ADD_DYNAMIC "game.physics.rigidbody.add.dynamic" [] ["game.rigidbody:0"]
    SYS_PHYSICS_RIGIDBODY_GET_TRANSFORM "game.physics.rigidbody.get.transform" ["game.rigidbody:0"] ["transform:0"]
    SYS_PHYSICS_RIGIDBODY_SET_TRANSFORM "game.physics.rigidbody.set.transform" ["game.rigidbody:0" "transform:0"] []
    SYS_PHYSICS_SHAPE_MAKE_COMPOUND_PUSH "game.physics.shape.make.compound.push" ["game.shape:0"] []
    SYS_PHYSICS_SHAPE_MAKE_COMPOUND "game.physics.shape.make.compound" [] ["game.shape:0"]
    SYS_PHYSICS_SHAPE_MAKE_CUBOID "game.physics.shape.make.cuboid" ["vec3:0"] ["game.shape:0"]
    SYS_PHYSICS_SHAPE_MAKE_HALFSPACE "game.physics.shape.make.halfspace" ["vec3:0"] ["game.shape:0"]
    SYS_PHYSICS_COLLIDER_ADD "game.physics.collider.add"
        ["game.rigidbody:0" "game.shape:0" "transform:0" "fp32:0" "fp32:1" "int32:0"]
        ["game.collider:0"]
    SYS_PATH_PUSH "game.path.push" ["int8:0"] []
    SYS_PATH_LEN "game.path.len" [] ["int32:0"]
    SYS_PATH_GET "game.path.get" ["int32:0"] ["int8:0"]
    SYS_PATH_CLEAR "game.path.clear" [] []
    SYS_CAMERA_SET_TRANSFORM "game.camera.set.transform" ["transform:0"] []
    //SYS_CAMERA_SET_PROJECTION_PERSPECTIVE
    //SYS_CAMERA_SET_PROJECTION_ORTHOGONAL
    SYS_SCRIPT_ARGS_CLEAR "game.script.args.clear" [] []
    SYS_SCRIPT_ARGS_PUSH_KEY "game.script.args.push_key.path" [] []
    SYS_SCRIPT_ARGS_PUSH_VALUE_INT32 "game.script.args.push_value.int32" ["int32:0"] []
    SYS_SCRIPT_LAUNCH "game.script.launch.path" [] ["game.script:0"]
    SYS_VEHICLE_LOAD "game.vehicle.load.path" ["int32:0"] ["game.vehicle:0"]
    SYS_VEHICLE_GET_TRANSFORM "game.vehicle.get.transform" ["game.vehicle:0"] ["transform:0"]
    SYS_VEHICLE_SET_TRANSFORM "game.vehicle.set.transform" ["game.vehicle:0" "transform:0"] []
    SYS_VEHICLE_SET_INPUT "game.vehicle.set.input.path" ["game.vehicle:0" "fp32:0"] []
    SYS_DEBUG_FP32 "game.debug.fp32" ["const_str:0" "fp32:0"] []
];

enum Stage<'a, 'b> {
    Step { gfx: &'a mut Gfx },
    Draw { draw: &'a mut crate::gfx::Draw<'b> },
}

impl PilScenario {
    pub fn new(script: &str, state: &mut crate::State, gfx: &mut Gfx) -> (Self, SoundsHandler) {
        let mut slf = Self {
            instances: Default::default(),
            physics: Physics::new(),
            vehicles: Default::default(),
            collections: Default::default(),
            mesh_sets: Default::default(),
            rigid_bodies: Default::default(),
            shapes: Default::default(),
            vehicle_sound: Default::default(),
            physics_step_start: state.time.now,
            camera_transform: Default::default(),
        };

        slf.launch_script(script, [(b"player_vehicle"[..].into(), ScriptArg::Path(b"user:__deathray_test.vvt"[..].into()))].into());

        slf.run_scripts(state, Stage::Step { gfx });
        (slf, Box::new(|_| ()))
    }

    fn run_scripts(&mut self, state: &mut crate::State, mut stage: Stage<'_, '_>) {
        // FIXME avoid redundant collect
        let keys = self.instances.keys().collect::<Vec<_>>();
        for k in keys {
            // FIXME bro
            let stage = match &mut stage {
                Stage::Step { gfx } => Stage::Step { gfx },
                Stage::Draw { draw } => Stage::Draw { draw },
            };
            self.run_script(k, state, stage);
        }
    }

    fn run_script(
        &mut self,
        script_i: ArenaHandle,
        state: &mut crate::State,
        mut stage: Stage<'_, '_>,
    ) {
        let mut script = &mut self.instances[script_i];

        if script.waiting {
            script.outputs(
                SYS_WAIT,
                &[match stage {
                    Stage::Step { .. } => 0,
                    Stage::Draw { .. } => 1,
                }],
            );
            script.waiting = false;
        }

        let mut transform_buffer = Vec::new();
        let mut path = Vec::new();
        let mut shape_compound = Vec::new();
        let mut script_args = HashMap::new();
        let mut script_arg_key = Box::<[u8]>::default();

        let stage = &mut stage;

        let parse_trf = |a: [u32; 7]| {
            let [q @ .., x, y, z] = a.map(f32::from_bits);
            Transform::new(Vec3::new(x, y, z), Quat::from_array(q))
        };
        let ser_trf = |t: Transform| {
            let mut a = [0.0; 7];
            a[..4].copy_from_slice(&t.rotation.to_array());
            a[4..].copy_from_slice(&t.translation.to_array());
            a
        };

        loop {
            match script.run() {
                script::Yield::Finish => {
                    self.instances.remove(script_i);
                    break;
                }
                script::Yield::Sys { id } => match id {
                    SYS_WAIT => {
                        script.waiting = true;
                        break
                    }
                    SYS_ARG_INT32 => {
                        let arg = &script.args[path.as_slice()];
                        let ScriptArg::Int32(arg) = arg else { todo!() };
                        script.outputs(id, &[*arg]);
                    }
                    SYS_ARG_PATH => {
                        let arg = &script.args[path.as_slice()];
                        let ScriptArg::Path(arg) = arg else { todo!() };
                        path.clear();
                        path.extend_from_slice(arg);
                    }
                    SYS_GLTF_LOAD => {
                        let data = script.read_bytes(&path);
                        let collection = mechaia::model::gltf::from_glb_slice(&data);
                        let index = self.collections.insert(collection);
                        script.outputs(id, &[index.as_u32()]);
                    }
                    SYS_ADD_MESH_SET => {
                        let Stage::Step { gfx } = stage else { todo!() };
                        let [collection] = script.inputs(id);
                        let collection = &self.collections[ArenaHandle::from_u32(collection)];
                        let set = gfx.add_mesh_set(collection);
                        let set = self.mesh_sets.insert(set);
                        script.outputs(id, &[set.as_u32()]);
                    }
                    SYS_GRAPHICS_DRAW_PUSH_TRANSFORM => {
                        let Stage::Draw { draw } = stage else { todo!() };
                        let [trf @ .., s] = script.inputs::<8>(id);
                        let trf = parse_trf(trf).with_scale(f32::from_bits(s));
                        transform_buffer.push(trf);
                    }
                    SYS_GRAPHICS_DRAW_MESH => {
                        let Stage::Draw { draw } = stage else { todo!() };
                        let [set, mesh, material] = script.inputs(id);
                        let set = self.mesh_sets[ArenaHandle::from_u32(set)];
                        draw.collector
                            .solid
                            .push(set, mesh, material, &transform_buffer);
                        transform_buffer.clear();
                    }
                    SYS_PHYSICS_RIGIDBODY_ADD_FIXED => {
                        let body = self.physics.engine.make_fixed_rigid_body();
                        dbg!(self.physics.engine.rigid_body_transform(body));
                        let body = self.rigid_bodies.insert(body);
                        script.outputs(id, &[body.as_u32()]);
                    }
                    SYS_PHYSICS_RIGIDBODY_ADD_DYNAMIC => {
                        let body = self.physics.engine.make_dynamic_rigid_body();
                        let body = self.rigid_bodies.insert(body);
                        script.outputs(id, &[body.as_u32()]);
                    }
                    SYS_PHYSICS_RIGIDBODY_GET_TRANSFORM => {
                        let [body] = script.inputs(id);
                        let body = self.rigid_bodies[ArenaHandle::from_u32(body)];
                        let trf = self.physics.engine.rigid_body_transform(body);
                        script.outputs(id, &ser_trf(trf).map(f32::to_bits));
                    }
                    SYS_PHYSICS_RIGIDBODY_SET_TRANSFORM => {
                        let [body, trf @ ..] = script.inputs::<8>(id);
                        let trf = parse_trf(trf);
                        let body = self.rigid_bodies[ArenaHandle::from_u32(body)];
                        self.physics.engine.set_rigid_body_transform(body, &trf);
                    }
                    SYS_PHYSICS_SHAPE_MAKE_COMPOUND_PUSH => {
                        let [shape, trf @ ..] = script.inputs::<8>(id);
                        let trf = parse_trf(trf);
                        let shape = self.shapes[ArenaHandle::from_u32(shape)];
                        shape_compound.push((trf, shape));
                    }
                    SYS_PHYSICS_SHAPE_MAKE_COMPOUND => {
                        let shape = self.physics.engine.make_compound_shape(&shape_compound);
                        shape_compound.clear();
                        let shape = self.shapes.insert(shape);
                        script.outputs(id, &[shape.as_u32()]);
                    }
                    SYS_PHYSICS_SHAPE_MAKE_CUBOID => {
                        let [x, y, z] = script.inputs(id).map(f32::from_bits);
                        let shape = self.physics.engine.make_cuboid_shape(Vec3::new(x, y, z));
                        let shape = self.shapes.insert(shape);
                        script.outputs(id, &[shape.as_u32()]);
                    }
                    SYS_PHYSICS_SHAPE_MAKE_HALFSPACE => {
                        let [x, y, z] = script.inputs(id).map(f32::from_bits);
                        dbg!(x, y, z);
                        let shape = self.physics.engine.make_halfspace_shape(Vec3::new(x, y, z));
                        let shape = self.shapes.insert(shape);
                        script.outputs(id, &[shape.as_u32()]);
                    }
                    SYS_PHYSICS_COLLIDER_ADD => {
                        let [body, shape, props @ ..] = script.inputs::<12>(id);
                        let body = self.rigid_bodies[ArenaHandle::from_u32(body)];
                        let shape = self.shapes[ArenaHandle::from_u32(shape)];
                        let [trf @ .., friction, bounciness, user_data] = props;
                        self.physics.engine.add_collider(
                            body,
                            shape,
                            dbg!(&ColliderProperties {
                                local_transform: parse_trf(trf),
                                friction: f32::from_bits(friction),
                                bounciness: f32::from_bits(bounciness),
                                user_data,
                            }),
                        );
                    }
                    SYS_VEHICLE_LOAD => {
                        let [user_data] = script.inputs(id);
                        let text = script.read_string(&path);
                        let mut vehicle = Vehicle::new(&mut self.physics, user_data);
                        vehicle.load_v0_text(
                            &mut self.vehicle_sound,
                            &state.block_set,
                            &mut self.physics,
                            &text,
                        );
                        let vehicle = self.vehicles.insert(vehicle);
                        script.outputs(id, &[vehicle.as_u32()]);
                    }
                    SYS_VEHICLE_GET_TRANSFORM => {
                        let [vehicle] = script.inputs(id);
                        let vehicle = &mut self.vehicles[ArenaHandle::from_u32(vehicle)];
                        let trf = vehicle.transform(&self.physics);
                        script.outputs(id, &ser_trf(trf).map(f32::to_bits));
                    }
                    SYS_VEHICLE_SET_TRANSFORM => {
                        let [vehicle, trf @ ..] = script.inputs::<8>(id);
                        let trf = parse_trf(trf);
                        dbg!(&trf);
                        let vehicle = &mut self.vehicles[ArenaHandle::from_u32(vehicle)];
                        vehicle.set_transform(&mut self.physics, &trf);
                    }
                    SYS_VEHICLE_SET_INPUT => {
                        let [vehicle, value] = script.inputs(id);
                        let vehicle = &mut self.vehicles[ArenaHandle::from_u32(vehicle)];
                        let name = core::str::from_utf8(&path).unwrap();
                        vehicle.set_input(state, &self.physics, name, f32::from_bits(value));
                    }
                    SYS_PATH_PUSH => {
                        let [c] = script.inputs(id);
                        path.push(c as u8);
                    }
                    SYS_PATH_GET => {
                        let [i] = script.inputs(id);
                        let c = path[i as usize];
                        script.outputs(id, &[u32::from(c)]);
                    }
                    SYS_PATH_LEN => {
                        let l = path.len() as u32;
                        script.outputs(id, &[l]);
                    }
                    SYS_PATH_CLEAR => {
                        path.clear();
                    }
                    SYS_CAMERA_SET_TRANSFORM => {
                        let trf = script.inputs(id);
                        self.camera_transform = parse_trf(trf);
                    }
                    SYS_SCRIPT_ARGS_CLEAR => {
                        script_args.clear();
                    }
                    SYS_SCRIPT_ARGS_PUSH_KEY => {
                        script_arg_key = mem::take(&mut path).into();
                    }
                    SYS_SCRIPT_ARGS_PUSH_VALUE_INT32 => {
                        let [n] = script.inputs(id);
                        script_args.insert(mem::take(&mut script_arg_key), ScriptArg::Int32(n));
                    }
                    SYS_SCRIPT_LAUNCH => {
                        let text = script.read_string(&path);
                        let dir = ""; // FIXME;
                        let h = self.launch_script_text(dir, &text, mem::take(&mut script_args));
                        script = &mut self.instances[script_i];
                        script.outputs(id, &[h.as_u32()]);
                    }
                    SYS_DEBUG_FP32 => {
                        let [s_ptr, s_len, x] = script.inputs(id);
                        let s = script.executable.const_str(s_ptr, s_len).unwrap();
                        let s = core::str::from_utf8(s).unwrap();
                        let x = f32::from_bits(x);
                        let i = script_i.as_u32();
                        crate::log::debug(format!("[script {i}] {s}: {x}"));
                    }
                    id => todo!("{id}: {:?}", SYS_TABLE.iter().find(|s| s.id == id)),
                },
            }
        }
    }

    fn launch_script(&mut self, path: &str, args: HashMap<Box<[u8]>, ScriptArg>) -> ArenaHandle {
        let text = crate::data::read_asset_string(path).unwrap();
        self.launch_script_text(path, &text, args)
    }

    fn launch_script_text(&mut self, path: &str, text: &str, args: HashMap<Box<[u8]>, ScriptArg>) -> ArenaHandle {
        let script_dir = path.rsplit_once('/').map_or("",|s| s.0);
        let (executable, _) = {
            let mut col = script::Collection::default();
            col.add_standard().unwrap();
            col.add_ieee754().unwrap();
            col.parse_text(Some("game_core.pil"), &crate::data::read_asset_string("game_core.pil").unwrap()).unwrap();
            col.parse_text(Some(path), text).unwrap();
            for sys in SYS_TABLE {
                col.add_sys(sys.name, sys.id, sys.inputs, sys.outputs)
                    .unwrap();
            }
            let (mut program, mut debug) = script::Program::from_collection(&col, &"start".into()).unwrap();
            if std::env::var("VOXELVECH_SCRIPT_DISABLE_OPTIMIZATION").is_ok_and(|v| ["", "0", "false"].contains(&&*v.to_ascii_lowercase())) {
                script::optimize::simple(&mut program, Some(&mut debug));
            }
            script::Executable::from_program(&program, Some(&mut debug))
        };
        dbg!(&executable);
        let instance = executable.new_instance();

        self.instances.insert(ScriptInstance {
            instance,
            executable,
            args,
            script_dir: script_dir.into(),
            waiting: false,
        })
    }
}

impl Scenario for PilScenario {
    fn step(&mut self, state: &mut crate::State, gfx: &mut Gfx) {
        let dt = Duration::from_secs_f32(self.physics.engine.time_delta());
        for _ in 0..MAX_PHYSICS_STEPS {
            let next_t = self.physics_step_start.checked_add(dt).unwrap();
            if next_t > state.time.now {
                break;
            }

            self.run_scripts(state, Stage::Step { gfx });
            for v in self.vehicles.values_mut() {
                v.step(&mut self.physics);
            }
            self.physics.engine.step();
            self.physics_step_start = next_t;
        }
    }

    fn draw(&mut self, state: &mut crate::State, draw: &mut crate::gfx::Draw<'_>) {
        self.run_scripts(state, Stage::Draw { draw });
        for v in self.vehicles.values() {
            v.render(state, &self.physics, 0.0, draw);
        }
    }

    fn camera(&self, aspect: f32) -> CameraView {
        CameraView {
            translation: self.camera_transform.translation,
            rotation: self.camera_transform.rotation,
            projection: CameraProjection::Perspective { fov: 3.1415 / 2.0 },
            aspect,
            near: 0.1,
            far: 1000.0,
        }
    }

    fn destroy(self: Box<Self>, state: &mut crate::State, gfx: &mut crate::gfx::Gfx) {
        for &mesh_set in self.mesh_sets.values() {
            gfx.remove_mesh_set(mesh_set);
        }
    }
}

impl ScriptInstance {
    fn run(&mut self) -> script::Yield {
        self.instance.run(&self.executable).unwrap()
    }

    fn inputs<const N: usize>(&self, id: u32) -> [u32; N] {
        self.instance.inputs::<N>(&self.executable, id).unwrap()
    }

    fn outputs(&mut self, id: u32, values: &[u32]) {
        self.instance.outputs(&self.executable, id, values).unwrap()
    }

    fn read_bytes(&mut self, path: &[u8]) -> Vec<u8> {
        let i = path.iter().position(|&c| c == b':').unwrap_or(0);
        let (root, path) = (&path[..i], &path[i + 1..]);
        // TODO don't strictly require utf-8
        let path = core::str::from_utf8(path).unwrap();
        match root {
            b"user" => crate::data::read_user(path).unwrap(),
            b"data" => crate::data::read_asset(path).unwrap(),
            b"self" => {
                let path = format!("{}/{}", self.script_dir, path);
                crate::data::read_asset(&path).unwrap()
            }
            _ => todo!("{}", String::from_utf8_lossy(root)),
        }
    }

    fn read_string(&mut self, path: &[u8]) -> String {
        String::from_utf8(self.read_bytes(path)).unwrap()
    }
}
