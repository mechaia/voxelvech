use {
    super::{Scenario, World},
    crate::{
        gfx::{Gfx, MeshSetHandle},
        physics::Physics,
        vehicle::Vehicle,
        SoundsHandler,
    },
    mechaia::{
        math::{Quat, Vec3},
        model::Collection,
        render::resource::camera::{CameraProjection, CameraView},
        script,
        util::{Arena, ArenaHandle, TransformScale},
    },
};

pub struct PilScenario {
    executable: script::Executable,
    instance: script::Instance,
    data_root: Box<str>,
    physics: Physics,
    vehicles: Arena<Vehicle>,
    collections: Arena<Collection>,
    mesh_sets: Arena<MeshSetHandle>,
}

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
    SYS_ARG "game.arg" [] ["const_str:0"]
    SYS_GLTF_LOAD "game.gltf.load" [] ["game.gltf:0"]
    SYS_VEHICLE_LOAD_V0 "game.vehicle.load.v0" [] ["game.vehicle:0"]
    SYS_ADD_MESH_SET  "game.graphics.meshset.add" ["game.gltf:0"] ["game.meshset:0"]
    SYS_GRAPHICS_DRAW_PUSH_TRANSFORM "game.graphics.draw.push_transform" ["fp32:0" "fp32:1" "fp32:2" "fp32:3" "fp32:4" "fp32:5" "fp32:6" "fp32:7"] []
    SYS_GRAPHICS_DRAW_MESH "game.graphics.draw.mesh" ["game.meshset:0" "int32:0" "int32:1"] []
    SYS_INPUT_GET "game.input.get" [] ["const_str:0"]
    SYS_PHYSICS_ADD_RIGIDBODY "game.physics.rigidbody.add" ["int2:0"] ["game.rigidbody:0"]
    SYS_PHYSICS_ADD_COLLIDER "game.physics.collider.add" ["game.rigidbody:0"] ["game.collider:0"]
    SYS_PATH_PUSH "game.path.push" ["int8:0"] []
    SYS_PATH_LEN "game.path.len" [] ["int32:0"]
    SYS_PATH_GET "game.path.get" ["int32:0"] ["int8:0"]
    SYS_PATH_CLEAR "game.path.clear" [] []
];

enum Stage<'a, 'b> {
    Step {
        state: &'a mut crate::State,
        gfx: &'a mut Gfx,
    },
    Draw {
        state: &'a mut crate::State,
        draw: &'a mut crate::gfx::Draw<'b>,
    },
}

impl PilScenario {
    pub fn new(script: &str, state: &mut crate::State, gfx: &mut Gfx) -> (Self, SoundsHandler) {
        let executable = {
            let text = crate::data::read_asset_string(script).unwrap();
            let mut col = script::Collection::default();
            col.add_standard().unwrap();
            col.add_ieee754().unwrap();
            for n in ["Vehicle", "Gltf", "Material", "MeshSet", "RigidBody", "Collider"] {
                let ty = format!("game_{n}");
                let reg = format!("game.{}:0", n.to_lowercase());
                col.add_opaque(&ty, 32).unwrap();
                col.add_register(&reg, &ty).unwrap();
            }
            for sys in SYS_TABLE {
                col.add_sys(sys.name, sys.id, sys.inputs, sys.outputs).unwrap();
            }
            col.parse_text(&text).unwrap();
            let mut program = script::Program::from_collection(&col, &"start".into()).unwrap();
            script::optimize::simple(&mut program);
            script::Executable::from_program(&program)
        };
        dbg!(&executable);
        let instance = executable.new_instance();

        let mut slf = Self {
            executable,
            instance,
            data_root: script.rsplit_once('/').map_or("", |s| s.0).into(),
            physics: Physics::new(),
            vehicles: Default::default(),
            collections: Default::default(),
            mesh_sets: Default::default(),
        };
        slf.run_script(Stage::Step { state, gfx });
        (slf, Box::new(|_| ()))
    }

    fn run_script(&mut self, mut stage: Stage<'_, '_>) {
        self.script_outputs(
            SYS_WAIT,
            &[match stage {
                Stage::Step { .. } => 0,
                Stage::Draw { .. } => 1,
            }],
        );
        
        let mut transform_buffer = Vec::new();
        let mut path = Vec::new();
        let stage = &mut stage;

        loop {
            match self.instance.run(&self.executable).unwrap() {
                script::Yield::Finish => todo!("exit"),
                script::Yield::Sys { id } => match id {
                    SYS_WAIT => break,
                    SYS_ARG => {
                        let p = core::str::from_utf8(&path);
                        let _ = dbg!(p);
                        path.clear();
                        path.extend_from_slice(b"user:__deathray_test.vvt");
                    }
                    SYS_GLTF_LOAD => {
                        let path = core::str::from_utf8(&path).unwrap();
                        let _ = dbg!(path);
                        let (root, path) = path.split_once(':').unwrap();
                        let path = match root {
                            "data" => self.data_root.to_string() + "/" + path,
                            _ => todo!("{root}"),
                        };
                        let collection = crate::data::read_asset_gltf(&path).unwrap();
                        let index = self.collections.insert(collection);
                        // TODO
                        self.script_outputs(id, &[index.as_u32()]);
                    }
                    SYS_ADD_MESH_SET => {
                        let Stage::Step { state, gfx } = stage else {
                            todo!()
                        };
                        let [collection] = self.script_inputs(id);
                        let collection = &self.collections[ArenaHandle::from_u32(collection)];
                        let set = gfx.add_mesh_set(collection);
                        let set = self.mesh_sets.insert(set);
                        self.script_outputs(id, &[set.as_u32()]);
                    }
                    SYS_GRAPHICS_DRAW_PUSH_TRANSFORM => {
                        let Stage::Draw { state, draw } = stage else {
                            todo!()
                        };
                        let [qw, qx, qy, qz, tx, ty, tz, s] =
                            self.script_inputs(id).map(f32::from_bits);
                        let q = Quat::from_xyzw(qx, qy, qz, qw);
                        let t = Vec3::new(tx, ty, tz);
                        let trf = TransformScale::new(t, q, s);
                        transform_buffer.push(trf);
                    }
                    SYS_GRAPHICS_DRAW_MESH => {
                        let Stage::Draw { state, draw } = stage else {
                            todo!()
                        };
                        let [set, mesh, material] = self.script_inputs(id);
                        let set = self.mesh_sets[ArenaHandle::from_u32(set)];
                        draw.collector
                            .solid
                            .push(set, mesh, material, &transform_buffer);
                        transform_buffer.clear();
                    }
                    SYS_VEHICLE_LOAD_V0 => {
                        let path = core::str::from_utf8(&path);
                        let _ = dbg!(path);
                        // TODO
                        self.script_outputs(id, &[0xdeadbeef]);
                    }
                    SYS_PATH_PUSH => {
                        let [c] = self.script_inputs(id);
                        path.push(c as u8);
                    }
                    SYS_PATH_GET => {
                        let [i] = self.script_inputs(id);
                        let c = path[i as usize];
                        self.script_outputs(id, &[u32::from(c)]);
                    }
                    SYS_PATH_LEN => {
                        let l = path.len() as u32;
                        self.script_outputs(id, &[l]);
                    }
                    SYS_PATH_CLEAR => {
                        path.clear();
                    }
                    id => todo!("{id}"),
                },
            }
        }
    }

    fn script_inputs<const N: usize>(&self, id: u32) -> [u32; N] {
        self.instance.inputs::<N>(&self.executable, id).unwrap()
    }

    fn script_outputs(&mut self, id: u32, values: &[u32]) {
        self.instance.outputs(&self.executable, id, values).unwrap()
    }
}

impl Scenario for PilScenario {
    fn step(&mut self, state: &mut crate::State, gfx: &mut Gfx) {
        self.run_script(Stage::Step { state, gfx })
    }

    fn draw(&mut self, state: &mut crate::State, draw: &mut crate::gfx::Draw<'_>) {
        self.run_script(Stage::Draw { state, draw })
    }

    fn camera(&self, aspect: f32) -> CameraView {
        CameraView {
            translation: Vec3::ZERO,
            rotation: Quat::IDENTITY,
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
