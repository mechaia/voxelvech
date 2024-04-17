use mechaia::{
    gui::Gui,
    math::{UVec2, Vec2, Vec3, Vec4},
    render::{
        resource::{
            camera::{Camera, CameraView},
            material::pbr::{PbrMaterial, PbrMaterialSet},
            mesh::{Mesh, MeshSet},
            texture::{TextureFormat, TextureSet},
        },
        stage::{
            renderpass::{RenderPass, RenderPassBuilder},
            standard3d::{ComputeStage, Instance, Standard3D},
        },
        Render, StageSetHandle,
    },
    util::Transform,
    window::Window,
};

pub struct Gfx {
    pub render: Render,
    pub pbr_solid: Standard3D,
    pub pbr_transparent: Standard3D,
    pub gui: Gui,
    pub stage_set: StageSetHandle,
    block_count: u32,
}

pub struct DrawCollector {
    pub solid: DrawCollectorOne,
    pub transparent: DrawCollectorOne,
}

pub struct DrawCollectorOne {
    instance_data: Vec<Vec<Instance>>,
    transforms_data: Vec<Transform>,
}

impl Gfx {
    pub fn new(window: &mut Window) -> Self {
        let mut render = {
            let (a, b) = window.render_handles();
            mechaia::render::Render::new(a, b)
        };

        let mut render_pass = RenderPass::builder(&mut render);

        let (pbr_solid, pbr_solid_stage) = make_pbr_stage(&mut render, &mut render_pass, false);
        let (pbr_transparent, pbr_transparent_stage) =
            make_pbr_stage(&mut render, &mut render_pass, true);

        let gui = {
            let texture_set = TextureSet::builder(&mut render, TextureFormat::Rgba8Unorm, 1)
                .push(UVec2::new(1, 1), &mut |s| s.fill(u8::MAX))
                .build();
            mechaia::gui::push(&mut render, &mut render_pass, 1024, texture_set)
        };

        let render_pass = render_pass.build(&mut render);

        let stage_set = render.add_stage_set(
            [
                Box::new(pbr_solid_stage) as Box<dyn mechaia::render::stage::Stage>,
                Box::new(pbr_transparent_stage),
                Box::new(render_pass),
            ]
            .into(),
        );

        let tex_pbr_white = 0;

        let mat_plain_white = 0;
        let mat_plain_green = 1;

        let block_count = crate::load_blocks().meshes.len().try_into().unwrap();

        Self {
            render,
            pbr_solid,
            pbr_transparent,
            gui,
            stage_set,
            block_count,
        }
    }

    pub fn draw(&mut self, camera: &CameraView, f: &mut dyn FnMut(&mut DrawCollector)) {
        let d = || DrawCollectorOne {
            instance_data: (0..self.block_count).map(|_| Vec::new()).collect(),
            transforms_data: Vec::new(),
        };
        let mut collector = DrawCollector {
            solid: d(),
            transparent: d(),
        };

        f(&mut collector);

        self.render.draw(self.stage_set, &mut |index| {
            for (pbr, coll) in [
                (&mut self.pbr_solid, &collector.solid),
                (&mut self.pbr_transparent, &collector.transparent),
            ] {
                pbr.set_transform_data(index, &mut coll.transforms_data.iter().copied());
                pbr.set_instance_data(
                    index,
                    &coll
                        .instance_data
                        .iter()
                        .map(|v| u32::try_from(v.len()).unwrap())
                        .collect::<Vec<_>>(),
                    &mut coll.instance_data.iter().flatten().copied(),
                );
                pbr.set_directional_light(index, Vec3::NEG_ONE.normalize(), Vec3::ONE * 5.0);
                pbr.set_camera(index, camera);
            }

            /*
            self.gui.draw(
                index,
                &mut [mechaia::gui::Instance {
                    position: Vec2::ZERO,
                    half_extents: Vec2::ONE / 64.0,
                    rotation: 0.0,
                    uv_start: Vec2::ZERO,
                    uv_end: Vec2::ZERO,
                    texture: 0,
                }]
                .into_iter(),
            )
                */
        });
    }
}

impl DrawCollectorOne {
    pub fn push(&mut self, mesh_id: u32, material_id: u32, transforms: &[Transform]) {
        self.instance_data[usize::try_from(mesh_id).unwrap()].push(Instance {
            transforms_offset: self.transforms_data.len().try_into().unwrap(),
            material: material_id,
        });
        self.transforms_data.extend_from_slice(transforms);
    }
}

fn make_pbr_stage(
    render: &mut Render,
    render_pass: &mut RenderPassBuilder,
    transparent: bool,
) -> (Standard3D, ComputeStage) {
    let texture_set = TextureSet::builder(render, TextureFormat::Rgba8Unorm, 1)
        .push(UVec2::new(1, 1), &mut |s| s.fill(u8::MAX))
        .build();
    let tex_white = 0;
    let material_set = PbrMaterialSet::builder(render, 2)
        .push(&PbrMaterial {
            albedo: Vec4::ONE,
            roughness: 0.5,
            metallic: 0.5,
            ambient_occlusion: 1.0,
            albedo_texture: tex_white,
            roughness_texture: tex_white,
            metallic_texture: tex_white,
            ambient_occlusion_texture: tex_white,
        })
        .push(&PbrMaterial {
            albedo: Vec4::new(0.0, 1.0, 0.0, 0.5),
            roughness: 0.2,
            metallic: 0.8,
            ambient_occlusion: 1.0,
            albedo_texture: tex_white,
            roughness_texture: tex_white,
            metallic_texture: tex_white,
            ambient_occlusion_texture: tex_white,
        })
        .build();
    let models = crate::load_blocks();
    let mesh_set = MeshSet::new(
        render,
        &models
            .meshes
            .iter()
            .map(|m| Mesh {
                indices: &m.indices,
                vertices: m.vertices.as_slice(),
            })
            .collect::<Vec<_>>(),
    );
    let camera = Camera::new(render);
    Standard3D::new(
        render,
        render_pass,
        texture_set,
        material_set,
        mesh_set,
        camera,
        transparent,
        1 << 20,
        1 << 20,
    )
}
