use mechaia::{
    gui::Gui,
    math::{UVec2, Vec4},
    render::{
        resource::{
            material::pbr::{PbrMaterial, PbrMaterialSet},
            texture::{TextureFormat, TextureSet},
        },
        stage::{
            renderpass::{RenderPass, RenderPassBuilder},
            standard3d::{ComputeStage, Standard3D},
        },
        Render, StageSetHandle,
    },
    window::Window,
};

pub struct Gfx {
    pub render: Render,
    pub pbr_solid: Standard3D,
    pub pbr_transparent: Standard3D,
    pub gui: Gui,
    pub stage_set: StageSetHandle,
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

        Self {
            render,
            pbr_solid,
            pbr_transparent,
            gui,
            stage_set,
        }
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
    let meshes = crate::load_blocks();
    Standard3D::new(
        render,
        render_pass,
        texture_set,
        material_set,
        &meshes,
        transparent,
    )
}
