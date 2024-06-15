use mechaia::{
    gui::{self, font::FontMap, immediate::Rect},
    math::{IVec2, UVec2, Vec3, Vec4},
    model::Collection,
    render::{
        resource::{
            camera::{Camera, CameraView},
            material::pbr::{PbrFlags, PbrMaterial, PbrMaterialSet},
            mesh::{Mesh, MeshSet},
            texture::{Texture, TextureFormat, TextureView},
            Shared,
        },
        stage::{
            renderpass::{RenderPass, RenderPassBuilder},
            standard3d::{self, ComputeStage, Instance, Standard3D},
        },
        Render, StageSetHandle,
    },
    util::{Arena, ArenaHandle, TransformScale},
    window::Window,
};

pub struct Gfx {
    pub render: Render,
    pub pbr_solid: Standard3D,
    pub pbr_transparent: Standard3D,
    pub stage_set: StageSetHandle,
    gui: gui::Gui,
    font: FontMap,
    mesh_sets: Arena<MeshSetInfo>,
    textures: Vec<Shared<Texture>>,
    texture_views: Vec<Shared<TextureView>>,
}

struct MeshSetInfo {
    set_len: usize,
    solid_handle: standard3d::MeshSetHandle,
    transparent_handle: standard3d::MeshSetHandle,
}

pub struct DrawCollector {
    pub solid: DrawCollectorOne,
    pub transparent: DrawCollectorOne,
}

pub struct DrawCollectorOne {
    // Triple jagged array!
    // The horror!
    //
    // per mesh set -> per mesh -> per instance
    instance_data: Vec<(MeshSetHandle, Vec<Vec<Instance>>)>,
    transforms_data: Vec<TransformScale>,
}

pub struct Draw<'a> {
    pub collector: DrawCollector,
    pub gui: gui::Draw<'a>,
    pub font: &'a FontMap,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct MeshSetHandle(ArenaHandle);

impl Gfx {
    pub fn new(window: &mut Window) -> Self {
        let mut render = {
            let (a, b) = window.render_handles();
            mechaia::render::Render::new(a, b)
        };

        let mut render_pass = RenderPass::builder();

        let tex_white = 0;
        let mut material_set = PbrMaterialSet::new(&mut render, 32);
        material_set.push(
            &mut render,
            &PbrMaterial {
                albedo: Vec4::ONE,
                roughness: 0.5,
                metallic: 0.5,
                ambient_occlusion: 1.0,
                albedo_texture: tex_white,
                roughness_texture: tex_white,
                metallic_texture: tex_white,
                ambient_occlusion_texture: tex_white,
                flags: Default::default(),
            },
        );
        material_set.push(
            &mut render,
            &PbrMaterial {
                albedo: Vec4::new(0.0, 1.0, 0.0, 0.5),
                roughness: 0.2,
                metallic: 0.8,
                ambient_occlusion: 1.0,
                albedo_texture: tex_white,
                roughness_texture: tex_white,
                metallic_texture: tex_white,
                ambient_occlusion_texture: tex_white,
                flags: Default::default(),
            },
        );
        material_set.push(
            &mut render,
            &PbrMaterial {
                albedo: Vec4::new(1.0, 1.0, 1.0, 1.0),
                roughness: 0.8,
                metallic: 0.3,
                ambient_occlusion: 1.0,
                albedo_texture: tex_white,
                roughness_texture: tex_white,
                metallic_texture: tex_white,
                ambient_occlusion_texture: tex_white,
                flags: Default::default(),
            },
        );

        material_set.push(
            &mut render,
            &PbrMaterial {
                albedo: Vec4::new(0.3, 0.4, 1.0, 1.0),
                albedo_texture: tex_white,
                flags: *PbrFlags::default().set_unlighted(true),
                ..Default::default()
            },
        );

        let camera = Camera::new(&mut render);

        let material_set = Shared::new(material_set);
        let camera = Shared::new(camera);

        let cfg = |transparent| standard3d::Configuration {
            max_transform_count: 1 << 20,
            max_instance_count: 1 << 20,
            max_texture_count: 1 << 12,
            max_material_count: 1 << 12,
            transparent,
        };

        let (mut pbr_solid, pbr_solid_stage) = make_pbr_stage(
            &mut render,
            &mut render_pass,
            camera.clone(),
            material_set.clone(),
            &cfg(false),
        );
        let (mut pbr_transparent, pbr_transparent_stage) = make_pbr_stage(
            &mut render,
            &mut render_pass,
            camera,
            material_set,
            &cfg(true),
        );

        let font = mechaia::gui::font::bdf::parse_from_str(mechaia::gui::font::bdf::SPLEEN_8X16);

        let mut textures = Vec::new();
        textures.push(Shared::new(Texture::new(
            &mut render,
            UVec2::ONE,
            TextureFormat::Rgba8Unorm,
            &mut |s| s.fill(u8::MAX),
        )));
        textures.push(Shared::new(Texture::new(
            &mut render,
            font.dimensions(),
            TextureFormat::Rgba8Unorm,
            &mut |s| font.copy_into_rgba8(s),
        )));

        let mut texture_views = Vec::new();
        for tex in textures.iter() {
            texture_views.push(Shared::new(TextureView::new(&mut render, tex.clone())));
        }

        let mut gui = {
            let config = mechaia::gui::Configuration {
                max_instances: 1 << 10,
                max_textures: 1 << 5,
            };
            mechaia::gui::push(&mut render, &mut render_pass, &config)
        };

        for tex in texture_views.iter() {
            pbr_solid.add_texture(&mut render, tex.clone());
            pbr_transparent.add_texture(&mut render, tex.clone());
            gui.add_texture(&mut render, tex.clone());
        }

        let render_pass = render_pass.build(&mut render);

        let stage_set = render.add_stage_set(
            [
                Box::new(pbr_solid_stage) as Box<dyn mechaia::render::stage::Stage>,
                Box::new(pbr_transparent_stage),
                Box::new(render_pass),
            ]
            .into(),
        );

        Self {
            render,
            pbr_solid,
            pbr_transparent,
            stage_set,
            gui,
            font,
            mesh_sets: Default::default(),
            textures: Default::default(),
            texture_views: Default::default(),
        }
    }

    pub fn add_mesh_set(&mut self, collection: &Collection) -> MeshSetHandle {
        let set = MeshSet::new(
            &mut self.render,
            &collection
                .meshes
                .iter()
                .map(|m| Mesh {
                    indices: &m.indices,
                    vertices: m.vertices.as_slice(),
                })
                .collect::<Vec<_>>(),
        );
        let set = Shared::new(set);
        let set_len = set.len();
        let solid_handle = self.pbr_solid.add_mesh_set(&mut self.render, set.clone());
        let transparent_handle = self.pbr_transparent.add_mesh_set(&mut self.render, set);
        self.render.rerecord_commands(self.stage_set);
        let h = self.mesh_sets.insert(MeshSetInfo {
            set_len,
            solid_handle,
            transparent_handle,
        });
        MeshSetHandle(h)
    }

    pub fn remove_mesh_set(&mut self, mesh_set: MeshSetHandle) {
        let info = self
            .mesh_sets
            .remove(mesh_set.0)
            .expect("no mesh set with handle");
        self.pbr_solid
            .remove_mesh_set(&mut self.render, info.solid_handle);
        self.pbr_transparent
            .remove_mesh_set(&mut self.render, info.transparent_handle);
        self.render.rerecord_commands(self.stage_set);
    }

    pub fn draw(&mut self, camera: &CameraView, f: &mut dyn FnMut(&mut Draw<'_>)) {
        self.render.draw(self.stage_set, &mut |index| {
            let d = || DrawCollectorOne {
                instance_data: self
                    .mesh_sets
                    .iter()
                    .map(|(k, v)| {
                        (
                            MeshSetHandle(k),
                            (0..v.set_len).map(|_| Vec::new()).collect(),
                        )
                    })
                    .collect(),
                transforms_data: Vec::new(),
            };

            let mut draw = Draw {
                collector: DrawCollector {
                    solid: d(),
                    transparent: d(),
                },
                gui: self.gui.draw(index),
                font: &self.font,
            };

            f(&mut draw);

            for (pbr, coll) in [
                (&mut self.pbr_solid, &draw.collector.solid),
                (&mut self.pbr_transparent, &draw.collector.transparent),
            ] {
                pbr.set_transform_data(index, &mut coll.transforms_data.iter().copied());
                pbr.set_instance_data(
                    index,
                    &coll
                        .instance_data
                        .iter()
                        .flat_map(|(_, v)| v)
                        .map(|v| u32::try_from(v.len()).unwrap())
                        .collect::<Vec<_>>(),
                    &mut coll
                        .instance_data
                        .iter()
                        .flat_map(|(_, v)| v)
                        .flatten()
                        .copied(),
                );
                pbr.set_directional_light(index, Vec3::NEG_ONE.normalize(), Vec3::ONE * 5.0);
                pbr.set_camera(index, camera);
            }
        });
    }

    pub fn rebuild(&mut self) {
        self.render.rebuild_swapchain();
    }
}

impl DrawCollectorOne {
    pub fn push(
        &mut self,
        mesh_set: MeshSetHandle,
        mesh_id: u32,
        material_id: u32,
        transforms: &[TransformScale],
    ) {
        let (_, v) = self
            .instance_data
            .iter_mut()
            .find(|(k, _)| *k == mesh_set)
            .expect("no mesh set with handle");
        v[usize::try_from(mesh_id).unwrap()].push(Instance {
            transforms_offset: self.transforms_data.len().try_into().unwrap(),
            material: material_id,
        });
        self.transforms_data.extend_from_slice(transforms);
    }
}

impl Draw<'_> {
    pub fn viewport_rect(&self) -> Rect {
        Rect::from_offset_size(IVec2::ZERO, self.gui.viewport())
    }
}

fn make_pbr_stage(
    render: &mut Render,
    render_pass: &mut RenderPassBuilder,
    camera: Shared<Camera>,
    material_set: Shared<PbrMaterialSet>,
    config: &standard3d::Configuration,
) -> (Standard3D, ComputeStage) {
    Standard3D::new(render, render_pass, camera, material_set, config)
}
