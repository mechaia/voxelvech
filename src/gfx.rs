use mechaia::{
    gui::{self, immediate::Layouter},
    math::{IVec2, U16Vec2, UVec2, Vec2, Vec3, Vec4},
    model::Collection,
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
    util::TransformScale,
    window::Window,
};

pub struct Gfx {
    pub render: Render,
    pub pbr_solid: Standard3D,
    pub pbr_transparent: Standard3D,
    pub stage_set: StageSetHandle,
    mesh_count: u32,
    pub gui: Gui,
}

pub struct Gui {
    pub gui: gui::Gui,
    pub data: GuiData,
}

pub struct GuiData {
    font: mechaia::gui::font::FontMap,
    pub show: Option<GuiShow>,
    pub file_picker: GuiFilePicker,
    pub menu: GuiMenu,
}

pub struct GuiMenu {
    entries: Vec<Box<str>>,
    index: usize,
}

pub struct GuiFilePicker {
    files: Vec<Box<str>>,
    pub input: String,
    index: usize,
}

pub enum GuiShow {
    Menu,
    LoadList,
    SaveList,
}

pub struct DrawCollector {
    pub solid: DrawCollectorOne,
    pub transparent: DrawCollectorOne,
}

pub struct DrawCollectorOne {
    instance_data: Vec<Vec<Instance>>,
    transforms_data: Vec<TransformScale>,
}

impl Gfx {
    pub fn new(window: &mut Window, collection: &Collection) -> Self {
        let mut render = {
            let (a, b) = window.render_handles();
            mechaia::render::Render::new(a, b)
        };

        let mut render_pass = RenderPass::builder(&mut render);

        let (pbr_solid, pbr_solid_stage) =
            make_pbr_stage(&mut render, &mut render_pass, collection, false);
        let (pbr_transparent, pbr_transparent_stage) =
            make_pbr_stage(&mut render, &mut render_pass, collection, true);

        let font = mechaia::gui::font::bdf::parse_from_str(mechaia::gui::font::bdf::SPLEEN_8X16);

        let gui = {
            let texture_set = TextureSet::builder(&mut render, TextureFormat::Rgba8Unorm, 2)
                .push(UVec2::new(1, 1), &mut |s| s.fill(u8::MAX))
                .push(font.dimensions(), &mut |s| font.copy_into_rgba(s))
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

        Self {
            render,
            pbr_solid,
            pbr_transparent,
            gui: Gui {
                gui,
                data: GuiData {
                    font,
                    show: None,
                    file_picker: GuiFilePicker {
                        files: Vec::new(),
                        index: 0,
                        input: String::new(),
                    },
                    menu: GuiMenu {
                        entries: ["continue", "reset vehicle", "save", "load", "exit"]
                            .into_iter()
                            .map(|s| s.to_string().into_boxed_str())
                            .collect(),
                        index: 0,
                    },
                },
            },
            stage_set,
            mesh_count: collection.meshes.len().try_into().unwrap(),
        }
    }

    pub fn draw(&mut self, camera: &CameraView, f: &mut dyn FnMut(&mut DrawCollector)) {
        let d = || DrawCollectorOne {
            instance_data: (0..self.mesh_count).map(|_| Vec::new()).collect(),
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

            self.gui.draw(index);
        });
    }
}

impl Gui {
    fn draw(&mut self, index: usize) {
        let layout = &mut Layouter::new(&self.gui);
        let draw = &mut self.gui.draw(index);
        self.data.draw_logbox(draw, layout);
        self.data.draw_crosshair(draw, layout);
        match &self.data.show {
            None => {}
            Some(GuiShow::Menu) => self.data.draw_menu(draw, layout),
            Some(GuiShow::LoadList) => self.data.draw_file_picker(draw, layout),
            Some(GuiShow::SaveList) => self.data.draw_file_picker(draw, layout),
        }
    }
}

impl GuiData {
    fn draw_logbox(&self, draw: &mut gui::Draw<'_>, layout: &mut Layouter) {
        use gui::immediate::Rect;
        let log_box = Rect {
            offset: U16Vec2::new(10, 10),
            size: U16Vec2::new(640, 160),
        };
        let mut start = IVec2::from(log_box.offset);
        use crate::log;
        log::iter_with(&mut |level, msg| {
            let [r, g, b] = match level {
                log::Level::Info => [1.0, 1.0, 1.0],
                log::Level::Error => [0.8, 0.0, 0.0],
                log::Level::Debug => [0.5, 0.5, 0.5],
                log::Level::Warning => [0.7, 0.7, 0.0],
                log::Level::Success => [0.0, 0.9, 0.0],
            };
            let color = Vec4::new(r, g, b, 1.0);
            gui::immediate::text::draw(draw, start, log_box, msg, 1, color, &self.font);
            start.y += i32::from(self.font.line_height());
        });
    }

    fn draw_crosshair(&self, draw: &mut gui::Draw<'_>, layout: &mut Layouter) {
        let r = layout.current();
        // 1x1 pixel is too small to see a damn thing, so use 2x2 and 3x3
        // also only consider x to keep it square
        let size = U16Vec2::splat(2) + (r.size.x % 2);
        let position = layout.current().center_rect_offset(size);
        draw.push(&mechaia::gui::Instance {
            position: position.try_into().unwrap_or(U16Vec2::ZERO),
            size,
            rotation: 0.0,
            uv_start: Vec2::ZERO,
            uv_end: Vec2::ZERO,
            texture: 0,
            color: Vec4::new(1.0, 1.0, 1.0, 1.0),
        });
    }

    fn draw_menu(&self, draw: &mut gui::Draw<'_>, layout: &mut Layouter) {
        use gui::immediate::{Margin, Value};
        let margin = Margin {
            left: Value::Mm(30.0),
            right: Value::Mm(30.0),
            top: Value::Mm(30.0),
            bottom: Value::Mm(30.0),
        };
        layout.push_margin(margin);

        let cur = layout.current();
        draw.push(&mechaia::gui::Instance {
            position: cur.offset,
            size: cur.size,
            rotation: 0.0,
            uv_start: Vec2::ZERO,
            uv_end: Vec2::ZERO,
            texture: 0,
            color: Vec4::new(0.0, 0.0, 0.0, 0.4),
        });

        let v = Value::Pixel(10);
        let margin = Margin {
            left: v,
            right: v,
            top: v,
            bottom: v,
        };
        layout.push_margin(margin);

        let cur = layout.current();
        let mut start = IVec2::from(cur.offset);

        let mut push_text = |t: String| {
            gui::immediate::text::draw(draw, start, cur, &t, 1, Vec4::ONE, &self.font);
            start.y += i32::from(self.font.line_height());
        };

        push_text(format!("   MENU"));
        push_text(format!(""));
        for (i, entry) in self.menu.entries.iter().enumerate() {
            let s = [' ', '>'][usize::from(i == self.menu.index)];
            push_text(format!("{s} {entry}"));
        }

        layout.pop();
        layout.pop();
    }

    fn draw_file_picker(&self, draw: &mut gui::Draw<'_>, layout: &mut Layouter) {
        use gui::immediate::{Margin, Value};
        let margin = Margin {
            left: Value::Mm(30.0),
            right: Value::Mm(30.0),
            top: Value::Mm(30.0),
            bottom: Value::Mm(30.0),
        };
        layout.push_margin(margin);

        let cur = layout.current();
        draw.push(&mechaia::gui::Instance {
            position: cur.offset,
            size: cur.size,
            rotation: 0.0,
            uv_start: Vec2::ZERO,
            uv_end: Vec2::ZERO,
            texture: 0,
            color: Vec4::new(0.0, 0.0, 0.0, 0.4),
        });

        let v = Value::Pixel(10);
        let margin = Margin {
            left: v,
            right: v,
            top: v,
            bottom: v,
        };
        layout.push_margin(margin);

        let cur = layout.current();
        let mut start = IVec2::from(cur.offset);

        draw.push(&gui::Instance {
            position: start.try_into().unwrap(),
            size: cur.size.with_y(self.font.line_height()),
            rotation: 0.0,
            uv_start: Vec2::ZERO,
            uv_end: Vec2::ZERO,
            texture: 0,
            color: Vec4::new(0.0, 0.0, 0.0, 0.4),
        });

        let mut push_text = |t: String| {
            gui::immediate::text::draw(draw, start, cur, &t, 1, Vec4::ONE, &self.font);
            start.y += i32::from(self.font.line_height());
        };

        push_text(format!("   {}", self.file_picker.input));
        push_text(format!(""));

        for (i, p) in self.file_picker.files.iter().enumerate() {
            let f = |b| [' ', '>'][usize::from(b)];
            let l = f(**p == *self.file_picker.input);
            let ll = f(i == self.file_picker.index);
            push_text(format!("{ll}{l} {p}"));
        }

        layout.pop();
        layout.pop();
    }
}

impl GuiMenu {
    pub fn current(&self) -> &str {
        &self.entries[self.index]
    }

    pub fn reset_index(&mut self) {
        self.index = 0;
    }

    pub fn next_index(&mut self) {
        self.index = (self.index + 1).rem_euclid(self.entries.len());
    }

    pub fn prev_index(&mut self) {
        self.index = self.index.checked_sub(1).unwrap_or(self.entries.len() - 1);
    }
}

impl GuiFilePicker {
    pub fn set_files(&mut self, files: Vec<Box<str>>) {
        self.files = files;
        self.index = self
            .files
            .iter()
            .position(|p| **p == *self.input)
            .unwrap_or(0);
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn input_push(&mut self, c: char) {
        self.input.push(c);
    }

    pub fn input_extend<I: IntoIterator<Item = char>>(&mut self, it: I) {
        self.input.extend(it);
    }

    pub fn input_pop(&mut self) {
        self.input.pop();
    }

    pub fn input_clear(&mut self) {
        self.input.clear();
    }

    pub fn input_set_to_index(&mut self) {
        if !self.files.is_empty() {
            self.input = self.files[self.index].to_string();
        }
    }

    /// Look for next entry that best matches the current input
    pub fn input_search_or_complete(&mut self) {
        let mut cur = self.index;
        let mut next = None;
        loop {
            if self.files[cur].starts_with(&self.input) {
                let (_, s) = next.get_or_insert((cur, &*self.files[cur]));
                *s = common_prefix(s, &*self.files[cur]);
            }
            cur = (cur + 1).rem_euclid(self.files.len().max(1));
            if cur == self.index {
                break;
            }
        }
        if let Some((i, prefix)) = next {
            self.index = i;
            self.input = prefix.into();
        }
    }

    pub fn selected(&self) -> Option<&str> {
        self.files.get(self.index).map(|s| &**s)
    }

    pub fn next_index(&mut self) {
        self.index = (self.index + 1).rem_euclid(self.files.len().max(1));
    }

    pub fn prev_index(&mut self) {
        self.index = self
            .index
            .checked_sub(1)
            .unwrap_or(self.files.len().max(1) - 1);
    }
}

impl DrawCollectorOne {
    pub fn push(&mut self, mesh_id: u32, material_id: u32, transforms: &[TransformScale]) {
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
    collection: &Collection,
    transparent: bool,
) -> (Standard3D, ComputeStage) {
    let texture_set = TextureSet::builder(render, TextureFormat::Rgba8Unorm, 1)
        .push(UVec2::new(1, 1), &mut |s| s.fill(u8::MAX))
        .build();
    let tex_white = 0;
    let material_set = PbrMaterialSet::builder(render, 3)
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
        .push(&PbrMaterial {
            albedo: Vec4::new(1.0, 1.0, 1.0, 1.0),
            roughness: 0.8,
            metallic: 0.3,
            ambient_occlusion: 1.0,
            albedo_texture: tex_white,
            roughness_texture: tex_white,
            metallic_texture: tex_white,
            ambient_occlusion_texture: tex_white,
        })
        .build();
    let mesh_set = MeshSet::new(
        render,
        &collection
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

fn common_prefix<'a>(a: &'a str, b: &'a str) -> &'a str {
    let (a, b) = if a.len() < b.len() { (b, a) } else { (a, b) };
    for (i, (x, y)) in a.bytes().zip(b.bytes()).enumerate() {
        if x != y {
            return &a[..i];
        }
    }
    &a[..b.len()]
}
