mod item_picker;

pub use item_picker::ItemPicker;

use {
    crate::{gfx::Draw, log},
    mechaia::{
        gui::immediate::{text::Text, Rect},
        math::{IVec2, U16Vec2, Vec2, Vec4},
    },
};

pub fn draw_logbox(draw: &mut Draw<'_>) {
    let rect = Rect {
        offset: IVec2::new(10, 10),
        size: U16Vec2::new(640, 160),
    };
    let mut text = Text::new(&mut draw.gui, 1, draw.font, rect.offset, rect);
    crate::log::iter_with(&mut |level, msg| {
        let [r, g, b] = match level {
            log::Level::Info => [1.0, 1.0, 1.0],
            log::Level::Error => [0.8, 0.0, 0.0],
            log::Level::Debug => [0.5, 0.5, 0.5],
            log::Level::Warning => [0.7, 0.7, 0.0],
            log::Level::Success => [0.0, 0.9, 0.0],
        };
        text.set_color(Vec4::new(r, g, b, 1.0));
        text.extend(msg.chars());
        text.push('\n');
    });
}

pub fn draw_crosshair(draw: &mut Draw<'_>) {
    let rect = Rect::from_offset_size(IVec2::ZERO, draw.gui.viewport());
    // 1x1 pixel is too small to see a damn thing, so use 2x2 and 3x3
    // also only consider x to keep it square
    let size = U16Vec2::splat(2) + (rect.size.x % 2);
    let position = rect.center_rect_offset(size);
    draw.gui.push(&mechaia::gui::Instance {
        position: position.try_into().unwrap_or(U16Vec2::ZERO),
        size,
        rotation: 0.0,
        uv_start: Vec2::ZERO,
        uv_end: Vec2::ZERO,
        texture: 0,
        color: Vec4::new(1.0, 1.0, 1.0, 1.0),
    });
}
