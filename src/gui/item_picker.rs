use {
    crate::gfx::Draw,
    mechaia::{
        gui::{
            self,
            immediate::{container, text, Margins, Rect, Value},
        },
        input::{Key, KeyOrName},
        math::{IVec2, U16Vec2, Vec2, Vec4},
    },
};

#[derive(Default)]
pub struct ItemPicker {
    pub items: Vec<Box<str>>,
    pub selected: usize,
    pub filter: String,
}

impl ItemPicker {
    pub fn handle_input<'a>(&'a mut self, state: &mut crate::State) -> Option<&'a str> {
        let mut select = None;

        for evt in state.input.state.events() {
            if !evt.is_edge_or_hold_over(0.5) {
                continue;
            }
            match &evt.key {
                KeyOrName::Key(key) => match key {
                    Key::Unicode(c) if c.as_str() == "\t" => {
                        let mut x = &*self.filter;
                        for y in self.items.iter() {
                            x = common_prefix(x, y);
                        }
                        if !self.filter.starts_with(x) {
                            self.filter = x.to_string();
                        }
                    }
                    Key::Unicode(c) if c.as_str() == "\n" => {}
                    Key::Unicode(c) => {
                        self.filter.extend(c.chars());
                        self.advance(|s| s.next_item());
                    }
                    Key::Backspace => {
                        self.filter.pop();
                    }
                    _ => {}
                },
                KeyOrName::Name(name) => match &**name {
                    "ui.up" => {
                        self.prev_item();
                        self.advance(|s| s.prev_item());
                    }
                    "ui.down" => {
                        self.next_item();
                        self.advance(|s| s.next_item());
                    }
                    "ui.select" => select = Some(self.selected),
                    _ => {}
                },
            }
        }

        select.and_then(|i| self.items.get(i)).map(|s| &**s)
    }

    pub fn next_item(&mut self) {
        self.selected = self.selected.saturating_add(1);
        if self.selected >= self.items.len() {
            self.selected = 0;
        }
    }

    pub fn prev_item(&mut self) {
        self.selected = self
            .selected
            .checked_sub(1)
            .unwrap_or(self.items.len().saturating_sub(1));
    }

    fn advance(&mut self, f: impl Fn(&mut Self)) {
        let i = self.selected;
        loop {
            if !self.is_filtered(self.selected) {
                break;
            }
            f(self);
            if i == self.selected {
                break;
            }
        }
    }

    fn is_filtered(&self, index: usize) -> bool {
        !self
            .items
            .get(index)
            .is_some_and(|w| w.starts_with(&self.filter))
    }

    pub fn render(&self, rect: Rect, draw: &mut Draw<'_>) {
        let rect = container::with_margins(rect, Margins::splat(Value::Pixel(30))).unwrap_or(rect);
        draw.gui.push(&mechaia::gui::Instance {
            position: rect.offset.try_into().unwrap_or(U16Vec2::ZERO),
            size: rect.size,
            rotation: 0.0,
            uv_start: Vec2::ZERO,
            uv_end: Vec2::ZERO,
            texture: 0,
            color: Vec4::new(0.0, 0.0, 0.0, 0.4),
        });

        let rect = container::with_margins(rect, Margins::splat(Value::Pixel(10))).unwrap_or(rect);
        draw.gui.push(&gui::Instance {
            position: rect.offset.try_into().unwrap_or(U16Vec2::ZERO),
            size: rect.size.with_y(draw.font.line_height()),
            rotation: 0.0,
            uv_start: Vec2::ZERO,
            uv_end: Vec2::ZERO,
            texture: 0,
            color: Vec4::new(0.0, 0.0, 0.0, 0.4),
        });

        let mut text = text::Text::new(&mut draw.gui, 1, &draw.font, rect.offset, rect);

        text.push(' ');
        text.push(' ');
        if self.filter.is_empty() {
            text.set_color(Vec4::ONE.with_w(0.5));
            text.extend("Filter...".chars());
            text.set_color(Vec4::ONE);
        } else {
            text.extend(self.filter.chars());
        }
        text.push('\n');
        text.push('\n');

        for (i, p) in self
            .items
            .iter()
            .enumerate()
            .filter(|p| !self.is_filtered(p.0))
        {
            text.push([' ', '>'][usize::from(i == self.selected)]);
            text.push(' ');
            text.extend(p.chars());
            text.push('\n');
        }
    }
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
