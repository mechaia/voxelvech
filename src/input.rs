use std::collections::HashMap;

type Key = mechaia::window::InputKey;

/// Mapping of inputs
#[derive(Default)]
pub struct InputMap {
    label_to_input: HashMap<Box<str>, Vec<(Key, f32)>>,
}

impl InputMap {
    pub fn from_list(list: &[(&str, Key, f32)]) -> Self {
        let mut slf = Self::default();
        for &(lbl, input, scale) in list {
            slf.add(lbl.to_string().into(), input, scale);
        }
        slf
    }

    pub fn add(&mut self, label: Box<str>, input: Key, scale: f32) {
        self.label_to_input
            .entry(label)
            .or_default()
            .push((input, scale));
    }

    pub fn get(&self, label: &str) -> &[(Key, f32)] {
        self.label_to_input.get(label).map_or(&[], |v| &**v)
    }
}

/// Filter to make boolean input edge-triggered.
#[derive(Default)]
pub struct TriggerEdge {
    prev: bool,
}

impl TriggerEdge {
    pub fn apply(&mut self, input: f32) -> bool {
        let p = self.prev;
        self.prev = input > 0.0;
        !p & self.prev
    }
}

/// Filter to make boolean input "toggled" on edge trigger
// FIXME what was the damn term? "hold"?
#[derive(Default)]
pub struct TriggerToggle {
    edge: TriggerEdge,
    value: bool,
}

impl TriggerToggle {
    pub fn apply(&mut self, input: f32) -> bool {
        self.value ^= self.edge.apply(input);
        self.value
    }
}
