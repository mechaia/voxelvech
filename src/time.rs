use std::time::Instant;

pub struct StopWatch {
    delta: f32,
    last: Instant,
}

impl StopWatch {
    pub fn new() -> Self {
        Self {
            delta: 0.0,
            last: Instant::now(),
        }
    }

    pub fn sample(&mut self) {
        let t = Instant::now();
        self.delta = t.duration_since(self.last).as_secs_f32();
        self.last = t;
    }

    pub fn delta(&self) -> f32 {
        self.delta
    }
}
