use mechaia::{
    math::{FloatExt, IVec3, Vec3},
    sound::mix::wave,
    util::{
        math::fixed::{U0d32, U32d32},
        Transform,
    },
};
use std::{collections::HashMap, num::Wrapping, ops::RangeInclusive, time::Instant};

#[derive(Default)]
pub struct VehicleSound {
    pub wheels: HashMap<IVec3, WheelSound>,
}

#[derive(Default)]
pub struct WheelSound {
    pub prev_volume: f32,
    pub last_volume: f32,
    pub volume: f32,
    pub speed: U32d32,
    pub t: Wrapping<U0d32>,
}

impl VehicleSound {
    pub fn update(&mut self, transform: &Transform) {
        for (&pos, w) in self.wheels.iter_mut() {
            let pos = transform.apply_to_translation(pos.as_vec3());
            let scale = (10.0 / pos.length_squared()).min(0.5);
            let scale = scale * w.speed.to_f32().min(1.0);
            //let scale = 1.0 / pos.length().max(1.0);

            w.prev_volume = w.last_volume;
            w.volume = scale;
        }
    }

    pub fn next_samples(&mut self, interpolate: RangeInclusive<f32>, dt: U0d32, buf: &mut [f32]) {
        for (&pos, wheel) in self.wheels.iter_mut() {
            wheel.next_samples(interpolate.clone(), dt, buf);
        }
    }
}

impl WheelSound {
    pub fn next_samples(&mut self, interpolate: RangeInclusive<f32>, dt: U0d32, buf: &mut [f32]) {
        let mut f = *interpolate.start();
        let df = (interpolate.end() - interpolate.start()) / buf.len() as f32;
        for (i, w) in buf.iter_mut().enumerate() {
            self.last_volume = self.prev_volume.lerp(self.volume, f);
            //*w += self.volume * wave::sine(self.t.0);
            *w += self.last_volume * wave::sine(self.t.0);
            f += df;
            self.t += (U32d32::from(dt) * self.speed * 10).frac();
            //self.t += (U32d32::from(dt) * 100 * self.speed).frac();
            //self.t += (U32d32::from(dt) * 100).frac();
        }
    }
}
