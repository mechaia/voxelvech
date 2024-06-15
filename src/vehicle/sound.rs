use mechaia::{
    math::{FloatExt, IVec3, Vec3},
    sound::mix::wave,
    util::{
        math::fixed::{U0d32, U32d32},
        rand::Rng,
        soa::Vec2,
        Transform,
    },
};
use std::{collections::HashMap, num::Wrapping, ops::RangeInclusive, time::Instant};

use crate::{DualReceptor, PanFactor};

#[derive(Default)]
pub struct VehicleSound {
    pub wheels: HashMap<IVec3, WheelSound>,
}

#[derive(Default)]
pub struct GlobalSound {
    pub lasers: Vec2<Vec3, LaserSound>,
    ping: Vec2<Vec3, PingSound>,
}

#[derive(Default)]
pub struct Volume {
    prev: f32,
    last: f32,
    cur: f32,
}

#[derive(Default)]
pub struct WheelSound {
    pub volume: Volume,
    pub speed: U32d32,
    pub t: Wrapping<U0d32>,
    pub pan: PanFactor,
}

pub struct LaserSound {
    pub volume: Volume,
    pub pitch_decay: f32,
    pub dt_factor: U32d32,
    pub t: Wrapping<U0d32>,
    pub pan: PanFactor,
}

struct PingSound {
    volume: Volume,
    t: Wrapping<U0d32>,
    hold: U0d32,
    frequency: u32,
    amplitude: f32,
    pan: PanFactor,
}

impl VehicleSound {
    pub fn update(&mut self, transform: &Transform) {
        for (pos, w) in self.wheels.iter_mut() {
            let pos = transform.apply_to_translation(pos.as_vec3());
            let scale = (10.0 / pos.length_squared()).min(0.5);
            let scale = scale * w.speed.to_f32().min(1.0);
            //let scale = 1.0 / pos.length().max(1.0);

            w.volume.prev = w.volume.last;
            w.volume.cur = scale;

            w.pan = PanFactor::from_dir(pos);
        }
    }

    pub fn next_samples(&mut self, receptor: &mut DualReceptor<'_>) {
        for (_, wheel) in self.wheels.iter_mut() {
            wheel.next_samples(receptor);
        }
    }
}

impl GlobalSound {
    pub fn update(&mut self, transform: &Transform) {
        for (pos, laser) in self.lasers.iter_mut() {
            let pos = transform.apply_to_translation(*pos);
            let scale = (10.0 / pos.length_squared()).min(0.2);

            laser.volume.prev = laser.volume.last;
            laser.volume.cur = scale;

            laser.pan = PanFactor::from_dir(pos);
        }

        for (pos, ping) in self.ping.iter_mut() {
            let pos = transform.apply_to_translation(*pos);
            let scale = (10.0 / pos.length_squared());

            ping.volume.prev = ping.volume.last;
            ping.volume.cur = scale;

            ping.pan = PanFactor::from_dir(pos);
        }
    }

    pub fn next_samples(&mut self, receptor: &mut DualReceptor<'_>) {
        for i in (0..self.lasers.len()).rev() {
            let (_, laser) = self.lasers.get_mut(i).unwrap();
            laser.next_samples(receptor);
            if laser.dt_factor <= U32d32::ONE / 16 {
                self.lasers.swap_remove(i);
            }
        }

        for i in (0..self.ping.len()).rev() {
            let (_, ping) = self.ping.get_mut(i).unwrap();
            ping.next_samples(receptor);
            if ping.finish() {
                self.ping.swap_remove(i);
            }
        }
    }

    pub fn add_sound_break(&mut self, pos: Vec3) {
        let mut rng = mechaia::util::rand::thread_rng();
        for (r, v) in [4e2..=4.5e2, 5e2..=8e2, 1.5e3..=2e3]
            .into_iter()
            .zip([0.5, 0.3, 0.2])
        {
            let t = gen_tone_freq(rng.gen(), U0d32::FRAC_1_4 / 32, rng.gen_range(r), v);
            self.ping.push((pos, t));
        }
    }
}

impl Volume {
    fn apply(&mut self, f: f32) -> f32 {
        self.last = self.prev.lerp(self.cur, f);
        self.last
    }
}

impl WheelSound {
    fn next_samples(&mut self, receptor: &mut DualReceptor<'_>) {
        receptor.add(self.pan, |dt, f| {
            let v = wave::sine(self.t.0) * self.volume.apply(f);
            self.t += (U32d32::from(dt) * self.speed * 10).frac();
            v
        });
    }
}

impl LaserSound {
    fn next_samples(&mut self, receptor: &mut DualReceptor<'_>) {
        receptor.add(self.pan, |dt, f| {
            let vf = (self.dt_factor.to_f32() / 16.0).min(1.0);
            let v = mechaia::sound::mix::wave::sine(self.t.0).powi(3) * vf * self.volume.apply(f);

            self.t += (dt * self.dt_factor).frac();
            let f = (-dt.to_f32() * self.pitch_decay).exp();
            self.dt_factor *= U0d32::from_f32(f);

            v
        });
    }
}

impl PingSound {
    fn next_samples(&mut self, receptor: &mut DualReceptor<'_>) {
        receptor.add(self.pan, |dt, f| self.next_sample(dt, f));
    }

    fn next_sample(&mut self, dt: U0d32, f: f32) -> f32 {
        self.t += dt.wrapping_mul_u32(self.frequency * 3);
        let v = mechaia::sound::mix::wave::sine(self.t.0);
        self.hold = self.hold.saturating_sub(dt);
        if self.hold == U0d32::ZERO {
            self.amplitude *= (-dt.to_f32() * 4e2).exp2();
        }
        v * self.amplitude * self.volume.apply(f)
    }

    fn finish(&mut self) -> bool {
        self.amplitude < 1e-2
    }
}

fn gen_tone_freq(t: U0d32, hold: U0d32, f: f32, amplitude: f32) -> PingSound {
    PingSound {
        volume: Default::default(), // FIXME
        t: Wrapping(t),
        hold,
        frequency: f as u32,
        amplitude,
        pan: Default::default(),
    }
}
