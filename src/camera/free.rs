use mechaia::{
    math::{EulerRot, Quat, Vec3},
    render::CameraProjection,
};
use std::f32::consts::{FRAC_PI_2, TAU};

pub struct FreeCamera {
    translation: Vec3,
    pan: f32,
    tilt: f32,
}

impl FreeCamera {
    pub fn new() -> Self {
        Self {
            translation: Vec3::ZERO,
            pan: 0.0,
            tilt: 0.0,
        }
    }

    pub fn translate(&mut self, diff: Vec3) {
        // FIXME the fuck we need to invert for?
        self.translation += self.rotation().inverse() * diff;
    }

    pub fn add_pan(&mut self, diff: f32) {
        self.pan = (self.pan + diff).rem_euclid(TAU);
    }

    pub fn add_tilt(&mut self, diff: f32) {
        self.tilt = (self.tilt + diff).clamp(-FRAC_PI_2, FRAC_PI_2);
    }

    pub fn translation(&self) -> Vec3 {
        self.translation
    }

    pub fn direction(&self) -> Vec3 {
        // FIXME the fuck we need to invert for?
        self.rotation().inverse() * Vec3::X
    }

    fn rotation(&self) -> Quat {
        Quat::from_euler(EulerRot::YZX, self.tilt, self.pan, 0.0)
    }

    pub fn to_render(&self, aspect: f32) -> mechaia::render::Camera {
        mechaia::render::Camera {
            translation: self.translation,
            rotation: self.rotation(),
            projection: CameraProjection::Perspective { fov: FRAC_PI_2 },
            aspect,
            near: 0.1,
            far: 100.0,
        }
    }
}
