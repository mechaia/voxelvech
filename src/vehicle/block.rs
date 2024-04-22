use mechaia::math::Quat;

#[derive(Copy, Clone)]
pub struct Block {
    pub id: u32,
    pub orientation: u8,
}

impl Block {
    pub fn new(id: u32, direction: u8, rotation: u8) -> Self {
        let mut s = Self { id, orientation: 0 };
        s.set_direction(direction);
        s.set_rotation(rotation);
        s
    }

    pub fn direction(&self) -> u8 {
        self.orientation & 7
    }

    pub fn rotation(&self) -> u8 {
        self.orientation >> 3
    }

    pub fn set_direction(&mut self, direction: u8) {
        debug_assert!(direction < 6);
        self.orientation &= !7;
        self.orientation |= direction;
    }

    pub fn set_rotation(&mut self, rotation: u8) {
        debug_assert!(rotation < 4);
        self.orientation &= 7;
        self.orientation |= rotation << 3;
    }

    pub fn orientation(&self) -> Quat {
        use core::f32::consts::{FRAC_PI_2, PI};
        let r = match self.direction() {
            0 => Quat::IDENTITY,
            1 => Quat::from_rotation_x(PI),
            2 => Quat::from_rotation_x(-FRAC_PI_2),
            3 => Quat::from_rotation_x(FRAC_PI_2),
            4 => Quat::from_rotation_y(FRAC_PI_2),
            5 => Quat::from_rotation_y(-FRAC_PI_2),
            _ => unreachable!(),
        };
        r * Quat::from_rotation_z(FRAC_PI_2 * f32::from(self.rotation()))
    }
}

