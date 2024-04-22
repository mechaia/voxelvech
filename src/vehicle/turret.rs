use mechaia::math::{Vec2, Vec3, Vec3Swizzles};

/// Calculate turret pan and tilt to aim directly at given target in local coordinates.
///
/// It is able to account for offset of tilt joint in 2D space.
/// Pan joint must be at origin.
///
/// TODO compensate for lateral offset.
pub fn calc_pan_tilt(target: Vec3, tilt_joint_offset: Vec2) -> (f32, f32) {
    // start with pan
    // project point to XY surface, then dot (cos) and cross (sin),
    // (in fact, dot/cross isn't necessary since we work in local space, just normalize)
    // which then allows us to calc atan2
    let target_xy = target.xy();
    let target_xy_dir = target_xy.normalize();
    let pan = target_xy_dir.y.atan2(target_xy_dir.x);

    // pretend pan rotation is fully applied
    // in this case, X' = length(XY) and Z' = Z
    // also apply offset
    // then simply normalize and atan2
    let target_xz = Vec2::new(target_xy.length(), target.z);
    let target_xz = target_xz - tilt_joint_offset;
    let target_xz_dir = target_xz.normalize();
    let tilt = target_xz_dir.y.atan2(target_xz_dir.x);

    (pan, tilt)
}
