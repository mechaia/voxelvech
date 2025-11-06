use crate::log;
use std::{env, fs, io::Read};

fn dir(env: &str) -> String {
    env::var(env).map_or_else(
        |_| "".to_string(),
        |s| {
            if !s.ends_with("/") {
                s + "/"
            } else {
                s
            }
        },
    )
}

fn asset_dir() -> String {
    dir("VOXELVECH_ASSET_DIR")
}

fn user_dir() -> String {
    dir("VOXELVECH_USER_DIR")
}

fn with_dir(f: impl FnOnce() -> String, path: &str) -> Option<String> {
    if ["../", "/.."].iter().any(|s| path.contains(s)) {
        // TODO backtrace to find the culprit
        log::warn("attempted to traverse up directory tree!");
        return None;
    }
    Some(f() + path)
}

fn open_read(f: impl FnOnce() -> String, path: &str) -> Option<fs::File> {
    let path = with_dir(f, path)?;
    match fs::OpenOptions::new().read(true).open(&path) {
        Ok(f) => Some(f),
        Err(e) => {
            log::error(format!("failed to open {path}: {e}"));
            None
        }
    }
}

fn open_read_asset(path: &str) -> Option<fs::File> {
    open_read(asset_dir, path)
}

fn open_read_user(path: &str) -> Option<fs::File> {
    open_read(user_dir, path)
}

pub fn read_asset(path: &str) -> Option<Vec<u8>> {
    let mut v = Vec::new();
    match open_read_asset(path)?.read_to_end(&mut v) {
        Ok(_) => Some(v),
        Err(e) => {
            log::error(format!("failed to read {path}: {e}"));
            None
        }
    }
}

pub fn read_asset_string(path: &str) -> Option<String> {
    match String::from_utf8(read_asset(path)?) {
        Ok(s) => Some(s),
        Err(e) => {
            log::error(format!("{path} is not valid UTF-8"));
            None
        }
    }
}

pub fn read_asset_gltf(path: &str) -> Option<mechaia::model::Collection> {
    let bytes = read_asset(path)?;
    Some(mechaia::model::gltf::from_glb_slice(&bytes))
}

pub fn read_user(path: &str) -> Option<Vec<u8>> {
    let mut v = Vec::new();
    match open_read_user(path)?.read_to_end(&mut v) {
        Ok(_) => Some(v),
        Err(e) => {
            log::error(format!("failed to read {path}: {e}"));
            None
        }
    }
}

pub fn read_user_string(path: &str) -> Option<String> {
    match String::from_utf8(read_user(path)?) {
        Ok(s) => Some(s),
        Err(e) => {
            log::error(format!("{path} is not valid UTF-8"));
            None
        }
    }
}
