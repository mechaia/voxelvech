use crate::log;
use std::{env, fs, io::Read};

fn asset_dir() -> String {
    env::var("VOXELVECH_ASSET_DIR").map_or_else(
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

fn with_asset_dir(path: &str) -> Option<String> {
    if ["../", "/.."].iter().any(|s| path.contains(s)) {
        // TODO backtrace to find the culprit
        log::warn("attempted to traverse up directory tree!");
        return None;
    }
    Some(asset_dir() + path)
}

fn open_read(path: &str) -> Option<fs::File> {
    let path = with_asset_dir(path)?;
    match fs::OpenOptions::new().read(true).open(&path) {
        Ok(f) => Some(f),
        Err(e) => {
            log::error(format!("failed to open {path}: {e}"));
            None
        }
    }
}

pub fn read_asset(path: &str) -> Option<Vec<u8>> {
    let mut v = Vec::new();
    match open_read(path)?.read_to_end(&mut v) {
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
