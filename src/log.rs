use std::{collections::VecDeque, sync::Mutex, time::Instant};

const MAX_LINES: usize = 128;

static LOG: Mutex<VecDeque<(Level, Box<str>)>> = Mutex::new(VecDeque::new());

#[derive(Clone, Copy)]
pub enum Level {
    Error,
    Warning,
    Success,
    Info,
    Debug,
}

fn push(level: Level, msg: Box<str>) {
    let lvl = match level {
        Level::Error => "error",
        Level::Warning => "warning",
        Level::Success => "success",
        Level::Info => "info",
        Level::Debug => "debug",
    };
    eprintln!("[{lvl}] {msg}");

    let mut log = LOG.lock().unwrap();
    if log.len() >= MAX_LINES {
        log.pop_back();
    }
    log.push_front((level, msg));
}

pub fn error<T: Into<Box<str>>>(msg: T) {
    push(Level::Error, msg.into())
}

pub fn warn<T: Into<Box<str>>>(msg: T) {
    push(Level::Warning, msg.into())
}

pub fn success<T: Into<Box<str>>>(msg: T) {
    push(Level::Success, msg.into())
}

pub fn info<T: Into<Box<str>>>(msg: T) {
    push(Level::Info, msg.into())
}

pub fn debug<T: Into<Box<str>>>(msg: T) {
    push(Level::Debug, msg.into())
}

pub fn iter_with(f: &mut dyn FnMut(Level, &str)) {
    let log = LOG.lock().unwrap();
    for (l, s) in log.iter() {
        f(*l, s)
    }
}
