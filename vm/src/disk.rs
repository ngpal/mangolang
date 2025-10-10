use std::fs;
use std::io::{Read, Write};
use std::path::Path;

const TABLE_SIZE: usize = u16::MAX as usize;
const SECTOR_SIZE: usize = 256;
const DISK_FOLDER: &str = "disk";

pub struct DiskDriver {
    sector_table: [bool; TABLE_SIZE],
}

impl DiskDriver {
    pub fn new() -> Self {
        let mut table = [false; TABLE_SIZE];
        let table_path = Path::new(DISK_FOLDER).join("table.bin");

        if table_path.exists() {
            if let Ok(mut file) = fs::File::open(table_path) {
                let mut buf = vec![0u8; TABLE_SIZE];
                if file.read_exact(&mut buf).is_ok() {
                    for (i, &b) in buf.iter().enumerate() {
                        table[i] = b != 0;
                    }
                }
            }
        }

        // ensure disk folder exists
        fs::create_dir_all(DISK_FOLDER).ok();

        Self {
            sector_table: table,
        }
    }

    pub fn get_sector(&self, addr: u16) -> [u8; SECTOR_SIZE] {
        let idx = addr as usize;
        if !self.sector_table[idx] {
            return [0u8; SECTOR_SIZE]; // empty sector
        }

        let path = Path::new(DISK_FOLDER).join(format!("sector_{:04X}.bin", addr));
        let mut data = [0u8; SECTOR_SIZE];
        if let Ok(mut f) = fs::File::open(path) {
            let _ = f.read_exact(&mut data);
        }
        data
    }

    pub fn write_sector(&mut self, data: &[u8; SECTOR_SIZE], addr: u16) -> bool {
        let idx = addr as usize;
        let path = Path::new(DISK_FOLDER).join(format!("sector_{:04X}.bin", addr));

        if let Ok(mut f) = fs::File::create(&path) {
            if f.write_all(data).is_ok() {
                self.sector_table[idx] = true;
                return true;
            }
        }
        false
    }

    pub fn persist_table(&self) {
        let table_path = Path::new(DISK_FOLDER).join("table.bin");
        let buf: Vec<u8> = self
            .sector_table
            .iter()
            .map(|&b| if b { 1 } else { 0 })
            .collect();
        let _ = fs::write(table_path, &buf);
    }
}
