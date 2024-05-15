use std::path::{Path, PathBuf};

use walkdir::WalkDir;

fn main() {
    env_logger::init();
    let target = PathBuf::from(std::env::args().skip(1).next().expect("path to dir to walk"));
    let shared_root = target.parent().expect("non-root dir");
    let dest = shared_root.join(format!("{}-stripped", target.file_name().unwrap().to_str().unwrap()));
    let diff = shared_root.join(format!("{}-diffs",  target.file_name().unwrap().to_str().unwrap()));
    for entry in WalkDir::new(&target) {
        let Ok(entry) = entry else {
            continue;
        };
        if entry.file_type().is_dir() {
            continue;
        }
        if !entry.path().extension().map(|e| e == "lua").unwrap_or(false) {
            continue;
        }
        let orig = std::fs::read(entry.path()).expect("file not found");
        let stripped = do_one(&orig);
        let dest_path = entry.path().strip_prefix(&target).expect("shared root");
        let dest_path = dest.join(dest_path);
        let p = dest_path.parent().expect("non-root dest");
        std::fs::create_dir_all(p).ok();
        std::fs::write(&dest_path, &stripped).unwrap();
        // return;
        let diff_path = entry.path().strip_prefix(&target).expect("shared root");
        let diff_path = diff.join(diff_path).with_extension("diff");
        
        let p = diff_path.parent().expect("non-root diff");
        std::fs::create_dir_all(p).ok();
        if let Some(changes) = generate_diff(&orig, &stripped, &entry.path(), &dest_path) {
            std::fs::write(&diff_path, changes).unwrap();
        } else{
            std::fs::remove_file(&diff_path).ok();
        }
    }
}

fn do_one(lua: &[u8]) -> Vec<u8> {
    let mut parser = analisar::aware::Parser::new(lua);
    let mut ret = Vec::new();
    {let mut writer = escrever::Writer::new(&lua, &mut ret);
    while let Some(Ok(stmt)) = parser.next() {
        writer.write_stmt(&stmt.statement).unwrap()
    }}
    ret
}

fn generate_diff(
    orig: &[u8],
    stripped: &[u8],
    o_path: &Path,
    s_path: &Path,
) -> Option<String> {
    let splitter = |v: &u8| *v == b'\n' || *v == b'\r' || *v == 0xff;
    let mut ret = String::new();
    for (line_no, (o, s)) in orig.split(splitter).zip(stripped.split(splitter)).enumerate() {
        if o == s {
            continue;
        }
        let original = String::from_utf8_lossy(o);
        let stripped = String::from_utf8_lossy(s);
        if original.contains("--") {
            continue;
        }
        if original.trim_end() == stripped {
            continue;
        }
        let mut non_whitespace_err = false;
        for (l, r) in o.iter().zip(s.iter()) {
            if l == r {
                continue;
            }
            if !(*l == b'\t' && *r == b' ') {
                non_whitespace_err = true;
                break;
            }
        }
        if non_whitespace_err {
            ret.push_str(&format!("line number {line_no}\n"));
            ret.push_str(&format!("--- {}\n", o_path.display()));
            ret.push_str(&format!("+++ {}\n", s_path.display()));
            ret.push_str(&format!("+ {stripped}\n"));
            ret.push_str(&format!("- {original}\n"));
        }
    }
    if ret.is_empty() {
        return None;
    }
    Some(ret)
}
