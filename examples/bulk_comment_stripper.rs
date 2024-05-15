use std::path::PathBuf;

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
        let mut changes = Vec::new();
        for v in diff::lines(&String::from_utf8_lossy(&orig), &String::from_utf8_lossy(&stripped)) {
            match v {
                diff::Result::Left(removed) => {
                    changes.push(format!("- {removed}"));
                },
                diff::Result::Right(added) => {
                    changes.push(format!("+ {added}"));
                },
                diff::Result::Both(_, _) => {},
            }
        }
        if changes.is_empty() {
            std::fs::remove_file(&diff_path).ok();
        } else{
            std::fs::write(&diff_path, changes.join("\n")).unwrap();
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
