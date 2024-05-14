fn main() {
    let target = std::env::args().skip(1).next().expect("path to lua file");
    let lua = std::fs::read(&target).expect("file not found");
    let mut parser = analisar::aware::Parser::new(&lua);
    let mut writer = escrever::Writer::new(&lua, std::io::stdout());
    while let Some(Ok(stmt)) = parser.next() {
        writer.write_stmt(&stmt.statement).unwrap()
    }
}
