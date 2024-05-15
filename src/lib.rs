use std::{collections::BTreeMap, io::Write};

use analisar::aware::ast;
use lex_lua::Span;

pub struct Writer<W: Write> {
    dest: W,
    last_offset: usize,
    line_map: BTreeMap<usize, &'static [u8]>,
}

impl<W: Write> Writer<W> {
    fn calculate_line_map(src: &[u8]) -> BTreeMap<usize, &'static [u8]> {
        let mut ret: BTreeMap<usize, &[u8]> = BTreeMap::new();
        let mut i = 0;
        while i < src.len() {
            let ch = src[i];
            let nl_set: &[u8] = match ch {
                b'\r' => {
                    if let Some(&b'\n') = src.get(i+1) {
                        &[b'\r',b'\n']
                    } else {
                        &[b'\r']
                    }
                }
                b'\n' => &[b'\n'],
                0xff => &[0xff],
                _ => {
                    i += 1;
                    continue;
                }
            };
            ret.insert(i, nl_set);
            i += nl_set.len();
        }
        ret
    }

    pub fn new(src: &[u8], dest: W) -> Self {
        Self {
            dest,
            last_offset: 0,
            line_map: Self::calculate_line_map(src),
        }
    }

    pub fn write_stmt(&mut self, stmt: &ast::Statement) -> std::io::Result<()> {
        log::trace!("write_stmt: {:?}", stmt);
        match stmt {
            ast::Statement::Empty(v) => self.write_spanned(v, b";"),
            ast::Statement::Expression(e) => self.write_expr(e),
            ast::Statement::Assignment {
                local_span,
                targets,
                eq_span,
                values,
            } => {
                if let Some(l) = local_span {
                    self.write_spanned(l, b"local")?;
                }
                for t in targets {
                    self.write_expr_list_item(t)?;
                }
                if let Some(e) = eq_span {
                    self.write_spanned(e, b"=")?;
                }
                for v in values {
                    self.write_expr_list_item(v)?;
                }
                Ok(())
            }
            ast::Statement::Label {
                colons1_span,
                name,
                colons2_span,
            } => {
                self.write_spanned(colons1_span, b":")?;
                self.write_name(name)?;
                self.write_spanned(colons2_span, b":")
            }
            ast::Statement::Break(s) => self.write_spanned(s, b"break"),
            ast::Statement::GoTo { goto_span, label } => {
                self.write_spanned(goto_span, b"goto")?;
                self.write_name(label)
            }
            ast::Statement::Do {
                do_span,
                block,
                end_span,
            } => {
                self.write_spanned(do_span, b"do")?;
                self.write_block(block)?;
                self.write_spanned(end_span, b"end")
            }
            ast::Statement::While {
                while_span,
                exp,
                do_span,
                block,
                end_span,
            } => {
                self.write_spanned(while_span, b"while")?;
                self.write_expr(exp)?;
                self.write_spanned(do_span, b"do")?;
                self.write_block(block)?;
                self.write_spanned(end_span, b"end")
            }
            ast::Statement::Repeat {
                repeat_span,
                block,
                until_span,
                exp,
            } => {
                self.write_spanned(repeat_span, b"repeat")?;
                self.write_block(block)?;
                self.write_spanned(until_span, b"until")?;
                self.write_expr(exp)
            }
            ast::Statement::If(s) => self.write_if(s),
            ast::Statement::For(f) => {
                self.write_spanned(&f.for_span, b"for")?;
                self.write_name(&f.init_name)?;
                self.write_spanned(&f.eq_span, b"=")?;
                self.write_expr(&f.init)?;
                self.write_spanned(&f.comma1_span, b",")?;
                self.write_expr(&f.limit)?;
                if let Some(c) = &f.comma2_span {
                    self.write_spanned(c, b",")?;
                }
                if let Some(e) = &f.step {
                    self.write_expr(e)?;
                }
                self.write_spanned(&f.do_span, b"do")?;
                self.write_block(&f.block)?;
                self.write_spanned(&f.end_span, b"end")
            }
            ast::Statement::ForIn(f) => {
                self.write_spanned(&f.for_span, b"for")?;
                for name in &f.name_list {
                    match name {
                        ast::NameListPart::Name(n) => self.write_name(n)?,
                        ast::NameListPart::Comma(s) => self.write_spanned(s, b",")?,
                    }
                }
                self.write_spanned(&f.in_span, b"in")?;
                for e in &f.exp_list {
                    self.write_expr_list_item(e)?;
                }
                self.write_spanned(&f.do_span, b"do")?;
                self.write_block(&f.block)?;
                self.write_spanned(&f.end_span, b"end")
            }
            ast::Statement::Function {
                local,
                function,
                name,
                body,
            } => {
                if let Some(l) = local {
                    self.write_spanned(l, b"local")?;
                }
                self.write_spanned(function, b"function")?;
                for part in &name.segments {
                    match part {
                        ast::FuncNamePart::Name(n) => self.write_name(n)?,
                        ast::FuncNamePart::Dot(d) => self.write_spanned(d, b".")?,
                        ast::FuncNamePart::Colon(c) => self.write_spanned(c, b":")?,
                    }
                }
                self.write_fn_def(body)
            }
            ast::Statement::Return(r) => {
                self.write_spanned(&r.return_span, b"return")?;
                for e in &r.exprs {
                    self.write_expr_list_item(e)?;
                }
                Ok(())
            }
        }
    }

    fn write_if(&mut self, s: &ast::If) -> std::io::Result<()> {
        self.write_spanned(&s.if_span, b"if")?;
        self.write_expr(&s.test)?;
        self.write_spanned(&s.then_span, b"then")?;
        self.write_block(&s.block)?;
        for ei in &s.else_ifs {
            self.write_else_if(ei)?;
        }
        if let Some(e) = &s.else_span {
            self.write_spanned(e, b"else")?;
        }
        if let Some(eb) = &s.catch_all {
            self.write_block(&eb)?;
        }
        self.write_spanned(&s.end_span, b"end")
    }

    fn write_else_if(&mut self, s: &ast::ElseIf) -> std::io::Result<()> {
        self.write_spanned(&s.else_if_span, b"elseif")?;
        self.write_expr(&s.test)?;
        self.write_spanned(&s.then_span, b"then")?;
        self.write_block(&s.block)
    }

    fn write_expr(&mut self, expr: &ast::Expression) -> std::io::Result<()> {
        match expr {
            ast::Expression::Nil(s) => self.write_spanned(s, b"nil"),
            ast::Expression::False(s) => self.write_spanned(s, b"false"),
            ast::Expression::True(s) => self.write_spanned(s, b"true"),
            ast::Expression::Numeral(n) => self.write_spanned(&n.span, n.numeral.as_bytes()),
            ast::Expression::LiteralString(l) => self.write_spanned(&l.span, l.value.as_ref()),
            ast::Expression::Name(n) => self.write_name(&n),
            ast::Expression::VarArgs(v) => self.write_spanned(v, b"..."),
            ast::Expression::FunctionDef(f) => {
                self.write(b"function")?;
                self.write_fn_def(&f)
            },
            ast::Expression::TableCtor(t) => self.write_table(t.as_ref()),
            ast::Expression::Parened {
                open_span,
                expr,
                close_span,
            } => {
                self.write_spanned(open_span, b"(")?;
                self.write_expr(expr.as_ref())?;
                self.write_spanned(close_span, b")")
            }
            ast::Expression::BinOp { left, op, right } => {
                self.write_expr(left)?;
                self.write_bin_op(op)?;
                self.write_expr(right)
            }
            ast::Expression::UnaryOp { op, exp } => {
                self.write_un_op(op)?;
                self.write_expr(exp)
            }
            ast::Expression::FuncCall(f) => self.write_call(f),
            ast::Expression::Suffixed(s) => self.write_suffixed(s.as_ref()),
        }
    }

    fn write_suffixed(&mut self, suffixed: &ast::Suffixed) -> std::io::Result<()> {
        self.write_expr(&suffixed.subject)?;
        match &suffixed.property {
            ast::SuffixedProperty::Name { sep, name } => {
                match sep {
                    ast::SuffixSep::Dot(d) => self.write_spanned(d, b".")?,
                    ast::SuffixSep::Colon(c) => self.write_spanned(c, b":")?,
                }
                self.write_name(name)
            }
            ast::SuffixedProperty::Computed {
                open_bracket,
                expr,
                close_bracket,
            } => {
                self.write_spanned(open_bracket, b"[")?;
                self.write_expr(expr)?;
                self.write_spanned(close_bracket, b"]")
            }
        }
    }

    fn write_call(&mut self, f: &ast::FunctionCall) -> std::io::Result<()> {
        self.write_expr(&f.prefix)?;
        self.write_args(&f.args)
    }

    fn write_args(&mut self, args: &ast::Args) -> std::io::Result<()> {
        match args {
            ast::Args::ExpList {
                open_paren,
                exprs,
                close_paren,
            } => {
                self.write_spanned(open_paren, b"(")?;
                for e in exprs {
                    self.write_expr_list_item(e)?;
                }
                self.write_spanned(close_paren, b")")
            }
            ast::Args::Table(t) => self.write_table(t),
            ast::Args::String(s) => self.write_spanned(&s.span, s.value.as_ref()),
        }
    }

    fn write_expr_list_item(&mut self, e: &ast::ExpListItem) -> std::io::Result<()> {
        match e {
            ast::ExpListItem::Expr(e) => self.write_expr(e),
            ast::ExpListItem::Comma(s) => self.write_spanned(s, b","),
        }
    }

    fn write_bin_op(&mut self, op: &ast::BinaryOperator) -> std::io::Result<()> {
        let (s, v) = match op {
            ast::BinaryOperator::Add(s) => (s, "+"),
            ast::BinaryOperator::Subtract(s) => (s, "-"),
            ast::BinaryOperator::Multiply(s) => (s, "*"),
            ast::BinaryOperator::Divide(s) => (s, "/"),
            ast::BinaryOperator::FloorDivide(s) => (s, "//"),
            ast::BinaryOperator::Power(s) => (s, "**"),
            ast::BinaryOperator::Modulo(s) => (s, "%"),
            ast::BinaryOperator::BitwiseAnd(s) => (s, "&"),
            ast::BinaryOperator::BitwiseXor(s) => (s, "^"),
            ast::BinaryOperator::BitwiseOr(s) => (s, "|"),
            ast::BinaryOperator::RightShift(s) => (s, ">>"),
            ast::BinaryOperator::LeftShift(s) => (s, "<<"),
            ast::BinaryOperator::Concatenate(s) => (s, ".."),
            ast::BinaryOperator::GreaterThan(s) => (s, ">"),
            ast::BinaryOperator::GreaterThanEqual(s) => (s, ">="),
            ast::BinaryOperator::LessThan(s) => (s, "<"),
            ast::BinaryOperator::LessThanEqual(s) => (s, "<="),
            ast::BinaryOperator::Equal(s) => (s, "=="),
            ast::BinaryOperator::NotEqual(s) => (s, "~="),
            ast::BinaryOperator::And(s) => (s, "and"),
            ast::BinaryOperator::Or(s) => (s, "or"),
        };
        self.write_spanned(s, v.as_bytes())
    }

    fn write_un_op(&mut self, op: &ast::UnaryOperator) -> std::io::Result<()> {
        let (s, v) = match op {
            ast::UnaryOperator::Negate(s) => (s, "-"),
            ast::UnaryOperator::Not(s) => (s, "not"),
            ast::UnaryOperator::Length(s) => (s, "#"),
            ast::UnaryOperator::BitwiseNot(s) => (s, "~"),
        };
        self.write_spanned(s, v.as_bytes())
    }

    fn write_table(&mut self, t: &ast::Table) -> std::io::Result<()> {
        self.write_spanned(&t.open_brace, b"{")?;
        for f in &t.field_list {
            self.write_field(f)?;
        }
        self.write_spanned(&t.close_brace, b"}")
    }

    fn write_field(&mut self, field: &ast::Field) -> std::io::Result<()> {
        match field {
            ast::Field::Record {
                name,
                eq,
                value,
                sep,
            } => {
                let needs_braces = !matches!(name, ast::Expression::Name(_));
                if needs_braces {
                    let s = name.start();
                    self.write_spanned(
                        &Span {
                            start: s - 1,
                            end: s,
                        },
                        b"[",
                    )?;
                }
                self.write_expr(name)?;
                if needs_braces {
                    let e = name.end();
                    self.write_spanned(
                        &Span {
                            start: e,
                            end: e + 1,
                        },
                        b"]",
                    )?;
                }
                self.write_spanned(eq, b"=")?;
                self.write_expr(value)?;
                if let Some(sep) = sep {
                    self.write_field_sep(sep)?;
                }
            }
            ast::Field::List { value, sep } => {
                self.write_expr(value)?;
                if let Some(sep) = sep {
                    self.write_field_sep(sep)?;
                }
            }
        }
        Ok(())
    }

    fn write_field_sep(&mut self, sep: &ast::FieldSep) -> std::io::Result<()> {
        match sep {
            ast::FieldSep::Comma(s) => self.write_spanned(s, b","),
            ast::FieldSep::Semi(s) => self.write_spanned(s, b";"),
        }
    }

    fn write_fn_def(&mut self, f: &ast::FuncBody) -> std::io::Result<()> {
        self.write_spanned(&f.open_paren_span, b"(")?;
        for p in &f.par_list.parts {
            match p {
                ast::ParListPart::Name(n) => self.write_name(&n)?,
                ast::ParListPart::Comma(s) => self.write_spanned(s, b",")?,
                ast::ParListPart::VarArgs(v) => self.write_spanned(v, b"...")?,
            }
        }
        self.write_spanned(&f.close_paren_span, b")")?;
        self.write_block(&f.block)?;
        self.write_spanned(&f.end_span, b"end")
    }

    fn write_block(&mut self, block: &ast::Block) -> std::io::Result<()> {
        for stmt in &block.0 {
            self.write_stmt(stmt)?;
        }
        Ok(())
    }

    fn write_name(&mut self, n: &ast::Name) -> std::io::Result<()> {
        self.write_spanned(&n.name_span, n.name.as_bytes())?;
        if let Some(attr) = &n.attr {
            self.write_spanned(&attr.open_angle, b"<")?;
            self.write(attr.value.as_bytes())?;
            self.write_spanned(&attr.close_angle, b">")?;
        }
        Ok(())
    }

    fn write_spanned(&mut self, span: &Span, bytes: &[u8]) -> std::io::Result<()> {
        log::trace!(
            "write_spanned {span:?} {:?} {:?}",
            String::from_utf8_lossy(bytes),
            self.last_offset
        );
        self.handle_whitespace(span)?;
        self.dest.write_all(bytes)?;
        self.last_offset = span.end;
        Ok(())
    }

    fn handle_whitespace(&mut self, span: &Span) -> std::io::Result<()> {
        log::trace!("{}->{}", self.last_offset, span.start);
        // copied or cloned wont work here because those expect iterators that return references but range returns
        // an owned tuple with references for fields.
        let new_lines: Vec<_> = self.line_map.range(self.last_offset..span.start).map(|(idx, ch)| {
            (*idx, *ch)
        }).collect();
        for (i, bytes) in &new_lines {
            self.write(bytes)?;
            self.last_offset = i + bytes.len();
        }
        for _ in self.last_offset..span.start {
            self.write(b" ")?;
        }
        if !new_lines.is_empty() {
            self.dest.flush()?;
        }
        Ok(())
    }

    fn write(&mut self, bytes: &[u8]) -> std::io::Result<()> {
        self.dest.write_all(bytes)?;
        self.last_offset = self.last_offset.wrapping_add(bytes.len());
        Ok(())
    }
}

impl<W> Drop for Writer<W>
where
    W: Write,
{
    fn drop(&mut self) {
        self.dest.write_all(b"\n").ok();
        self.dest.flush().ok();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use analisar::aware::Parser;

    #[test]
    fn assignment() {
        let lua = b"local T = {}\n";
        let mut p = Parser::new(lua);
        let mut dest = Vec::new();
        {
            let mut w = Writer::new(lua, &mut dest);
            while let Some(Ok(stmt)) = p.next() {
                w.write_stmt(&stmt.statement).unwrap();
            }
        }
        assert_eq!(
            lua.to_vec(),
            dest,
            "\n{:?}\n{:?}",
            String::from_utf8_lossy(lua),
            String::from_utf8_lossy(&dest)
        );
    }

    #[test]
    fn fn_expr() {
        let lua = b"(function() end)\n";
        let mut p = Parser::new(lua);
        let mut dest = Vec::new();
        {
            let mut w = Writer::new(lua, &mut dest);
            while let Some(Ok(stmt)) = p.next() {
                w.write_stmt(&stmt.statement).unwrap();
            }
        }
        assert_eq!(
            lua.to_vec(),
            dest,
            "\n{:?}\n{:?}",
            String::from_utf8_lossy(lua),
            String::from_utf8_lossy(&dest)
        );
    }

    #[test]
    fn preserve_early_whitespace() {
        let lua = br#"-- this is some early whitespace

local value = {}
"#;
        let mut p = Parser::new(lua);
        let mut dest = Vec::new();
        {
            let mut w = Writer::new(lua, &mut dest);
            while let Some(Ok(stmt)) = p.next() {
                w.write_stmt(&stmt.statement).unwrap();
            }
        }
        let expected: &[u8] = b"\n\nlocal value = {}\n".as_slice();
        assert_eq!(
            expected,
            dest,
            "\n{:?}\n{:?}",
            String::from_utf8_lossy(expected),
            String::from_utf8_lossy(&dest)
        );
    }
}
