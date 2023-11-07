use std::env;
use std::fs;
use std::usize;

#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
enum TokenKind {
    AUTO,
    REGISTER,
    RETURN,
    WHILE,
    IF,
    THEN,
    ELSE,

    Number(u64),
    Identifier(String),
    At,

    Bang,
    Tilde,

    Mult,
    Div,
    Mod,
    Plus,
    Minus,

    ShiftL,
    ShiftR,
    Lt,
    Gt,
    Le,
    Ge,
    Band,
    Bxor,
    Bor,
    And,
    Or,
    Equals,
    NotEquals,

    Assignment,
    Colon,
    Semicolon,

    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenBrace,
    CloseBrace,

    EOF,
}

#[derive(Debug, Clone)]
struct Token {
    token_type: TokenKind,
    line: usize,
    line_pos: usize,
}

struct Scanner {
    stream: Vec<u8>,
    tokens: Vec<Token>,

    pos: usize,

    line: usize,
    line_pos: usize,
}

impl Scanner {
    pub fn new(stream: Vec<u8>) -> Self {
        Self {
            stream,
            tokens: Vec::new(),
            pos: 0,
            line: 0,
            line_pos: 0,
        }
    }

    fn peek(&self) -> u8 {
        self.stream[self.pos + 1]
    }

    fn curr(&self) -> u8 {
        self.stream[self.pos]
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn finished(&self) -> bool {
        self.pos >= self.stream.len()
    }

    fn push_token(&mut self, token: TokenKind) {
        self.push_token_pos(token, self.line_pos);
        self.line_pos += 1;
    }
    fn push_token_pos(&mut self, token: TokenKind, line_pos: usize) {
        self.tokens.push(Token {
            token_type: token,
            line: self.line,
            line_pos,
        })
    }

    fn scan(&mut self) -> Vec<Token> {
        while !self.finished() {
            match self.curr() {
                b'{' => self.push_token(TokenKind::OpenBrace),
                b'}' => self.push_token(TokenKind::CloseBrace),
                b'[' => self.push_token(TokenKind::OpenSquare),
                b']' => self.push_token(TokenKind::CloseSquare),
                b'(' => self.push_token(TokenKind::OpenParen),
                b')' => self.push_token(TokenKind::CloseParen),

                b'@' => self.push_token(TokenKind::At),
                b',' => self.push_token(TokenKind::Colon),
                b';' => self.push_token(TokenKind::Semicolon),
                b'-' => self.push_token(TokenKind::Minus),
                b'+' => self.push_token(TokenKind::Plus),
                b'~' => self.push_token(TokenKind::Tilde),

                b'!' => {
                    if self.peek() == b'=' {
                        self.advance();
                        self.push_token_pos(TokenKind::NotEquals, self.line_pos);
                        self.line_pos += 2;
                    } else {
                        self.push_token(TokenKind::Bang)
                    }
                }

                b'&' => {
                    if self.peek() == b'&' {
                        self.advance();
                        self.push_token_pos(TokenKind::And, self.line_pos);
                        self.line_pos += 2;
                    } else {
                        self.push_token(TokenKind::Band)
                    }
                }
                b'|' => {
                    if self.peek() == b'|' {
                        self.advance();
                        self.push_token_pos(TokenKind::Or, self.line_pos);
                        self.line_pos += 2;
                    } else {
                        self.push_token(TokenKind::Bor)
                    }
                }
                b'^' => self.push_token(TokenKind::Bxor),

                b'*' => self.push_token(TokenKind::Mult),
                b'/' => {
                    if self.peek() == b'/' {
                        while self.curr() != b'\n' {
                            self.advance();
                        }
                        self.line_pos = 0;
                        self.line += 1;

                    } else {
                        self.push_token(TokenKind::Div)
                    }
                },
                b'%' => self.push_token(TokenKind::Mod),

                b'<' => {
                    if self.peek() == b'<' {
                        self.advance();
                        self.push_token_pos(TokenKind::ShiftL, self.line_pos);
                        self.line_pos += 2;
                    } else if self.peek() == b'=' {
                        self.advance();
                        self.push_token_pos(TokenKind::Le, self.line_pos);
                        self.line_pos += 2;
                    } else {
                        self.push_token(TokenKind::Lt)
                    }
                }
                b'>' => {
                    if self.peek() == b'>' {
                        self.advance();
                        self.push_token_pos(TokenKind::ShiftR, self.line_pos);
                        self.line_pos += 2;
                    } else if self.peek() == b'=' {
                        self.advance();
                        self.push_token_pos(TokenKind::Ge, self.line_pos);
                        self.line_pos += 2;
                    } else {
                        self.push_token(TokenKind::Gt)
                    }
                }
                b'=' => {
                    if self.peek() == b'=' {
                        self.advance();
                        self.push_token_pos(TokenKind::Equals, self.line_pos);
                        self.line_pos += 2;
                    } else {
                        self.push_token(TokenKind::Assignment)
                    }
                }

                b'\n' => {
                    self.line_pos = 0;
                    self.line += 1;
                }
                b' ' | b'\t' | b'\r' => self.line_pos += 1,
                _ => {
                    let start_pos = self.pos;
                    let multichar_token = if (self.curr() as char).is_alphabetic() {
                        let start_pos = self.pos;

                        while !self.finished() && (self.curr() as char).is_alphanumeric() {
                            self.advance();
                        }
                        let ident =
                            match String::from_utf8(self.stream[start_pos..self.pos].to_vec()) {
                                Ok(v) => v,
                                Err(e) => panic!("Invalid UTF-8 sequence: {}", e),
                            };

                        self.pos -= 1;
                        if let Some(value) = is_keyword(&ident) {
                            value
                        } else {
                            TokenKind::Identifier(ident)
                        }
                    } else if (self.curr() as char).is_numeric() {
                        let start_pos = self.pos;

                        while !self.finished() && (self.curr() as char).is_alphanumeric() {
                            self.advance();
                        }

                        let value = slice_ascii_u8_to_u64(&self.stream[start_pos..self.pos]);
                        self.pos -= 1;

                        TokenKind::Number(value)
                    } else {
                        panic!(
                            "invalid char at: line:{} pos:{} ch:{}",
                            self.line,
                            self.line_pos,
                            self.curr()
                        )
                    };

                    self.push_token_pos(multichar_token, self.line_pos);
                    self.line_pos += self.pos - start_pos;
                }
            }

            self.advance();
        }

        self.push_token(TokenKind::EOF);
        return self.tokens.clone();
    }
}

#[derive(Debug, PartialEq)]
enum AssignmentType {
    Auto,
    Register,
}

#[derive(Debug, PartialEq)]
enum UnaryType {
    None,
    Minus,
    Not,
    Tilde,
    Addr,
}

#[derive(Debug, PartialEq)]
enum BinaryType {
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    ShiftL,
    ShiftR,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    BinAnd,
    BinOr,
    BinXor,
    And,
    Or,
    Assign,
}

impl BinaryType {
    fn from_token(tk: &TokenKind) -> Self {
        match tk {
            TokenKind::Mult => BinaryType::Mult,
            TokenKind::Div => BinaryType::Div,
            TokenKind::Mod => BinaryType::Mod,
            TokenKind::Plus => BinaryType::Plus,
            TokenKind::Minus => BinaryType::Minus,
            TokenKind::ShiftL => BinaryType::ShiftL,
            TokenKind::ShiftR => BinaryType::ShiftR,
            TokenKind::Lt => BinaryType::Lt,
            TokenKind::Gt => BinaryType::Gt,
            TokenKind::Le => BinaryType::Le,
            TokenKind::Ge => BinaryType::Ge,
            TokenKind::Band => BinaryType::BinAnd,
            TokenKind::Bxor => BinaryType::BinXor,
            TokenKind::Bor => BinaryType::BinOr,
            TokenKind::And => BinaryType::And,
            TokenKind::Or => BinaryType::Or,
            TokenKind::Equals => BinaryType::Eq,
            TokenKind::NotEquals => BinaryType::Ne,
            TokenKind::Assignment => BinaryType::Assign,

            _ => unreachable!("unreachable binary type"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum StatementType {
    Assignment {
        assign_type: AssignmentType,
        identifier: String,
        expression: ExprType,
    },
    Return(Option<ExprType>),
    Block(Box<Node>),
    While {
        expression: ExprType,
        statement: Box<Node>,
    },
    If {
        expression: ExprType,
        statement: Box<Node>,
        else_statement: Option<Box<Node>>,
    },
    Expression(ExprType),
}

#[derive(Debug, PartialEq)]
enum ExprType {
    None,
    Paren(Box<ExprType>),
    FnCall {
        ident: String,
        arglist: Box<Node>,
    },
    Identifier(String),
    Number(u64),
    Unary {
        op: UnaryType,
        expr: Box<ExprType>,
    },
    Binary {
        op: BinaryType,
        left: Box<ExprType>,
        right: Box<ExprType>,
    },
    Subscript {
        left: Box<ExprType>,
        right: Box<ExprType>,
        size_spec: Option<Box<Node>>, // Node::SizeSpec
    },
}

#[derive(Debug, PartialEq)]
enum Node {
    Program(Vec<Node>),
    Function {
        ident: String,
        param_list: Option<Box<Node>>,
        block: Box<Node>,
    },
    ParamList(Vec<String>),
    Block(Vec<Node>),
    Statement(StatementType),
    ArgList(Vec<ExprType>),
    SizeSpec(u64),

    Empty,
}

fn indent(level: usize) -> String {
    "|   ".repeat(level)
    // match level {
    //     0 => "".to_string(),
    //     1 => "|___".to_string() ,
    //     2 => "|   |___".to_string(),
    //     _ => "|   ".to_owned() + &"    ".repeat(level-2) + "|___",
    // }
}

fn node_printer(node: &Node, level: usize) {
    print!("{}node: ", indent(level));
    match node {
        Node::Program(funcs) => {
            println!("program");
            funcs.iter().for_each(|f| node_printer(f, level + 1));
        }
        Node::Function {
            ident,
            param_list,
            block,
        } => {
            println!(
                "function (with{} paramlist): {}",
                if param_list.is_some() { "" } else { "out" },
                ident
            );
            if let Some(list) = param_list {
                node_printer(list, level + 1);
            }
            node_printer(block, level + 1);
        }
        Node::ParamList(list) => {
            println!("paramList");
            list.iter()
                .for_each(|f| println!("{}- {}", indent(level + 1), f));
        }
        Node::Block(block) => {
            println!("block");
            block.iter().for_each(|f| node_printer(f, level + 1));
        }
        Node::Statement(stmt) => {
            println!("statement");
            stmt_printer(stmt, level + 1)
        }
        Node::ArgList(list) => {
            println!("arglist");
            list.iter().for_each(|f| expr_printer(f, level + 1));
        }
        Node::SizeSpec(spec) => println!("sizespec @{}", spec),
        Node::Empty => println!("EMPTY????"),
    }
}

fn stmt_printer(stmt: &StatementType, level: usize) {
    print!("{}statement: ", indent(level));
    match stmt {
        StatementType::Assignment {
            assign_type,
            identifier,
            expression,
        } => {
            println!("assign {:?} {}", assign_type, identifier);
            expr_printer(expression, level + 1)
        }
        StatementType::Return(retr) => match retr {
            Some(expr) => {
                println!("return");
                expr_printer(expr, level + 1)
            }
            None => println!("return <empty>"),
        },
        StatementType::Block(block) => {
            println!("return");
            node_printer(block, level + 1)
        }
        StatementType::While {
            expression,
            statement,
        } => {
            println!("while");
            expr_printer(expression, level + 1);
            node_printer(statement, level + 1);
        }
        StatementType::If {
            expression,
            statement,
            else_statement,
        } => {
            println!(
                "if ({} else)",
                if else_statement.is_some() {
                    "with"
                } else {
                    "no"
                }
            );
            expr_printer(expression, level + 1);
            node_printer(statement, level + 1);
            match else_statement {
                Some(expr) => node_printer(expr, level + 1),
                None => {}
            }
        }
        StatementType::Expression(expr) => {
            println!("expression");
            expr_printer(expr, level + 1);
        }
    }
}

fn expr_printer(expr: &ExprType, level: usize) {
    print!("{}expr: ", indent(level));
    match expr {
        ExprType::None => println!("NONE?????"),
        ExprType::Paren(expr) => {
            println!("parenthesis");
            expr_printer(expr, level + 1);
        }
        ExprType::FnCall { ident, arglist } => {
            println!("function call: {}", ident);
            node_printer(arglist, level + 1);
        }
        ExprType::Identifier(ident) => println!("identifier: {}", ident),
        ExprType::Number(number) => println!("number: {}", number),
        ExprType::Unary { op, expr } => {
            println!("unary: {:?}", op);
            expr_printer(expr, level + 1);
        }
        ExprType::Binary { op, left, right } => {
            println!("binary: {:?}", op);
            expr_printer(left, level + 1);
            expr_printer(right, level + 1);
        }
        ExprType::Subscript {
            left,
            right,
            size_spec,
        } => {
            println!(
                "subscript (with{} sizespec)",
                if size_spec.is_some() { "" } else { "out" }
            );
            match size_spec {
                Some(expr) => node_printer(expr, level + 1),
                None => {}
            }
            expr_printer(left, level + 1);
            expr_printer(right, level + 1);
        }
    }
}

struct Parser {
    stream: Vec<Token>,
    cursor: usize,
}

#[allow(unused_macros)]
macro_rules! dbg_pos {
    ($a:expr) => {{
        let token = &$a.stream[$a.cursor];
        dbg!(&token.line, &token.line_pos, &token.token_type);
    }};
}

macro_rules! expect {
    ($self:ident, $token:expr, $loc:expr, $chr:expr) => {{
        if !$self.match_t($token) {
            let ctx = $self.context();
            panic!(
                "{}: expecting \"{}\" at line {}:{}, found \"{:?}\" token instead",
                $loc,
                $chr,
                ctx.line + 1,
                ctx.line_pos + 1,
                ctx.token_type
            );
        }
    }};
}

impl Parser {
    pub fn new(stream: Vec<Token>) -> Self {
        Self { stream, cursor: 0 }
    }

    fn current(&self) -> &TokenKind {
        &self.stream[self.cursor].token_type
    }

    fn context(&self) -> &Token {
        &self.stream[self.cursor]
    }

    fn advance(&mut self) -> &TokenKind {
        let res = &self.stream[self.cursor];
        if !self.is_end() {
            self.cursor += 1;
        }
        &res.token_type
    }

    fn parse_iterative(&mut self) -> Node {
        return Node::Empty;
    }

    fn parse(&mut self) -> Node {
        self.program()
    }

    fn is_end(&self) -> bool {
        *self.current() == TokenKind::EOF
    }

    fn prev(&self) -> &TokenKind {
        &self.stream[self.cursor - 1].token_type
    }

    // fn peek(&self) -> &TokenKind {
    //     &self.stream[self.cursor + 1].token_type
    // }

    fn match_t(&mut self, t: TokenKind) -> bool {
        if std::mem::discriminant(self.current()) == std::mem::discriminant(&t) {
            self.advance();
            return true;
        }
        return false;
    }

    fn program(&mut self) -> Node {
        let mut node = Node::Program(Vec::new());

        while !self.is_end() {
            let func = self.function();
            if let Node::Program(ref mut funcs) = node {
                funcs.push(func);
            }
        }

        return node;
    }

    fn function(&mut self) -> Node {
        if !self.match_t(TokenKind::Identifier("".to_string())) {
            return Node::Empty;
        }
        let TokenKind::Identifier(ident) = self.prev().clone() else {
            panic!("func: expecting identifier")
        };

        expect!(self, TokenKind::OpenParen, "func", "(");

        let param_list = self.param_list();

        expect!(self, TokenKind::CloseParen, "func", ")");

        let block = self.block();

        return Node::Function {
            ident,
            param_list: match param_list {
                Node::Empty => None,
                _ => Some(Box::new(param_list)),
            },
            block: Box::new(block),
        };
    }

    fn param_list(&mut self) -> Node {
        let mut list = Vec::new();

        if let TokenKind::Identifier(ident) = self.current().clone() {
            list.push(ident);
            self.advance();
        } else {
            return Node::Empty;
        };

        while self.match_t(TokenKind::Colon) {
            let TokenKind::Identifier(ident) = self.advance().clone() else {
                panic!("param_list: expecting identifier")
            };
            list.push(ident);
        }

        return Node::ParamList(list);
    }

    fn block(&mut self) -> Node {
        let mut list = Vec::new();
        if !self.match_t(TokenKind::OpenBrace) {
            return Node::Empty;
        }

        while !self.match_t(TokenKind::CloseBrace) {
            list.push(self.statement());
        }

        return Node::Block(list);
    }

    fn statement(&mut self) -> Node {
        // TODO: change into match statement
        if self.match_t(TokenKind::AUTO) {
            // TODO: patch auto assignment with all patches on register
            let TokenKind::Identifier(ident) = self.advance().clone() else {
                panic!("statement_auto: expecting identifier")
            };

            expect!(self, TokenKind::Assignment, "statement_auto", "=");

            let expr = self.expr();

            expect!(self, TokenKind::Semicolon, "statement_auto", ";");
            return Node::Statement(StatementType::Assignment {
                assign_type: AssignmentType::Auto,
                identifier: ident,
                expression: expr,
            });
        } else if self.match_t(TokenKind::REGISTER) {
            let TokenKind::Identifier(ident) = self.advance().clone() else {
                panic!("statement_register: expecting identifier")
            };

            expect!(self, TokenKind::Assignment, "statement_register", "=");
            
            let expr = self.expr();

            expect!(self, TokenKind::Semicolon, "statement_register", ";");

            return Node::Statement(StatementType::Assignment {
                assign_type: AssignmentType::Register,
                identifier: ident,
                expression: expr,
            });
        } else if self.match_t(TokenKind::RETURN) {
            let expr = self.expr();

            expect!(self, TokenKind::Semicolon, "statement_return", ";");

            if let ExprType::None = expr {
                return Node::Statement(StatementType::Return(None));
            }
            return Node::Statement(StatementType::Return(Some(expr)));
        } else if self.match_t(TokenKind::WHILE) {
            expect!(self, TokenKind::OpenParen, "statement_while", "(");
            let expr = self.expr();
            expect!(self, TokenKind::CloseParen, "statement_while", ")");
            let stmt = self.statement();
            return Node::Statement(StatementType::While {
                expression: expr,
                statement: Box::new(stmt),
            });
        } else if self.match_t(TokenKind::IF) {
            expect!(self, TokenKind::OpenParen, "statement_while", "(");
            let expression = self.expr();
            expect!(self, TokenKind::CloseParen, "statement_while", ")");
            let stmt = self.statement();

            let mut else_statement = None;
            if self.match_t(TokenKind::ELSE) {
                else_statement = Some(Box::new(self.statement()));
            }

            return Node::Statement(StatementType::If {
                expression,
                statement: Box::new(stmt),
                else_statement,
            });
        } else {
            let block = self.block();

            if let Node::Empty = block {
                let expr = self.expr();
                expect!(self, TokenKind::Semicolon, "statement_expr", ";");
                return Node::Statement(StatementType::Expression(expr));
            } else {
                return Node::Statement(StatementType::Block(Box::new(block)));
            }
        }
    }

    fn expr(&mut self) -> ExprType {
        return self.expr_assign();
    }

    fn expr_assign(&mut self) -> ExprType {
        let mut expr = self.expr_or();
        if let ExprType::None = expr {
            return ExprType::None;
        }

        while self.match_t(TokenKind::Assignment) {

            let right = self.expr_or();
            expr = ExprType::Binary {
                op: BinaryType::Assign,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_or(&mut self) -> ExprType {
        let mut expr = self.expr_and();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        while self.match_t(TokenKind::Or) {
            let right = self.expr_and();
            expr = ExprType::Binary {
                op: BinaryType::Or,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_and(&mut self) -> ExprType {
        let mut expr = self.expr_bit_or();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        while self.match_t(TokenKind::And) {
            let right = self.expr_bit_or();
            expr = ExprType::Binary {
                op: BinaryType::And,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_bit_or(&mut self) -> ExprType {
        let mut expr = self.expr_bit_xor();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        while self.match_t(TokenKind::Bor) {
            let right = self.expr_bit_xor();
            expr = ExprType::Binary {
                op: BinaryType::BinOr,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_bit_xor(&mut self) -> ExprType {
        let mut expr = self.expr_bit_and();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        while self.match_t(TokenKind::Bxor) {
            let right = self.expr_bit_and();
            expr = ExprType::Binary {
                op: BinaryType::BinXor,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_bit_and(&mut self) -> ExprType {
        let mut expr = self.expr_equality();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        while self.match_t(TokenKind::Band) {
            let right = self.expr_equality();
            expr = ExprType::Binary {
                op: BinaryType::BinAnd,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_equality(&mut self) -> ExprType {
        let mut expr = self.expr_inequality();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        let curr = self.current().clone();
        while self.match_t(TokenKind::Equals) || self.match_t(TokenKind::NotEquals) {
            let right = self.expr_inequality();
            expr = ExprType::Binary {
                op: BinaryType::from_token(&curr),
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }

    fn expr_inequality(&mut self) -> ExprType {
        let mut expr = self.expr_shift();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        let curr = self.current().clone();
        while self.match_t(TokenKind::Lt)
            || self.match_t(TokenKind::Gt)
            || self.match_t(TokenKind::Le)
            || self.match_t(TokenKind::Ge)
        {
            let right = self.expr_shift();
            expr = ExprType::Binary {
                op: BinaryType::from_token(&curr),
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }

    fn expr_shift(&mut self) -> ExprType {
        let mut expr = self.expr_term();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        let curr = self.current().clone();
        while self.match_t(TokenKind::ShiftL) || self.match_t(TokenKind::ShiftR) {
            let right = self.expr_term();
            expr = ExprType::Binary {
                op: BinaryType::from_token(&curr),
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_term(&mut self) -> ExprType {
        let mut expr = self.expr_factor();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        let curr = self.current().clone();
        while self.match_t(TokenKind::Plus) || self.match_t(TokenKind::Minus) {
            let right = self.expr_factor();
            expr = ExprType::Binary {
                op: BinaryType::from_token(&curr),
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_factor(&mut self) -> ExprType {
        let mut expr = self.expr_unary();
        if let ExprType::None = expr {
            return ExprType::None;
        }
        let curr = self.current().clone();
        while self.match_t(TokenKind::Mult)
            || self.match_t(TokenKind::Div)
            || self.match_t(TokenKind::Mod)
        {
            let right = self.expr_unary();
            expr = ExprType::Binary {
                op: BinaryType::from_token(&curr),
                left: Box::new(expr),
                right: Box::new(right),
            }
        }
        return expr;
    }
    fn expr_unary(&mut self) -> ExprType {
        let mut in_while = false;
        let mut expr: ExprType = ExprType::None;

        while self.match_t(TokenKind::Minus)
            || self.match_t(TokenKind::Bang)
            || self.match_t(TokenKind::Tilde)
            || self.match_t(TokenKind::Band)
        {
            in_while = true;
            let op = match &self.prev() {
                TokenKind::Minus => UnaryType::Minus,
                TokenKind::Bang => UnaryType::Not,
                TokenKind::Tilde => UnaryType::Tilde,
                TokenKind::Band => UnaryType::Addr,
                _ => UnaryType::None,
            };

            let e_sub = self.expr_subscript();
            expr = ExprType::Unary {
                op,
                expr: Box::new(e_sub),
            };
        }

        if in_while {
            return expr;
        }

        return self.expr_subscript();
    }
    fn expr_subscript(&mut self) -> ExprType {
        let left = self.expr_primary();
        if let ExprType::None = left {
            return ExprType::None;
        }

        // TODO: handle multiple/chaining subscript operator
        // while self.match_t(TokenKind::OpenSquare) {}
        if !self.match_t(TokenKind::OpenSquare) {
            return left;
        }

        let right = Box::new(self.expr());
        let size_spec = self.size_spec();

        expect!(self, TokenKind::CloseSquare, "expr_subscript", "]");

        return ExprType::Subscript {
            left: Box::new(left),
            right,
            size_spec,
        };
    }
    fn expr_primary(&mut self) -> ExprType {
        match self.current().clone() {
            TokenKind::OpenBrace => {
                self.advance();
                let expr = self.expr();
                expect!(self, TokenKind::CloseBrace, "expr_primary", "}");
                return ExprType::Paren(Box::new(expr));
            }
            TokenKind::Identifier(ident) => {
                self.advance();
                if self.match_t(TokenKind::OpenParen) {
                    let arglist = Box::new(self.arg_list());
                    let node = ExprType::FnCall { ident, arglist };
                    expect!(self, TokenKind::CloseParen, "expr_primary", ")");
                    return node;
                }
                return ExprType::Identifier(ident);
            }
            TokenKind::Number(number) => {
                self.advance();
                return ExprType::Number(number);
            }
            _ => {
                return ExprType::None;
            }
        }
    }

    fn arg_list(&mut self) -> Node {
        let mut list = Vec::new();

        list.push(self.expr());

        while self.match_t(TokenKind::Colon) {
            list.push(self.expr());
        }

        return Node::ArgList(list);
    }

    fn size_spec(&mut self) -> Option<Box<Node>> {
        if !self.match_t(TokenKind::At) {
            return None;
        }

        if let TokenKind::Number(number) = self.current().clone() {
            self.advance();
            return Some(Box::new(Node::SizeSpec(number)));
        } else {
            let ctx = self.context();
            panic!(
                "sizespec: expecting a number at line {}:{}, found \"{:?}\" token instead",
                ctx.line + 1,
                ctx.line_pos + 1,
                ctx.token_type
            );
        };
    }
}

fn is_keyword(token: &String) -> Option<TokenKind> {
    return match token.as_ref() {
        "auto" => Some(TokenKind::AUTO),
        "register" => Some(TokenKind::REGISTER),
        "return" => Some(TokenKind::RETURN),
        "while" => Some(TokenKind::WHILE),
        "if" => Some(TokenKind::IF),
        "then" => Some(TokenKind::THEN),
        "else" => Some(TokenKind::ELSE),
        _ => None,
    };
}

fn slice_ascii_u8_to_u64(slice: &[u8]) -> u64 {
    let mut sum = 0;
    let len = slice.len();
    for i in 0..len {
        sum += (slice[i] - b'0') as u64 * (len - i) as u64;
    }
    sum
}

fn main() {
    let mut args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        // panic!("usage: {} <source.b>", args[0])
        args.push("test1.b".to_string());
    }

    let file_path = &args[1];

    let contents = fs::read(file_path).expect(format!("usage: {} <source.b>", args[0]).as_str());

    let mut scanner = Scanner::new(contents);

    let result = scanner.scan();

    print!(
        "{:?}\n\n",
        result
            .iter()
            .map(|x| &x.token_type)
            .collect::<Vec<&TokenKind>>()
    );

    let mut parser = Parser::new(result);

    let ast = parser.parse();
    node_printer(&ast, 0);
}