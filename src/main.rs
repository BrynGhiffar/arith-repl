use std::{io, rc::Rc};

#[derive(Debug)]
enum PointerError {
    OutOfBounds
}

#[derive(Debug)]
struct Pointer<T> where T: Clone {
    text: Vec<T>,
    position: usize
}

impl Pointer<char> {

    fn from_str<'a>(text: &'a str) -> Self {
        let text_char: Vec<char> = text.trim().chars().collect();
        
        Pointer {text: text_char, position: 0}
    }

    fn peek_n_char(&self, offset: usize) -> Result<String, PointerError> {
        let index = self.position + offset;
        return match self.text.get(self.position..index) {
            Some(val) => Ok(val.to_vec().into_iter().collect()),
            None => Err(PointerError::OutOfBounds)
        }
    }

}

impl<T> Pointer<T> where T: Clone {


    fn from(tokens: Vec<T>) -> Self {
        Pointer { text: tokens, position: 0}
    }

    fn peek_n(&self, offset: usize) -> Result<Vec<T>, PointerError> {
        let index = self.position + offset;
        return match self.text.get(self.position..index) {
            Some(val) => Ok(val.to_vec()),
            None => Err(PointerError::OutOfBounds)
        }
    }

    fn peek(&self) -> Result<T, PointerError> {
        match self.text.get(self.position) {
            Some(c) => Ok(c.clone()),
            None => Err(PointerError::OutOfBounds)
        }
    }

    fn next_n(&self, offset: usize) -> Pointer<T> {
        let new_position = self.position + offset;
        return Pointer {text: self.text.clone(), position: new_position};
    }

    fn next(&self) -> Pointer<T> {
        self.next_n(1)
    }

    fn next_while<P>(&self, mut predicate: P) -> Pointer<T> 
        where P: FnMut(T) -> bool
    {
        let mut new_position = self.position;
        while let Some(item) = self.text.get(new_position) {
            if predicate(item.clone()) {
                new_position += 1;
                continue;
            } else {
                break;
            }
        }

        return Pointer { text: self.text.clone(), position: new_position };
    }


    fn clone(&self) -> Pointer<T> {
        Pointer {text: self.text.clone(), position: self.position}
    }

}

#[derive(Debug, Clone)]
enum Token {
    Number(i32),
    Plus, Minus, Multiply, Divide,
    Whitespace,
    OpenRoundPar, CloseRoundPar
}

enum LexerError {
    CouldNotLex,
    BadCharacter
}

struct Lexer {
    pointer: Pointer<char>
}

impl Lexer {

    fn new<'a>(text: &'a str) -> Self {
        Lexer { pointer: Pointer::from_str(text) }
    }

    fn lex_number(&mut self) -> Result<Token, LexerError>{
        let mut digits: Vec<u32> = Vec::new();
        let pointer = self.pointer.next_while(
            |c| match c.to_digit(10) {
                Some(dig) => {
                    digits.push(dig);
                    return true;
                },
                None => false
            }
        );
        if digits.len() == 0 { return Err(LexerError::CouldNotLex);}

        let mut tot = 0;
        for (i, e) in digits.iter().rev().enumerate() {
            tot += 10_u32.pow(i.try_into().unwrap()) * e;
        }
        self.pointer = pointer;

        return Ok(Token::Number(tot.try_into().unwrap()));

    }

    fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut res: Vec<Token> = Vec::new();

        while let Ok(c) = self.pointer.peek() {
            match self.lex_number() {
                Ok(num) => { 
                    res.push(num);
                    continue;
                },
                Err(_) => ()
            }

            if c == '+' {
                res.push(Token::Plus);
                self.pointer = self.pointer.next();
                continue;                
            }

            if c == '-' {
                res.push(Token::Minus);
                self.pointer = self.pointer.next();
                continue;
            }

            if c == '/' {
                res.push(Token::Divide);
                self.pointer = self.pointer.next();
                continue;
            }

            if c == '*' {
                res.push(Token::Multiply);
                self.pointer = self.pointer.next();
                continue;
            }

            if c.is_whitespace() {
                res.push(Token::Whitespace);
                self.pointer = self.pointer.next();
                continue;
            }

            if c == '(' {
                res.push(Token::OpenRoundPar);
                self.pointer = self.pointer.next();
                continue;
            }

            if c == ')' {
                res.push(Token::CloseRoundPar);
                self.pointer = self.pointer.next();
                continue;
            }

            return Err(LexerError::BadCharacter);
        }

        return Ok(res);
    }

}

struct Parser {
    pointer: Pointer<Token>
}


#[derive(Debug)]
enum ParseNode {
    NumberNode(i32),
    Add { left: Rc<ParseNode>, right: Rc<ParseNode> },
    Subtract { left: Rc<ParseNode>, right: Rc<ParseNode> },
    DivideNode { left: Rc<ParseNode>, right: Rc<ParseNode> },
    Multiply { left: Rc<ParseNode>, right: Rc<ParseNode> },
}

impl ParseNode {

    fn pretty_print(&self) {
        match self {
            ParseNode::NumberNode(_) => {
                println!("{:?}", self);
            },
            ParseNode::Add {left, right} => {
                println!("Add");
                ParseNode::pretty_print_aux(Rc::clone(&left), 0, 0b0);
                ParseNode::pretty_print_aux(Rc::clone(&right), 0, 0b1);
            },
            ParseNode::Subtract { left, right } => {
                println!("Subtract");
                ParseNode::pretty_print_aux(Rc::clone(&left), 0, 0b1);
                ParseNode::pretty_print_aux(Rc::clone(&right), 0, 0b1);
            },
            ParseNode::DivideNode { left, right } =>  {
                println!("Divide");
                ParseNode::pretty_print_aux(Rc::clone(&left), 0, 0b0);
                ParseNode::pretty_print_aux(Rc::clone(&right), 0, 0b1);
            },
            ParseNode::Multiply { left, right } => {
                println!("Multiply");
                ParseNode::pretty_print_aux(Rc::clone(&left), 0, 0b0);
                ParseNode::pretty_print_aux(Rc::clone(&right), 0, 0b1);
            } 
        }
    }

    fn pretty_print_aux(node: Rc<ParseNode>, depth: usize, is_last: usize) {
        for i in 0..(depth * 4) {
            let d = i / 4;
            if (i % 4 == 0) && (is_last >> d & 0b1) == 0b0 {
                print!("│");
            } else {
                print!(" ");
            }
        }
        if (is_last >> depth & 0b1) == 0b1 {
            print!("└── ")
        } else {
            print!("├── ");
        }
        match &*node {
            ParseNode::NumberNode(_) => println!("{:?}", node),
            ParseNode::Add { left, right } =>{
                println!("Add");
                let mask = 1 << (depth + 1);
                ParseNode::pretty_print_aux(Rc::clone(&left), depth + 1, is_last);
                ParseNode::pretty_print_aux(Rc::clone(&right), depth + 1, is_last | mask);
            },
            ParseNode::Subtract {left, right} => {
                println!("Subtract");
                let mask = 1 << (depth + 1);
                ParseNode::pretty_print_aux(Rc::clone(&left), depth + 1, is_last);
                ParseNode::pretty_print_aux(Rc::clone(&right), depth + 1, is_last | mask);
            },
            ParseNode::DivideNode { left, right } => {
                println!("Divide");
                let mask = 1 << (depth + 1);
                ParseNode::pretty_print_aux(Rc::clone(&left), depth + 1, is_last);
                ParseNode::pretty_print_aux(Rc::clone(&right), depth + 1, is_last | mask);
            },
            ParseNode::Multiply { left, right } => {
                println!("Multiply");
                let mask = 1 << (depth + 1);
                ParseNode::pretty_print_aux(Rc::clone(&left), depth + 1, is_last);
                ParseNode::pretty_print_aux(Rc::clone(&right), depth + 1, is_last | mask);
            }
        }

    }
}

enum ParseError {
    CouldNotParse
}

impl Parser {

    // expr1 -> expr0 + expr1
    // expr1 -> expr0 - expr1
    // expr1 -> expr0

    // expr0 -> num / expr0
    // expr0 -> num * expr0
    // expr0 -> (expr1) / (expr1)
    // expr0 -> (expr1) * (expr1)
    // expr0 -> num

    fn new(tokens: Vec<Token>) -> Self{
        Parser { pointer: Pointer::from(tokens) }
    }

    fn remove_whitespace(pointer: Pointer<Token>) -> Pointer<Token> {
        let pointer = pointer
            .next_while(|token| match token {
                Token::Whitespace => true,
                _ => false
            });
        return pointer;
    }

    fn is_plus_token(pointer: &Pointer<Token>) -> bool {
        let token = match pointer.peek() {
            Ok(token) => token,
            Err(_) => return false
        };
        match token {
            Token::Plus => true,
            _ => false
        }
    }

    fn is_minus_token(pointer: &Pointer<Token>) -> bool {
        let token = match pointer.peek() {
            Ok(token) => token,
            Err(_) => return false
        };
        match token {
            Token::Minus => true,
            _ => false
        }
    }

    fn parse_add_expr(&mut self, pointer: Pointer<Token>) -> Result<(ParseNode, Pointer<Token>), ParseError> {
        let mut pointer = pointer;
        pointer = Parser::remove_whitespace(pointer);

        let left_tree = match self.parse_expr0(pointer.clone()) {
            Ok((node, new_pointer)) => {
                pointer = new_pointer;
                node
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        pointer = pointer.next();
        pointer = Parser::remove_whitespace(pointer);

        if !Parser::is_plus_token(&pointer) {
            return Err(ParseError::CouldNotParse);
        }

        pointer = pointer.next();

        let node = match self.parse_expr1(pointer.clone()) {
            Ok((node, new_pointer)) => {
                pointer = new_pointer;
                node
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        return Ok((ParseNode::Add {
            left: Rc::new(left_tree),
            right: Rc::new(node)
        }, pointer));

    }

    fn parse_sub_expr(&mut self, pointer: Pointer<Token>) -> Result<(ParseNode, Pointer<Token>), ParseError> {
        let mut pointer = pointer;
        pointer = Parser::remove_whitespace(pointer);

        let left_tree = match self.parse_expr0(pointer.clone()) {
            Ok((node, new_pointer)) => {
                pointer = new_pointer;
                node
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        pointer = pointer.next();
        pointer = Parser::remove_whitespace(pointer);

        if !Parser::is_minus_token(&pointer) {
            return Err(ParseError::CouldNotParse);
        }

        pointer = pointer.next();

        let node = match self.parse_expr1(pointer.clone()) {
            Ok((node, new_pointer)) => {
                pointer = new_pointer;
                node
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        return Ok((ParseNode::Subtract {
            left: Rc::new(left_tree),
            right: Rc::new(node)
        }, pointer));

    }

    fn parse_number(&mut self, pointer: Pointer<Token>) -> Result<(ParseNode, Pointer<Token>), ParseError> {
        let mut pointer = pointer;
        pointer = Parser::remove_whitespace(pointer);

        let token = match pointer.peek() {
            Ok(tok) => tok,
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        let num = match token {
            Token::Number(num) => num,
            _ => return Err(ParseError::CouldNotParse)
        };

        return Ok((ParseNode::NumberNode(num), pointer.clone()));
    }

    fn parse_expr1(&mut self, pointer: Pointer<Token>) -> Result<(ParseNode, Pointer<Token>), ParseError> {
        match self.parse_add_expr(pointer.clone()) {
            Ok(res) => return Ok(res),
            Err(_) => ()
        };
        match self.parse_sub_expr(pointer.clone()) {
            Ok(res) => return Ok(res),
            Err(_) => ()
        };
        match self.parse_expr0(pointer.clone()) {
            Ok(res) => return Ok(res),
            Err(_) => ()
        };
        return Err(ParseError::CouldNotParse);
    }

    fn parse_div_expr(&mut self, pointer: Pointer<Token>) -> Result<(ParseNode, Pointer<Token>), ParseError> {

        let mut pointer = pointer;
        
        let number_node = match self.parse_number(pointer.clone()) {
            Ok((node, new_pointer)) => {
                pointer = new_pointer;
                node
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        pointer = Parser::remove_whitespace(pointer.next());

        match pointer.peek() {
            Ok(token) => match token {
                Token::Divide => (),
                _ => return Err(ParseError::CouldNotParse)
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };
        pointer = pointer.next();

        let node = match self.parse_expr0(pointer.clone()) {
            Ok((node, new_pointer)) => {
                pointer = new_pointer;
                node
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        return Ok((ParseNode::DivideNode { 
            left: Rc::new(number_node),
            right: Rc::new(node)
        }, pointer));
    }

    fn parse_mult_expr(&mut self, pointer: Pointer<Token>) -> Result<(ParseNode, Pointer<Token>), ParseError> {
        let mut pointer = Parser::remove_whitespace(pointer);
        
        let number_node = match self.parse_number(pointer.clone()) {
            Ok((node, new_pointer)) => {
                pointer = new_pointer;
                node
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        pointer = Parser::remove_whitespace(pointer.next());

        match pointer.peek() {
            Ok(token) => match token {
                Token::Multiply => (),
                _ => return Err(ParseError::CouldNotParse)
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        pointer = pointer.next();

        let node = match self.parse_expr0(pointer.clone()) {
            Ok((node, new_pointer)) => {
                pointer = new_pointer;
                node
            },
            Err(_) => return Err(ParseError::CouldNotParse)
        };

        return Ok((ParseNode::Multiply { 
            left: Rc::new(number_node),
            right: Rc::new(node)
        }, pointer));
    }

    fn parse_expr0(&mut self, pointer: Pointer<Token>) -> Result<(ParseNode, Pointer<Token>), ParseError> {
        match self.parse_div_expr(pointer.clone()) {
            Ok(res) => return Ok(res),
            Err(_) => ()
        };

        match self.parse_mult_expr(pointer.clone()) {
            Ok(res) => return Ok(res),
            Err(_) => ()
        };
        match self.parse_number(pointer.clone()) {
            Ok(res) => return Ok(res),
            Err(_) => ()
        };
        return Err(ParseError::CouldNotParse);
    }

    fn parse(&mut self) -> Result<ParseNode, ParseError> {
        let pointer = self.pointer.clone();
        return match self.parse_expr1(pointer.clone()) {
            Ok((node, _)) => Ok(node),
            Err(e) => Err(e)
        };
    }
}

fn command_line() {
    print!("> ");
    io::Write::flush(&mut io::stdout()).expect("flush failed");
    let mut line = String::new();

    io::stdin()
        .read_line(&mut line)
        .expect("Failed to read line");
    
    // let line = line.trim();
    // println!("{}", line.len());

    let mut lexer = Lexer::new(line.as_str());
    let res = lexer.lex();

    match res {
        Ok(res) => {
            println!("--- Tokens ---");
            for token in &res {
                println!("- {:?}", token);
            }
            let mut parser = Parser::new(res);
            match parser.parse() {
                Ok(res) => {
                    // println!("{:?}", res);
                    res.pretty_print();
                },
                Err(_) => ()
            }

        },
        Err(_) => ()
    }
}

fn main() {
    loop {
        command_line();
    }
}
