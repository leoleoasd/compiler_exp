grammar Cb;
@lexer::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(unused_macros)]
}
@listener::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(unused_macros)]
}
@parser::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(unused_macros)]
use std::collections::HashMap;
use crate::ast::types::Type;
use crate::ast::scope::Scope;
use crate::ast::scope::SubScope;
use crate::ast::types;
use crate::ast::node::Node;
use crate::ast::expr::*;
use std::ops::Range;
use std::fmt::{Formatter, Debug, Display, self};
use crate::parser::errors::ParserError;
use std::sync::atomic::{AtomicIsize, Ordering};
use quick_error::ResultExt;
macro_rules! location_for_ctx {
	(\$token:expr) => {
		\$token.start().get_start() as usize .. \$token.stop().get_stop() as usize + 1
	};
}
macro_rules! location_for_token {
	(\$token:expr) => {
		\$token.start as usize .. \$token.stop as usize + 1
	};
}
macro_rules! report_or_unwrap {
	(\$result:expr, \$recog: ident) => {
		match \$result {
			Ok(result) => result,
			Err(err) => {
				let err = ANTLRError::from(err);
				\$recog.notify_error_listeners(
					"".to_string(),
					// last token
					Some(\$recog.base.input.index() - 1),
					Some(&err)
				);
				return Err(err)
			},
		}
	};
	(\$result:expr, \$recog: ident, \$location: expr) => {
		match \$result {
			Ok(result) => result,
			Err(err) => {
				let err = ANTLRError::from(err);
				\$recog.notify_error_listeners(
					"".to_string(),
					// last token
					Some(\$location),
					Some(&err)
				);
				return Err(err)
			},
		}
	};
}
}
@parser::fields {
    pub types: HashMap<String, types::Type>,
	pub scope: Scope,
}
@parser::init {
    types: HashMap::default(),
	scope: Scope::new(),
}
@parser::members {
	fn registerType(&mut self, name: String, t: types::Type) {
		println!("Registering type {}", name);
		println!("{:?}", t);
		self.types.insert(name, t);
	}
	fn getType(&self, name: &str) -> Option<&types::Type> {
		println!("Getting type {}: {:?}", name, self.types.get(name));
		self.types.get(name)
	}
    fn isType(&self, name: &TokenType) -> bool {
        let t = &name.text;
        match t {
            std::borrow::Cow::Owned(s) => self.types.contains_key(s),
            std::borrow::Cow::Borrowed(s) => self.types.contains_key(*s),
        }
    }
}

LINE_COMMENT: '//' .*? '\r'? '\n' -> skip;

// Allow recursive block comments
BLOCK_COMMENT: '/*' (BLOCK_COMMENT | .)*? '*/' -> skip;

SPACES: ('\n' | '\r' | '\t' | ' ')+ -> skip;

VOID: 'void';
CHAR: 'char';
SHORT: 'short';
INT: 'int';
LONG: 'long';
STRUCT: 'struct';
UNION: 'union';
ENUM: 'enum';
STATIC: 'static';
EXTERN: 'extern';
CONST: 'const';
SIGNED: 'signed';
UNSIGNED: 'unsigned';
IF: 'if';
ELSE: 'else';
SWITCH: 'switch';
CASE: 'case';
DEFAULT: 'default';
WHILE: 'while';
DO: 'do';
FOR: 'for';
RETURN: 'return';
BREAK: 'break';
CONTINUE: 'continue';
GOTO: 'goto';
TYPEDEF: 'typedef';
SIZEOF: 'sizeof';

IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

// Literals
INTEGER: INT10 | INT8 | INT16;
REAL: REAL10 | REAL8 | REAL16;

fragment INT10: '0' | [1-9]DIGIT10*;
fragment INT8: '0' [1-7]DIGIT8*;
fragment INT16: '0x' [1-9a-fA-F]DIGIT16*;

fragment REAL10: INT10 '.' DIGIT10*;
fragment REAL8: INT8 '.' DIGIT8*;
fragment REAL16: INT16 '.' DIGIT16*;

fragment DIGIT10: [0-9];
fragment DIGIT16: [0-9a-fA-F];
fragment DIGIT8: [0-7];

CHAR_LITERAL: '\'' CCHAR '\'';
STRING_LITERAL: '"' SCHAR*? '"';
fragment CCHAR: ~["\\\r\n] | ESCAPE;
fragment SCHAR: ~["\\\r\n] | ESCAPE | '\\\n' | '\\\r\n';
fragment ESCAPE: '\\' ['"?abfnrtv\\];

// Parser compile unit
compUnit: topDef+ EOF;
name: IDENTIFIER;
topDef: funcDef | funcDecl | varDef | constDef | structDef;
// TODO: support unionDef TODO: support typeDef;
varDef:
	s = storage t = typeName name {
		let text = &$name.text; 
		let t = $t.v.clone();
		let result = recog.scope.define_variable(
			text, 
			location_for_ctx!(&$name.ctx),
			t
		);
		report_or_unwrap!(result, recog);
	} ('=' init = expr)? (',' name ('=' init = expr)?)* ';';
constDef: CONST t = typeName name '=' value = expr ';';
funcDef:
	s = storage ret = typeName name '(' params ')' body = block {
		let ret_type = $ret.v;
		let (param, variadic) = $params.v.borrow().clone();
		let func_type = ret_type.function_type(
			param,
			variadic
		);
		let text = &$name.text;
		let location = $s.ctx.start().get_start() as usize .. $body.ctx.start().get_start() as usize;
		let result = recog.scope.define_function(
			text,
			location,
			func_type,
			false
		);
		let index = $name.ctx.start().token_index.load(Ordering::Relaxed);
		report_or_unwrap!(result, recog, index);
	};
funcDecl:
	EXTERN ret = typeName name '(' paramsDecl ')' ';' {
		let ret_type = $ret.v;
		let (param, variadic) = $paramsDecl.v.borrow().clone();
		let func_type = ret_type.function_type(
			param,
			variadic
		);
		let text = &$name.text;
		let location = $start.start as usize .. $stop.stop as usize;
		let result = recog.scope.define_function(
			text,
			location,
			func_type,
			false
		);
		let index = $name.ctx.start().token_index.load(Ordering::Relaxed);
		report_or_unwrap!(result, recog, index);
};
storage: STATIC?;
params
	returns[RefCell<(Vec<types::Type>, bool)> v]:
	VOID
	| param {
	(&$v).borrow_mut().0.push($param.v.1.clone());
} (
		',' param {
	(&$v).borrow_mut().0.push($param.v.1.clone());
}
	)* (
		',' '...' {
	(&$v).borrow_mut().1 = true;
}
	)?;
param
	returns[(String, Type) v]:
	t = typeName name {
	$v = ($name.text.to_string(), $t.v.to_owned());
};
paramsDecl
	returns[RefCell<(Vec<types::Type>, bool)> v]:
	VOID
	| paramDecl {
	(&$v).borrow_mut().0.push($paramDecl.v.clone());
} (
		',' paramDecl {
	(&$v).borrow_mut().0.push($paramDecl.v.clone());
}
	)* (
		',' '...' {
	(&$v).borrow_mut().1 = true;
}
	)?;
paramDecl
	returns[Type v]:
	t = typeName {
	$v = $t.v.to_owned();
};
block
	returns[Option<Rc<RefCell<SubScope>>> scope]:
	'{' {$scope = Some(recog.scope.push());} defvarList stmts '}' {recog.scope.pop();};
defvarList: vars = varDef*;
structDef:
	STRUCT name memberList ';' {
	let name = $name.text.to_owned();
	let selfType = Type::Struct{name, fields: $memberList.v.borrow().clone().into_iter().map(|x| (x.0, x.1)).collect()};
	let name = $name.text.to_owned();
    recog.registerType(name, selfType.into());
};
// unionDef: UNION name memberList ';' { let name = $name.text.to_owned(); let selfType =
// types::UnionType{name, fields: $memberList.v}; recog.registerType(name, selfType.into()); };
memberList
	returns[RefCell<Vec<(String, types::Type, Range<usize>)>> v]:
	'{' (
		m = member {
		let v = $m.v.clone();
		report_or_unwrap!((&$v).borrow().iter().try_for_each(|x| -> Result<(), ParserError> {
			if x.0 == v.0 {
				Err(ParserError::DuplicateStructField(
					v.0.to_string(),
					x.2.clone()
				))
			} else {
				Ok(())
			}
		}), recog);
		(&$v).borrow_mut().push(v);
	} ';'
	)* '}' {
	
};
member
	returns[(String, types::Type, Range<usize>) v]:
	t = typeName name {
	$v = ($name.text.to_owned(), $t.v.to_owned(), location_for_ctx!(&$name.ctx));
};
// typeDef: TYPEDEF typeName name ';' { let name = $name.text.to_owned(); let selfType =
// type::Type::Named( type::Type::NamedType( type::Type::TypeDefType(name), ) )
// recog.registerType(name); };
typeName
	returns[types::Type v]:
	typeBase {$v = $typeBase.v;} (
		'[' ']' {
			$v = Type::Pointer{element_type: Box::new((&$v).to_owned())};
		}
		| '[' INTEGER ']' {
			$v = Type::Array{element_type: Box::new((&$v).to_owned()), size: str::parse::<usize>($INTEGER.text).unwrap()};
		}
		| '*' {
			$v = Type::Pointer{element_type: Box::new((&$v).to_owned())};
		}
		| '(' paramtypes ')' {
			$v = Type::Function{
				return_type: Box::new((&$v).to_owned()),
				parameters: $paramtypes.v.0.clone(),
				variadic: $paramtypes.v.1,
			};
		}
	)*;
paramtypes
	returns[(Vec<types::Type>, bool) v]
	locals[bool variadic, Vec<Type> types]:
	VOID {
	$v = (vec![], false);
}
	| (
		paramtype {
	let tt = $paramtype.v.clone();
	let mut ttt = (&$types).clone();
	ttt.push(tt);
	$types = ttt;
}
	)+ (
		',' '...' {
	$variadic = true;
	}
	)? {
	$v = ((&$types).clone(), $variadic);
};
paramtype
	returns[types::Type v]:
	CHAR {
		$v = types::Type::Integer{size: 8, signed: true};
	}
	| SHORT {
		$v = types::Type::Integer{size: 16, signed: true};
	}
	| INT {
		$v = types::Type::Integer{size: 32, signed: true};
	}
	| LONG {
		$v = types::Type::Integer{size: 64, signed: true};
	}
	| UNSIGNED CHAR {
		$v = types::Type::Integer{size: 8, signed: false};
	}
	| UNSIGNED SHORT {
		$v = types::Type::Integer{size: 16, signed: false};
	}
	| UNSIGNED INT {
		$v = types::Type::Integer{size: 32, signed: false};
	}
	| UNSIGNED LONG {
		$v = types::Type::Integer{size: 64, signed: false};
	}
	| STRUCT n = IDENTIFIER {
		let t = match recog.getType(&$n.text) {
			Some(t) => t.clone(),
			None => {
				let name = (&$n.text);
				recog.notify_error_listeners(
					format!("Type struct {} not found", name),
					// last token
					Some(recog.base.input.index() - 1),
					None
				);
				return Err(
					ANTLRError::FallThrough(Rc::new(
						ParserError::TypeNotFound(name.to_string())
					))
				);
			},
		};
		$v = t;
	};
typeBase
	returns[types::Type v]:
	VOID {
		$v = types::Type::Void;
	}
	| CHAR {
		$v = types::Type::Integer{size: 8, signed: true};
	}
	| SHORT {
		$v = types::Type::Integer{size: 16, signed: true};
	}
	| INT {
		$v = types::Type::Integer{size: 32, signed: true};
	}
	| LONG {
		$v = types::Type::Integer{size: 64, signed: true};
	}
	| UNSIGNED CHAR {
		$v = types::Type::Integer{size: 8, signed: false};
	}
	| UNSIGNED SHORT {
		$v = types::Type::Integer{size: 16, signed: false};
	}
	| UNSIGNED INT {
		$v = types::Type::Integer{size: 32, signed: false};
	}
	| UNSIGNED LONG {
		$v = types::Type::Integer{size: 64, signed: false};
	}
	| STRUCT n = IDENTIFIER {
		let t = match recog.getType(&$n.text) {
			Some(t) => t.clone(),
			None => {
				let name = (&$n.text);
				recog.notify_error_listeners(
					format!("Type struct {} not found", name),
					// last token
					Some(recog.base.input.index() - 1),
					None
				);
				return Err(
					ANTLRError::FallThrough(Rc::new(
						ParserError::TypeNotFound(name.to_string())
					))
				);
			},
		};
		$v = t;
	};
// | {recog.isType(recog.get_current_token())}? IDENTIFIER;
stmts: stmt*;
stmt:
	';'
	| labeledStmt
	| expr ';'
	| block
	| ifStmt
	| whileStmt
	| dowhileStmt
	| forStmt
	| switchStmt
	| breakStmt
	| continueStmt
	| gotoStmt
	| returnStmt;
labeledStmt: IDENTIFIER ':' stmt;
ifStmt:
	IF '(' cond = expr ')' thenStmt = stmt (ELSE elseStmt = stmt)?;
whileStmt: WHILE '(' cond = expr ')' body = stmt;
dowhileStmt: DO body = stmt WHILE '(' cond = expr ')';
forStmt:
	FOR '(' init = expr ';' cond = expr ';' inc = expr ')' body = stmt;
switchStmt: SWITCH '(' cond = expr ')' '{' caseClauses '}';
caseClauses: caseClause* defaultClause?;
caseClause: values = cases () body = caseBody;
cases: (CASE primary ':')+;
defaultClause: DEFAULT ':' body = caseBody;
caseBody: (stmt)+;
gotoStmt: GOTO IDENTIFIER ';';
breakStmt: BREAK ';';
continueStmt: CONTINUE ';';
returnStmt: RETURN expr? ';';
assignmentExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]
	locals[
    bool hasAddress,
    bool valueType,
]:
	// post
	primary (
		'++'
		| '--'
		| '[' expr ']'
		| '.' IDENTIFIER
		| '->' IDENTIFIER // member access
		| '(' args ')' // func call
	)* # postfixOp
	|
	// unary
	('++' | '--' | '+' | '-' | '!' | '~' | '*' | '&') assignmentExpr	# unaryOp
	| (SIZEOF '(' typeName ')')											# sizeofType
	| (SIZEOF assignmentExpr)											# sizeofExpr
	// cast
	| '(' typeName ')' assignmentExpr # castOp
	// binary
	| assignmentExpr ('*' | '/' | '%') assignmentExpr	# mulDiv
	| assignmentExpr ('+' | '-') assignmentExpr			# addSub
	| assignmentExpr ('<<' | '>>') assignmentExpr		# shift
	| assignmentExpr '&' assignmentExpr					# and
	| assignmentExpr '^' assignmentExpr					# xor
	| assignmentExpr '|' assignmentExpr					# or
	// relational
	| assignmentExpr ('==' | '!=' | '>' | '>=' | '<' | '<=') assignmentExpr # rel
	// logical
	| assignmentExpr ('&&') assignmentExpr	# logicalAnd
	| assignmentExpr ('||') assignmentExpr	# logicalOr
	// ternary
	| <assoc = right> assignmentExpr (
		'?' expr ':' assignmentExpr
	) # ternary
	// assignment
	| <assoc = right> assignmentExpr (
		'='
		| '*='
		| '/='
		| '%='
		| '+='
		| '-='
		| '<<='
		| '>>='
		| '&='
		| '^='
		| '|='
	) assignmentExpr # assign;
expr
	returns[
		Option<Box<dyn ExprNode>> e
	]: assignmentExpr {
		$e = $assignmentExpr.e;
	} | <assoc = right> expr ',' expr {
		todo!();
	};
args: (assignmentExpr (',' assignmentExpr)*)?;
primary
	returns[
		Option<Box<dyn ExprNode>> e
	]:
	i = INTEGER {
		let text = $i.text;
		let num = if text == "0" {0} else {
			if text.starts_with("0x") {
				i32::from_str_radix(&text[2..], 16).unwrap()
			} else if text.starts_with("0") {
				i32::from_str_radix(&text[1..], 8).unwrap()
			} else {
				i32::from_str_radix(&text, 10).unwrap()
			}
		};
		$e = Some(Box::new(IntegerLiteralNode::new(num, location_for_token!(
			recog.get_current_token()
		))) as Box<dyn ExprNode>);
	}
	| i = CHAR_LITERAL {
		let text = $i.text;
		let text = &text[1..text.len() - 1];
		let text = "\"".to_string() + text + "\"";
		let text = snailquote::unescape(&text).map_err(|err| {
			ParserError::from(quick_error::Context(text.to_string(), err))
		});
		let text = report_or_unwrap!(text, recog);
		assert_eq!(text.chars().count(), 1);
		let ch = text.chars().next().unwrap();
		if !ch.is_ascii() {
			let err = ANTLRError::from(
				ParserError::InvalidCharacterLiteral(text.to_string())
			);
			recog.notify_error_listeners(
				"".to_string(),
				// last token
				Some(recog.base.input.index() - 1),
				Some(&err)
			);
			return Err(err);
		}
		$e = Some(Box::new(CharLiteralNode::new(ch as i8, location_for_token!(
			recog.get_current_token()
		))) as Box<dyn ExprNode>);
	}
	| i = STRING_LITERAL {
		let text = $i.text;
		let text = snailquote::unescape(&text).map_err(|err| {
			ParserError::from(quick_error::Context(text.to_string(), err))
		});
		let text = report_or_unwrap!(text, recog);
		$e = Some(Box::new(StringLiteralNode::new(text, location_for_token!(
			recog.get_current_token()
		))) as Box<dyn ExprNode>);
	}
	| i = IDENTIFIER {
		// variable or function
		let name = $i.text;
		let entity = recog.scope.get(name).ok_or_else(|| ParserError::VariableUndefined(name.to_string()));
		let entity = report_or_unwrap!(entity, recog);
	}
	| '(' expr ')' {
		$e = $expr.e;
	};
