grammar Cb;
@lexer::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
}
@listener::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
}
@parser::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
use std::collections::HashMap;
use crate::ast::types::Type;
use crate::ast::scope::Scope;
use crate::ast::scope::SubScope;
use crate::ast::types;
use std::fmt::{Formatter, Debug, Display, self};
use crate::parser::errors::ParserError;
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
topDef:
	funcDef
	| funcDecl
	| varDef
	| constDef
	| structDef ;
	// TODO: support unionDef TODO: support typeDef;
varDef:
	s = storage t = typeName name {
		let text = &$name.text;
		let start = $name.start.unwrap().deref().start as usize;
		let stop = $name.stop.unwrap().deref().stop as usize;
		let t = $t.v.clone();
		let result = recog.scope.defineVariable(
			text, 
			start..stop + 1, 
			t
		);
		if result.is_err() {
			let name = $name.text.to_string();
			let err = result.unwrap_err();
			let err = ANTLRError::FallThrough(Rc::new(err));
			recog.notify_error_listeners(
				format!("Variable {} is defined twice", name),
				// last token
				Some(recog.base.input.index() - 1),
				Some(&err)
			);
			return Err(err);
		}
	} ('=' init = expr)? (
		',' name ('=' init = expr)?
	)* ';';
constDef: CONST t = typeName name '=' value = expr ';';
funcDef:
	s = storage ret = typeName name '(' p = params ')' body = block;
funcDecl: EXTERN ret = typeName name '(' p = paramsDecl ')' ';';
storage: STATIC?;
params: VOID | param (',' param)* (',' '...')?;
param: t = typeName name;
paramsDecl: VOID | paramDecl (',' paramDecl)* (',' '...')?;
paramDecl: t = typeName;
block returns [Option<Rc<RefCell<SubScope>>> scope]: '{' {$scope = Some(recog.scope.push());} defvarList stmts '}' {recog.scope.pop();};
defvarList: vars = varDef*;
structDef:
	STRUCT name memberList ';' {
	let name = $name.text.to_owned();
	let selfType = types::StructType{name, fields: $memberList.v.clone()};
	let name = $name.text.to_owned();
    recog.registerType(name, selfType.into());
};
// unionDef: UNION name memberList ';' { let name = $name.text.to_owned(); let selfType =
// types::UnionType{name, fields: $memberList.v}; recog.registerType(name, selfType.into()); };
memberList
	returns[Vec<(String, types::Type)> v]:
	'{' (m = member {
		let mut vclone = (&$v).clone();
		vclone.push($m.v.clone());
		$v = vclone;
	} ';')* '}' {
	
};
member
	returns[(String, types::Type) v]:
	t = typeName name {
	$v = ($name.text.to_owned(), $t.v.to_owned());
};
// typeDef: TYPEDEF typeName name ';' { let name = $name.text.to_owned(); let selfType =
// type::Type::Named( type::Type::NamedType( type::Type::TypeDefType(name), ) )
// recog.registerType(name); };
typeName
	returns[types::Type v]:
	typeBase {$v = $typeBase.v;} (
		'[' ']' {
			$v = Type::from(types::PointerType{element_type: Box::new((&$v).to_owned())});
		}
		| '[' INTEGER ']' {
			$v = Type::from(types::ArrayType{element_type: Box::new((&$v).to_owned()), size: str::parse::<usize>($INTEGER.text).unwrap()});
		}
		| '*' {
			$v = Type::from(types::PointerType{element_type: Box::new((&$v).to_owned())});
		}
		| '(' paramtypes ')' {
			$v = Type::from(types::FunctionType{
				return_type: Box::new((&$v).to_owned()),
				parameters: $paramtypes.v.0.clone(),
				variadic: $paramtypes.v.1,
			});
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
		$v = types::Type::Integer(types::IntegerType{size: 8, signed: true});
	}
	| SHORT {
		$v = types::Type::Integer(types::IntegerType{size: 16, signed: true});
	}
	| INT {
		$v = types::Type::Integer(types::IntegerType{size: 32, signed: true});
	}
	| LONG {
		$v = types::Type::Integer(types::IntegerType{size: 64, signed: true});
	}
	| UNSIGNED CHAR {
		$v = types::Type::Integer(types::IntegerType{size: 8, signed: false});
	}
	| UNSIGNED SHORT {
		$v = types::Type::Integer(types::IntegerType{size: 16, signed: false});
	}
	| UNSIGNED INT {
		$v = types::Type::Integer(types::IntegerType{size: 32, signed: false});
	}
	| UNSIGNED LONG {
		$v = types::Type::Integer(types::IntegerType{size: 64, signed: false});
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
		$v = types::Type::Integer(types::IntegerType{size: 8, signed: true});
	}
	| SHORT {
		$v = types::Type::Integer(types::IntegerType{size: 16, signed: true});
	}
	| INT {
		$v = types::Type::Integer(types::IntegerType{size: 32, signed: true});
	}
	| LONG {
		$v = types::Type::Integer(types::IntegerType{size: 64, signed: true});
	}
	| UNSIGNED CHAR {
		$v = types::Type::Integer(types::IntegerType{size: 8, signed: false});
	}
	| UNSIGNED SHORT {
		$v = types::Type::Integer(types::IntegerType{size: 16, signed: false});
	}
	| UNSIGNED INT {
		$v = types::Type::Integer(types::IntegerType{size: 32, signed: false});
	}
	| UNSIGNED LONG {
		$v = types::Type::Integer(types::IntegerType{size: 64, signed: false});
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
expr: assignmentExpr | <assoc = right> expr ',' expr;
args: (assignmentExpr (',' assignmentExpr)*)?;
primary
	locals[
		types::Type t,
	]:
	INTEGER {
		$t = types::Type::Integer(
			types::IntegerType{
				signed: true,
				size: 64,
			}
		);
	}
	| CHAR_LITERAL {
		$t = types::Type::Integer(
			types::IntegerType{
				signed: true,
				size: 8,
			}
		);
	}
	| STRING_LITERAL {
		$t = types::Type::Pointer(
				types::Type::Integer(
				types::IntegerType{
					signed: true,
					size: 8,
				}
			).pointer_type()
		);
	}
	| IDENTIFIER {
		
	}
	| '(' expr ')';
