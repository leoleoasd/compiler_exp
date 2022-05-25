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
use crate::ast::types;
}
@parser::fields {
    pub types: HashMap<String, types::Type>,
}
@parser::init {
    types: HashMap::default(),
}
@parser::members {
	fn registerType(&mut self, name: String, t: types::Type) {
		self.types.insert(name, t);
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
	| structDef
	// TODO: support unionDef TODO: support typeDef;
varDef:
	s = storage t = typeName name ('=' init = expr)? (
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
block: '{' defvarList stmts '}';
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
	'{' (m = member ';')* '}' {
	
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
	typeBase (
		'[' ']' {
			$v = Type::from(types::PointerType{element_type: Box::new($typeBase.v.to_owned())});
		}
		| '[' INTEGER ']' {
			$v = Type::from(types::ArrayType{element_type: Box::new($typeBase.v.to_owned()), size: str::parse::<usize>($INTEGER.text).unwrap()});
		}
		| '*' {
			$v = Type::from(types::PointerType{element_type: Box::new($typeBase.v.to_owned())});
		}
		| '(' paramtypes ')' {
			$v = Type::from(types::FunctionType{
				return_type: Box::new($typeBase.v.to_owned()),
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
		todo!();
	}
	| UNION n = IDENTIFIER {
		todo!();
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
		todo!();
	}
	| UNION n = IDENTIFIER {
		todo!();
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
