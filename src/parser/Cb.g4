grammar Cb;


LINE_COMMENT: '//' .*? '\r'? '\n' -> skip;

// Allow recursive block comments
BLOCK_COMMENT:
	'/*' (BLOCK_COMMENT | .)*? '*/' -> skip;

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


// compile unit
compUnit: top_def+ EOF;
name: IDENTIFIER;
top_def:
	func_def
	| var_def
	| const_def
	| struct_def
	| union_def
	| type_def;
var_def:
	s = storage t = type n = name ('=' init = expr)? (
		',' n = name ('=' init = expr)?
	)* ';';
const_def: CONST t = type n = name '=' value = expr ';';
func_def:
	s = storage ret = typeRef n = name '(' p = params ')' body = block;
storage: STATIC?;
params: VOID | param (',' param)* (',' '...')?;
param: t = type n = name;
block: '{' defvar_list stmts '}';
defvar_list: vars = var_def*;
struct_def: STRUCT n = name members = member_list ';';
union_def: UNION n = name members = member_list ';';
member_list: '{' (member ';')* '}';
member: t = type n = name;
type_def: TYPEDEF typeRef IDENTIFIER ';';
type: typeRef;
typeRef:
	typeRefBase (
		'[' ']'
		| '[' INTEGER ']'
		| '*'
		| '(' paramTypeRefs ')'
	)*;
paramTypeRefs: VOID | paramTypeRef+ (',' '...')?;
paramTypeRef: n = name;
typeRefBase:
	VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| UNSIGNED CHAR
	| UNSIGNED SHORT
	| UNSIGNED INT
	| UNSIGNED LONG
	| STRUCT n = IDENTIFIER
	| UNION n = IDENTIFIER
	| {isType()}? IDENTIFIER;
stmts: stmt*;
stmt:
	';'
	| labeled_stmt
	| expr ';'
	| block
	| if_stmt
	| while_stmt
	| dowhile_stmt
	| for_stmt
	| switch_stmt
	| break_stmt
	| continue_stmt
	| goto_stmt
	| return_stmt;
labeled_stmt: n = IDENTIFIER ':' stmt;
if_stmt:
	IF '(' cond = expr ')' thenStmt = stmt (ELSE elseStmt = stmt)?;
while_stmt: WHILE '(' cond = expr ')' body = stmt;
dowhile_stmt: DO body = stmt WHILE '(' cond = expr ')';
for_stmt:
	FOR '(' init = expr ';' cond = expr ';' inc = expr ')' body = stmt;
switch_stmt: SWITCH '(' cond = expr ')' '{' case_clauses '}';
case_clauses: case_clause* default_clause?;
case_clause: values = cases () body = case_body;
cases: (CASE primary ':')+;
default_clause: DEFAULT ':' body = case_body;
case_body: (stmt)+;
goto_stmt: GOTO n = IDENTIFIER ';';
break_stmt: BREAK ';';
continue_stmt: CONTINUE ';';
return_stmt: RETURN expr? ';';
expr:
    // post
	primary (
		'++'
		| '--'
		| '[' expr ']'
		| '.' IDENTIFIER
		| '->' IDENTIFIER  // member access
		| '(' a = args ')' // func call
	)* # postfixOp
	| 
    // unary
    <assoc=right> ('++' | '--' | '+' | '-' | '!' | '~' | '*' | '&') expr # unaryOp
    | (SIZEOF '(' type ')') # sizeofType
    | (SIZEOF expr) # sizeofExpr
    // cast
	| '(' type ')' expr	# castOp
    // binary
    | expr ('*' | '/' | '%') expr # mulDiv
    | expr ('+' | '-') expr # addSub
    | expr ('<<' | '>>') expr # shift
    | expr ('&' expr)+ # and
    | expr ('^') expr # xor
    | expr ('|') expr # or
    // relational
    | expr ('==' | '!=' | '>' | '>=' | '<' | '<=') expr # rel
    // logical
    | expr ('&&') expr # logicalAnd
    | expr ('||') expr # logicalOr
    // ternary
    | expr ('?' expr ':' expr) # ternary
    // assignment
    | <assoc=right> expr ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=') expr # assign
    | expr ',' expr # comma
;
args: (expr (',' expr)*)?;
primary:
	INTEGER
	| CHAR_LITERAL
	| STRING_LITERAL
	| IDENTIFIER
	| '(' expr ')';
