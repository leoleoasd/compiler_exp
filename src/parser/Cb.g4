grammar Cb;
@lexer::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(unused_macros)]
#![allow(clippy::redundant_static_lifetimes)]
#![allow(clippy::identity_op)]
#![allow(clippy::redundant_clone)]
}
@listener::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(unused_macros)]
#![allow(clippy::redundant_static_lifetimes)]
#![allow(clippy::identity_op)]
#![allow(clippy::redundant_clone)]
}
@parser::header {
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(unused_macros)]
#![allow(clippy::redundant_static_lifetimes)]
#![allow(clippy::identity_op)]
#![allow(clippy::redundant_clone)]
#![allow(clippy::type_complexity)]
#![allow(clippy::or_fun_call)]
#![allow(clippy::nonminimal_bool)]
#![allow(clippy::new_ret_no_self)]
#![allow(clippy::tabs_in_doc_comments)]
#![allow(clippy::double_parens)]
use std::collections::HashMap;
use crate::ast::scope::Scope;
use crate::ast::scope::SubScope;
use crate::ast::scope::Entity;
use crate::ast::types::Type;
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
    pub types: HashMap<String, Arc<Type>>,
	pub scope: Scope,
}
@parser::init {
    types: HashMap::default(),
	scope: Scope::new(),
}
@parser::members {
	fn registerType(&mut self, name: String, t: Arc<Type>) -> Result<(), ParserError> {
		let r = self.types.insert(name.clone(), t.clone());
		match r {
			None=> Ok(()),
			Some(old) => {
				if let Type::Struct{ location, .. } = &*old{
					Err(ParserError::TypeNameConflict(
						name,
						location.clone()
					))
				} else {
					unreachable!();
				}
			}
		}
	}
	fn getType(&self, name: &str) -> Option<Arc<Type>> {
		self.types.get(name).cloned()
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
topDef: funcDef | funcDecl | varDef | structDef;
// TODO: support unionDef TODO: support typeDef;
varDef locals [
	Option<Box<dyn ExprNode>> init_expr
]:
	s = storage t = typeName name ('=' init = assignmentExpr {
		$init_expr = $init.e;
	}) ? {
		let text = &$name.text; 
		let t = $t.v.clone();
		// if (&$init_expr).is_some() {
		// 	if !(&$init_expr).as_ref().unwrap().is_constant() {
		// 		report_or_unwrap!(Err(
		// 			ParserError::ConstantExpressionRequired((&$init_expr).as_ref().unwrap().get_location().clone())
		// 		), recog);
		// 	}
		// }
		let result = recog.scope.define_variable(
			text, 
			location_for_ctx!($name.ctx),
			t,
			(&$init_expr).clone()
		);
		report_or_unwrap!(result, recog);
	} (',' name ('=' init = assignmentExpr {
		$init_expr = $init.e;
	})?{
		let text = &$name.text; 
		let t = $t.v.clone();
		let result = recog.scope.define_variable(
			text, 
			location_for_ctx!($name.ctx),
			t,
			(&$init_expr).clone()
		);
		report_or_unwrap!(result, recog);
	})* ';';
// TODO: support const
constDef: CONST t = typeName name '=' value = expr ';';
funcDef returns [
	Option<Arc<RefCell<Entity>>> e,
	Option<Arc<RefCell<SubScope>>> scope
]:
	storage ret = typeName name {
		$scope = Some(recog.scope.push());
	} '(' params ')'{
		let ret_type = $ret.v;
		let (param, variadic) = $params.v.borrow().clone();
		let func_type = Type::function_type(
			ret_type.clone(),
			param,
			variadic
		);
		let text = &$name.text;
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		let result = recog.scope.define_function(
			text,
			location,
			func_type,
			false
		);
		let index = $name.ctx.start().token_index.load(Ordering::Relaxed);
		$e = Some(report_or_unwrap!(result, recog, index));
	} body = block {
		recog.scope.pop();
	};
funcDecl:
	EXTERN ret = typeName name '(' paramsDecl ')' ';' {
		let ret_type = $ret.v;
		let (param, variadic) = $paramsDecl.v.borrow().clone();
		let func_type = Type::function_type(
			ret_type.clone(),
			param,
			variadic
		);
		let text = &$name.text;
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		let result = recog.scope.define_function(
			text,
			location,
			func_type,
			true
		);
		let index = $name.ctx.start().token_index.load(Ordering::Relaxed);
		report_or_unwrap!(result, recog, index);
};
storage: STATIC?;
params
	returns[RefCell<(Vec<(String, Arc<Type>)>, bool)> v]:
	VOID
	| param {
	(&$v).borrow_mut().0.push($param.v.clone());
} (
		',' param {
	(&$v).borrow_mut().0.push($param.v.clone());
}
	)*;
param
	returns[(String, Arc<Type>) v]:
	t = typeName name {
	let t = $t.v;
	let name = $name.text;
	let result = recog.scope.define_variable(
		&name, 
		location_for_ctx!($name.ctx),
		t.clone(),
		None
	);
	report_or_unwrap!(result, recog);
	$v = (name, t.clone());
};
paramsDecl
	returns[RefCell<(Vec<(String, Arc<Type>)>, bool)> v]:
	VOID
	| paramDecl {
	(&$v).borrow_mut().0.push(("_unnamed_".to_string(), $paramDecl.v.clone()));
} (
		',' paramDecl {
	(&$v).borrow_mut().0.push(("_unnamed_".to_string(), $paramDecl.v.clone()));
}
	)* (
		',' '...' {
	(&$v).borrow_mut().1 = true;
}
	)?;
paramDecl
	returns[Arc<Type> v]:
	t = typeName {
	$v = $t.v.to_owned();
};
block
	returns[Option<Arc<RefCell<SubScope>>> scope]:
	'{' {$scope = Some(recog.scope.push());} 
	vars = defvarList body = stmts '}' {recog.scope.pop();};
defvarList: vars = varDef*;
structDef:
	STRUCT name memberList ';' {
	let name = $name.text.to_owned();
	let location = $start.start as usize .. recog.get_current_token().stop as usize - 3;
	let selfType = Arc::new(Type::Struct{
		name, 
		fields: $memberList.v.borrow().clone().into_iter().map(|x| (x.0, x.1)).collect(),
		location
	});
	report_or_unwrap!(selfType.is_legal(), recog);
	let name = $name.text.to_owned();
    report_or_unwrap!(recog.registerType(name, selfType), recog, $name.ctx.start().token_index.load(Ordering::Relaxed));
};
// unionDef: UNION name memberList ';' { let name = $name.text.to_owned(); let selfType =
// types::UnionType{name, fields: $memberList.v}; recog.registerType(name, selfType.into()); };
memberList
	returns[RefCell<Vec<(String, Arc<Type>, Range<usize>)>> v]:
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
	returns[(String, Arc<Type>, Range<usize>) v]:
	t = typeName name {
	$v = ($name.text.to_owned(), $t.v.to_owned(), location_for_ctx!($name.ctx));
};
// typeDef: TYPEDEF typeName name ';' { let name = $name.text.to_owned(); let selfType =
// type::Type::Named( type::Type::NamedType( type::Type::TypeDefType(name), ) )
// recog.registerType(name); };
typeName
	returns[Arc<Type> v]:
	typeBase {$v = $typeBase.v;} (
		'[' ']' {
			$v = Arc::new(Type::Pointer{element_type: (&$v).clone()});
		}
		| '[' INTEGER ']' {
			$v = Arc::new(Type::Array{element_type: (&$v).clone(), size: str::parse::<usize>($INTEGER.text).unwrap()});
		}
		| '*' {
			$v = Arc::new(Type::Pointer{element_type: (&$v).clone()});
		}
		// support function pointer
		// | '(' paramtypes ')' {
		// 	$v = Arc::new(Type::Function{
		// 		return_type: (&$v).clone(),
		// 		parameters: $paramtypes.v.0.clone(),
		// 		variadic: $paramtypes.v.1,
		// 	});
		// }
	)*;
paramtypes
	returns[(Vec<Arc<Type>>, bool) v]
	locals[bool variadic, Vec<Arc<Type> > types]:
	VOID {
	$v = (vec![], false);
}
	| (
		typeName {
	let tt = $typeName.v.clone();
	let mut ttt = (&$types).clone();
	ttt.push(tt);
	$types = ttt;
}
	) (
		','typeName {
		let tt = $typeName.v.clone();
		let mut ttt = (&$types).clone();
		ttt.push(tt);
		$types = ttt;
	}
	)* (
		',' '...' {
	$variadic = true;
	}
	)? {
	$v = ((&$types).clone(), $variadic);
};
typeBase
	returns[Arc<Type> v]:
	VOID {
		$v = Arc::new(Type::Void);
	}
	| CHAR {
		$v = Arc::new(Type::Integer{size: 8, signed: true});
	}
	| SHORT {
		$v = Arc::new(Type::Integer{size: 16, signed: true});
	}
	| INT {
		$v = Arc::new(Type::Integer{size: 32, signed: true});
	}
	| LONG {
		$v = Arc::new(Type::Integer{size: 64, signed: true});
	}
	| UNSIGNED CHAR {
		$v = Arc::new(Type::Integer{size: 8, signed: false});
	}
	| UNSIGNED SHORT {
		$v = Arc::new(Type::Integer{size: 16, signed: false});
	}
	| UNSIGNED INT {
		$v = Arc::new(Type::Integer{size: 32, signed: false});
	}
	| UNSIGNED LONG {
		$v = Arc::new(Type::Integer{size: 64, signed: false});
	}
	| STRUCT n = IDENTIFIER {
		let t = match recog.getType($n.text) {
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
	';' #empty
	// | labeledStmt
	| expr ';' #exprStmt
	| block    #blockStmt
	| ifStmt   #if
	| whileStmt #while
	| dowhileStmt #dowhile
	| forStmt #for
	// TODO
	// | switchStmt #switch 
	| breakStmt #break
	| continueStmt #contine
	// | gotoStmt // TODO: implement goto
	| returnStmt #return;
labeledStmt: IDENTIFIER ':' stmt;
ifStmt:
	IF '(' cond = expr ')' thenStmt = stmt (ELSE elseStmt = stmt)?;
whileStmt: WHILE '(' cond = expr ')' body = stmt;
dowhileStmt: DO body = stmt WHILE '(' cond = expr ')';
forStmt:
	FOR '(' init = expr ';' cond = expr ';' step = expr ')' body = stmt;
switchStmt: SWITCH '(' cond = expr ')' '{' caseClauses '}';
caseClauses: caseClause* defaultClause?;
caseClause: values = cases () body = caseBody;
cases: (CASE primary ':')+;
defaultClause: DEFAULT ':' body = caseBody;
caseBody: (stmt)+;
// gotoStmt: GOTO IDENTIFIER ';';
breakStmt: BREAK ';';
continueStmt: CONTINUE ';';
returnStmt: RETURN expr? ';';

postfixExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]:
	p = primary {
		$e = $p.e;
	} (
		'++' {
			let inner_expr = (&$e).clone().unwrap();
			let location = $start.start as usize .. recog.get_current_token().stop as usize;
			$e = Some(Box::new(
				report_or_unwrap!(
					PostfixExprNode::new_inc(inner_expr, location)
					,recog)
			)  as Box<dyn ExprNode>);
		}
		| '--' {
			let inner_expr = (&$e).clone().unwrap();
			let location = $start.start as usize .. recog.get_current_token().stop as usize;
			$e = Some(Box::new(
				report_or_unwrap!(
					PostfixExprNode::new_dec(inner_expr, location)
					,recog)
			)  as Box<dyn ExprNode>);
		}
		| '[' expr ']' {
			let inner_expr = (&$e).clone().unwrap();
			let index_expr = ($expr.e).clone().unwrap();
			let location = $start.start as usize .. recog.get_current_token().stop as usize;
			$e = Some(Box::new(
				report_or_unwrap!(
					PostfixExprNode::new_index(inner_expr, index_expr, location)
					,recog)
			)  as Box<dyn ExprNode>);
		}
		| '.' i = IDENTIFIER {
			let inner_expr = (&$e).clone().unwrap();
			let location = $start.start as usize .. recog.get_current_token().stop as usize;
			let field =  $i.text.to_string();
			$e = Some(Box::new(
				report_or_unwrap!(
					PostfixExprNode::new_member_of(inner_expr, field, location)
					,recog)
			)  as Box<dyn ExprNode>);
		}
		| '->' i = IDENTIFIER { // member access 
			let inner_expr = (&$e).clone().unwrap();
			let location = $start.start as usize .. recog.get_current_token().stop as usize;
			let field =  $i.text.to_string();
			$e = Some(Box::new(
				report_or_unwrap!(
					PostfixExprNode::new_member_of_pointer(inner_expr, field, location)
					,recog)
			)  as Box<dyn ExprNode>);
		}
		| '(' args ')' {// func call
			let func = (&$e).clone().unwrap();
			let args = ($args.v).borrow().clone().into_iter().map(|e| e.unwrap()).collect();
			let location = $start.start as usize .. recog.get_current_token().stop as usize;
			$e = Some(Box::new(
				report_or_unwrap!(
					PostfixExprNode::new_func_call(func, args, location)
					,recog)
			)  as Box<dyn ExprNode>);
		}
	)*;
unaryExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]:
	'++' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				UnaryExprNode::new_inc(inner_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| '--' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				UnaryExprNode::new_dec(inner_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| '+' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				UnaryExprNode::new_add(inner_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| '-' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				UnaryExprNode::new_neg(inner_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| '!' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				UnaryExprNode::new_logical_not(inner_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| '~' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				UnaryExprNode::new_not(inner_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| '*' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				UnaryExprNode::new_deref(inner_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| '&' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				UnaryExprNode::new_addr(inner_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| postfixExpr {
		$e = $postfixExpr.e;
	};
castExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: '(' typeName ')' castExpr {
		let inner_expr = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				CastExprNode::new(inner_expr, $typeName.v.clone(), location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | unaryExpr {
		$e = $unaryExpr.e;
	};
mulDivExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: castExpr {
		$e = $castExpr.e;
	} ( '*' castExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_mul(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | '/' castExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_div(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | '%' castExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($castExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_mod(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
addSubExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: mulDivExpr {
		$e = $mulDivExpr.e;
	} ( '+' mulDivExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($mulDivExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_add(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | '-' mulDivExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($mulDivExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_sub(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
shiftExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: addSubExpr {
		$e = $addSubExpr.e;
	} ( '<<' addSubExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($addSubExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_shl(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | '>>' addSubExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($addSubExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_shr(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
relExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]:
	shiftExpr {
		$e = $shiftExpr.e;
	} ( '<' shiftExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($shiftExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_lt(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | '>' shiftExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($shiftExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_gt(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | '<=' shiftExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($shiftExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_le(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | '>=' shiftExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($shiftExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_ge(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
eqExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: relExpr {
		$e = $relExpr.e;
	} ( '==' relExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($relExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_eq(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	} | '!=' relExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($relExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_ne(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
andExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: eqExpr {
		$e = $eqExpr.e;
	} ( '&' eqExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($eqExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_and(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
xorExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: andExpr {
		$e = $andExpr.e;
	 } ( '^' andExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($andExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_xor(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	 })*;
orExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: xorExpr {
		$e = $xorExpr.e;
	} ( '|' xorExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($xorExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_or(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
logicAndExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: orExpr {
		$e = $orExpr.e;
	} ( '&&' orExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($orExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_logical_and(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
logicOrExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: logicAndExpr {
		$e = $logicAndExpr.e;
	} ( '||' logicAndExpr {
		let lhs = (&$e).clone().unwrap();
		let rhs = ($logicAndExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				BinaryExprNode::new_logical_or(lhs, rhs, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
condExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]: logicOrExpr {
		$e = $logicOrExpr.e;
	} ( '?' true_expr = logicOrExpr ':' false_expr = logicOrExpr {
		let cond = (&$e).clone().unwrap();
		let true_expr = ($true_expr.e).clone().unwrap();
		let false_expr = ($false_expr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				CondExprNode::new(cond, true_expr, false_expr, location)
				,recog)
		)  as Box<dyn ExprNode>);
	})*;
assignmentExpr
	returns[
		Option<Box<dyn ExprNode>> e
	]:
	condExpr {
		$e = $condExpr.e;
	}
	| unaryExpr '=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::Assign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '+=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::AddAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '-=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::SubAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '*=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::MulAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '/=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::DivAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '%=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::ModAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '&=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::AndAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	} 
	| unaryExpr '|=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::OrAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '^=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::XorAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '<<=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::ShlAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
	| unaryExpr '>>=' assignmentExpr {
		let lhs = ($unaryExpr.e).clone().unwrap();
		let rhs = ($assignmentExpr.e).clone().unwrap();
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(
			report_or_unwrap!(
				AssignExprNode::new(lhs, rhs, AssignOp::ShrAssign,location)
				,recog)
		)  as Box<dyn ExprNode>);
	}
;
expr
	returns[
		Option<Box<dyn ExprNode>> e
	]:
	assignmentExpr {
		$e = $assignmentExpr.e;
	}
	(
		',' assignmentExpr {
			let lhs = (&$e).clone().unwrap();
			let rhs = ($assignmentExpr.e).clone().unwrap();
			let location = $start.start as usize .. recog.get_current_token().stop as usize;
			$e = Some(Box::new(
				report_or_unwrap!(
					BinaryExprNode::new_comma(lhs, rhs,location)
					,recog)
			)  as Box<dyn ExprNode>);
		}
	)*;

args
	returns[
	RefCell<Vec<Option<Box<dyn ExprNode>>>> v
]: (
		assignmentExpr {
	(&$v).borrow_mut().push($assignmentExpr.e.clone());
} (
			',' assignmentExpr {
	(&$v).borrow_mut().push($assignmentExpr.e.clone());
}
		)*
	)?;
primary
	returns[
		Option<Box<dyn ExprNode>> e
	]:
	i = INTEGER {
		let text = $i.text;
		let num = if text == "0" {0}
		    else if let Some(stripped) = text.strip_prefix("0x") {
				i32::from_str_radix(stripped, 16).unwrap()
			} else if let Some(stripped) = text.strip_prefix('0') {
				i32::from_str_radix(stripped, 8).unwrap()
			} else {
				#[allow(clippy::from_str_radix_10)]
				i32::from_str_radix(text, 10).unwrap()
			};
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(IntegerLiteralNode::new(num, location)) as Box<dyn ExprNode>);
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
				ParserError::InvalidCharacterLiteral(text)
			);
			recog.notify_error_listeners(
				"".to_string(),
				// last token
				Some(recog.base.input.index() - 1),
				Some(&err)
			);
			return Err(err);
		}
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(CharLiteralNode::new(ch as i8, location)) as Box<dyn ExprNode>);
	}
	| i = STRING_LITERAL {
		let text = $i.text;
		let text = snailquote::unescape(text).map_err(|err| {
			ParserError::from(quick_error::Context(text.to_string(), err))
		});
		let text = report_or_unwrap!(text, recog);
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(StringLiteralNode::new(text, location)) as Box<dyn ExprNode>);
	}
	| i = IDENTIFIER {
		// variable or function
		let name = $i.text;
		let entity = recog.scope.get(name).ok_or_else(|| ParserError::VariableUndefined(name.to_string()));
		let entity = report_or_unwrap!(entity, recog);
		let location = $start.start as usize .. recog.get_current_token().stop as usize;
		$e = Some(Box::new(EntityNode::new(entity, location)) as Box<dyn ExprNode>);
	}
	| '(' expr ')' {
		$e = $expr.e;
	};
