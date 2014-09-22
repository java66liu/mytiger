#include "semant.h"
#include "errormsg.h"
#include <stdlib.h>
#include <assert.h>
#include "env.h"

static Ty_ty actual_ty(Ty_ty ty) {
	if (ty->kind != Ty_name) {
		return ty;
	}
	else {
		return actual_ty(ty->u.name.ty);
	}
}

struct expty transExp(S_table venv, S_table tenv, A_exp e) {
	if (e == NULL) {
		return expTy(NULL, Ty_Void());
	}
	switch (e->kind) {
		case A_varExp: {
			return transVar(venv, tenv, e->u.var);
		}
		case A_nilExp: {
			return expTy(NULL, Ty_Nil());
		}
		case A_intExp: {
			return expTy(NULL, Ty_Int());
		}
		case A_stringExp: {
			return expTy(NULL, Ty_String());
		}
		case A_callExp: {
			E_enventry fun = S_look(venv, e->u.call.func);
			if (fun == NULL || fun->kind == E_varEntry) {
				EM_error(e->pos, "undefined function %s", S_name(e->u.call.func));
				return expTy(NULL, Ty_Int());
			}
			else {
				if (fun->u.fun.formals == NULL && e->u.call.args == NULL) {
					return expTy(NULL, actual_ty(fun->u.fun.result));
				}
				else if (fun->u.fun.formals == NULL && e->u.call.args != NULL) {
					EM_error(e->pos, "incorrect function prototype %s", S_name(e->u.call.func) );
					return expTy(NULL, actual_ty(fun->u.fun.result));
				}
				else if (fun->u.fun.formals != NULL && e->u.call.args == NULL) {
					EM_error(e->pos, "incorrect function prototype %s", S_name(e->u.call.func)       );
					return expTy(NULL, actual_ty(fun->u.fun.result));
				}
				else {
					return expTy(NULL, actual_ty(fun->u.fun.result));
				}
			}
		}
		case A_opExp: {
			A_oper oper = e->u.op.oper;
			struct expty left = transExp(venv, tenv, e->u.op.left);
			struct expty right = transExp(venv, tenv, e->u.op.right);
			if (oper == A_plusOp || oper == A_minusOp || oper == A_timesOp || oper == A_divideOp) {
				if (left.ty->kind != Ty_int)
					EM_error(e->u.op.left->pos, "integer required");
				if (right.ty->kind != Ty_int)
					EM_error(e->u.op.right->pos, "integer required");
				return expTy(NULL, Ty_Int());
			}
			else if (oper == A_eqOp) {
				if (left.ty->kind == Ty_int && right.ty->kind == Ty_int) {
					return expTy(NULL, Ty_Int());
				}
				else if (left.ty->kind == Ty_string && right.ty->kind == Ty_string) {
					return expTy(NULL, Ty_Int());
				}
			}
		}
	}

	assert(0);
}

struct expty transVar(S_table venv, S_table tenv, A_var v) {
	switch (v->kind) {
		E_enventry x = S_look(venv, v->u.simple);
		if (x && x->kind == E_varEntry)
			return expTy(NULL, actual_ty(x->u.var.ty));
		else {
			EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
			return expTy(NULL, Ty_Int());
		}
	}
	assert(0);
}
