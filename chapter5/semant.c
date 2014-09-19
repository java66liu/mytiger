#include "semant.h"
#include "errormsg.h"
#include <stdlib.h>
#include <assert.h>
#include "env.h"

struct expty transExp(S_table venv, S_table tenv, A_exp a) {
	switch(a->kind) {
		case A_opExp: {
			A_oper oper = a->u.op.oper;
			struct expty left = transExp(venv, tenv, a->u.op.left);
			struct expty right = transExp(venv, tenv, a->u.op.right);
			if (oper == A_plusOp) {
				if (left.ty->kind != Ty_int)
					EM_error(a->u.op.left->pos, "integer required");
				if (right.ty->kind != Ty_int)
					EM_error(a->u.op.right->pos, "integer required");
				return expTy(NULL, Ty_Int());
			}
		}
	}
	assert(0);
}

struct expty transVer(S_table venv, S_table tenv, A_var v) {
	switch(v->kind) {
		case A_simpleVar: {
			E_enventry x = S_look(venv, v->u.simple);
			if (x && x->kind == E_varEntry)
				return expTy(NULL, actual_ty(x->u.var.ty));
			else {
				EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
				return expTy(NULL, Ty_Int());
			}
		}
	}
}
