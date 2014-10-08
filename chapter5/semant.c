#include "semant.h"
#include "errormsg.h"
#include <stdlib.h>
#include <assert.h>
#include "env.h"

void SEM_transProg(A_exp exp) {
	S_table tenv = E_base_tenv();
	S_table venv = E_base_venv();
	struct expty main = transExp(venv, tenv, exp);
}

static Ty_ty actual_ty(Ty_ty ty) {
	assert(ty);
	if (ty->kind != Ty_name) {
		return ty;
	}
	else {
		return actual_ty(ty->u.name.ty);
	}
}

struct expty expTy(Tr_exp exp, Ty_ty ty)
{
	struct expty e;
	e.exp = exp;
	e.ty = ty;
	return e;
};

Ty_tyList makeFormalTyList(S_table tenv, A_fieldList params) {
	A_fieldList fl;
	Ty_tyList pTL = NULL;
	Ty_tyList resultTL = NULL;
	for (fl = params; fl; fl = fl->tail) {
		Ty_ty ty = (Ty_ty)S_look(tenv, fl->head->typ);
		if (ty) {
			if (resultTL) {
				pTL->tail = Ty_TyList(ty, NULL);
				pTL = pTL->tail;
			}
			else {
				resultTL = Ty_TyList(ty, NULL);
				pTL = resultTL;
			}
		}
		else {
			EM_error(fl->head->pos, (string)"undefined type %s", S_name(fl->head->typ));
			exit(1);
		}
	}
	return resultTL;
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
			E_enventry fun = (E_enventry)S_look(venv, e->u.call.func);
			if (!fun || fun->kind == E_varEntry) {
				EM_error(e->pos, (string)"undefined function %s", S_name(e->u.call.func));
				return expTy(NULL, Ty_Int());
			}
			if ((!fun->u.fun.formals && e->u.call.args) || (fun->u.fun.formals && !e->u.call.args)) {
				EM_error(e->pos, (string)"incorrect function prototype %s", S_name(e->u.call.func));
				return expTy(NULL, Ty_Int());
			}
			return expTy(NULL, actual_ty(fun->u.fun.result));
		}
		case A_opExp: {
			A_oper oper = e->u.op.oper;
			struct expty left = transExp(venv, tenv, e->u.op.left);
			struct expty right = transExp(venv, tenv, e->u.op.right);
			if (oper == A_plusOp || oper == A_minusOp || oper == A_timesOp || oper == A_divideOp) {
				if (left.ty->kind != Ty_int) {
					EM_error(e->u.op.left->pos, (string)"integer required");
					return expTy(NULL, Ty_Int());
				}
				if (right.ty->kind != Ty_int) {
					EM_error(e->u.op.right->pos, (string)"integer required");
					return expTy(NULL, Ty_Int());
				}
				return expTy(NULL, Ty_Int());
			}
			if (oper == A_eqOp || oper == A_neqOp) {
				if (left.ty->kind == right.ty->kind) {
					if (left.ty->kind == Ty_record && right.ty->kind == Ty_record) {
						if (left.ty != right.ty) {
							EM_error(e->pos, (string)"same record type required");
							return expTy(NULL, Ty_Int());
						}
					}
					if (left.ty->kind == Ty_array && right.ty->kind == Ty_array) {
						if (left.ty != right.ty) {
							EM_error(e->pos, (string)"same array type required");
							return expTy(NULL, Ty_Int());
						}
					}
					return expTy(NULL, Ty_Int());
				}
				else {
					if (left.ty->kind == Ty_record && right.ty->kind == Ty_nil) {
						return expTy(NULL, Ty_Int());
					}
					if (left.ty->kind == Ty_nil && right.ty->kind == Ty_record) {
						return expTy(NULL, Ty_Int());
					}
					EM_error(e->pos, "type not match");
					return expTy(NULL, Ty_Int());
				}
			}
			if (oper == A_ltOp || oper == A_leOp || oper == A_gtOp || oper == A_geOp) {
				if (left.ty->kind == Ty_int && right.ty->kind == Ty_int) {
					return expTy(NULL, Ty_Int());
				}
				if (left.ty->kind == Ty_string && right.ty->kind == Ty_string) {
					return expTy(NULL, Ty_Int());
				}
				EM_error(e->pos, "type not match");
				return expTy(NULL, Ty_Int());
			}
			assert(0);
		}
		case A_recordExp: {
			Ty_ty ty = actual_ty(S_look(tenv, e->u.record.typ));
			if (ty == NULL || ty->kind != Ty_record) {
				EM_error(e->pos, "undefined record type %s", S_name(e->u.record.typ));
				return expTy(NULL, Ty_Nil());
			}
			else {
				Ty_fieldList fieldList;
				A_efieldList efieldList;
				for (fieldList = ty->u.record, efieldList = e->u.record.fields; fieldList && efieldList; \
				fieldList = fieldList->tail, efieldList = efieldList->tail) {
					if (fieldList->head->name != efieldList->head->name) {
						EM_error(e->pos, "record field not match");
						return expTy(NULL, Ty_Nil());
					}
					else {
						struct expty res = transExp(venv, tenv, efieldList->head->exp);
						Ty_ty fieldty = actual_ty(fieldList->head->ty);
						if (fieldty->kind == Ty_record && res.ty->kind == Ty_record) {
							if (fieldty != res.ty) {
								EM_error(e->pos, "record field not match");
								return expTy(NULL, Ty_Nil());
							}
							return expTy(NULL, Ty_Nil());
						}
						if (fieldty->kind == Ty_array && res.ty->kind == Ty_array) {
							if (fieldty != res.ty) {
								EM_error(e->pos, "record field not match");
								return expTy(NULL, Ty_Nil());
							}
							return expTy(NULL, Ty_Nil());
						}
						if (fieldty->kind != res.ty->kind) {
							EM_error(e->pos, "record field not match");
							return expTy(NULL, Ty_Nil());
						}
					}
					if ((fieldList && !efieldList) || (!fieldList && efieldList)) {
						EM_error(e->pos, "record field not match");
						return expTy(NULL, Ty_Nil());
					}
					return expTy(NULL, Ty_Nil());
				}
			}
			return expTy(NULL, ty);
		}
		case A_seqExp: {
			if (e->u.seq) {
				return expTy(NULL, transExp(venv, tenv, e->u.seq->head).ty);
			}
			else {
				return expTy(NULL, Ty_Void());
			}
		}
		case A_assignExp: {
			struct expty left = transVar(venv, tenv, e->u.assign.var);
			struct expty right = transExp(venv, tenv, e->u.assign.exp);
			if (left.ty->kind == Ty_nil) {
				if (right.ty->kind == Ty_nil) {
					return expTy(NULL, Ty_Void());
				}
				if (right.ty->kind == Ty_record && left.ty == right.ty) {
					return expTy(NULL, Ty_Void());
				}
			}
			if (left.ty->kind == Ty_array) {
				if (right.ty->kind == Ty_array && left.ty == right.ty) {
					return expTy(NULL, Ty_Void());
				}
			}
			if (left.ty->kind == right.ty->kind) {
				return expTy(NULL, Ty_Void());
			}
			EM_error(e->pos, "type not match");
			return expTy(NULL, Ty_Void());
		}
		case A_ifExp: {
			struct expty test = transExp(venv, tenv, e->u.iff.test);
			if (test.ty->kind != Ty_int) {
				EM_error(e->pos, "if-else clause integer test required");
				return expTy(NULL, Ty_Void());
			}
			else {
				struct expty then = transExp(venv, tenv, e->u.iff.then);
				if (e->u.iff.elsee == NULL) {
					if (then.ty->kind != Ty_void) {
						EM_error(e->pos, "if-then clause void then required");
						return expTy(NULL, Ty_Void());
					}
					return expTy(NULL, Ty_Void());
				}
				else {
					struct expty elsee = transExp(venv, tenv, e->u.iff.elsee);
					if (then.ty->kind == Ty_record) {
						if (elsee.ty->kind == Ty_record && then.ty == elsee.ty) {
							return expTy(NULL, then.ty);
						}
						if (elsee.ty->kind == Ty_nil) {
							return expTy(NULL, then.ty);
						}
					}
					if (then.ty->kind == Ty_nil && elsee.ty->kind == Ty_record) {
						return expTy(NULL, elsee.ty);
					}
					if (then.ty->kind == Ty_array && elsee.ty->kind == Ty_array) {
						if (then.ty == elsee.ty) {
							return expTy(NULL, then.ty);
						}
						else {
							EM_error(e->pos, "then and else type not match");
							return expTy(NULL, Ty_Void());
						}
					}
					if (then.ty->kind == elsee.ty->kind) {
						return expTy(NULL, then.ty);
					}
					EM_error(e->pos, "then and else type not match");
					return expTy(NULL, Ty_Void());
				}
			}
		}
		case A_whileExp: {
			struct expty test = transExp(venv, tenv, e->u.whilee.test);
			struct expty body = transExp(venv, tenv, e->u.whilee.body);
			if (test.ty->kind != Ty_int) {
				EM_error(e->pos, "test clause integer required");
				return expTy(NULL, Ty_Void());
			}
			else {
				return expTy(NULL, Ty_Void());
			}
		}
		case A_forExp: {
			struct expty lo = transExp(venv, tenv, e->u.forr.lo);
			struct expty hi = transExp(venv, tenv, e->u.forr.hi);
			if (lo.ty->kind != Ty_int || hi.ty->kind != Ty_int) {
				EM_error(e->pos, "for clause lo and hi integer required");
				return expTy(NULL, Ty_Void());
			}
			else {
				struct expty body = transExp(venv, tenv, e->u.forr.body);
				if (body.ty->kind != Ty_void) {
					EM_error(e->pos, "for clause body no-value required");
				}
				return expTy(NULL, Ty_Void());
			}
		}
		case A_breakExp: {
			return expTy(NULL, Ty_Void());
		}
		case A_letExp: {
			A_decList d = e->u.let.decs;
			S_beginScope(venv);
			S_beginScope(tenv);
			while (d) {
				transDec(venv, tenv, d->head);
				d = d->tail;
			}
			struct expty exp = transExp(venv, tenv, e->u.let.body);
			S_endScope(venv);
			S_endScope(tenv);
			return exp;
		}
		case A_arrayExp: {
			struct expty size = transExp(venv, tenv, e->u.array.size);
			struct expty init = transExp(venv, tenv, e->u.array.init);
			Ty_ty ty = actual_ty(S_look(tenv, e->u.array.typ));
			Ty_ty ty2 = actual_ty(ty->u.array);
			if (size.ty->kind != Ty_int) {
				EM_error(e->pos, "size int value required");
				return expTy(NULL, actual_ty(ty));
			}
			else if (ty2->kind == Ty_record) {
				if ((init.ty->kind == Ty_nil) || (init.ty->kind == Ty_record && ty2 == init.ty)) {
				}
				else {
					EM_error(e->pos, "array init fail:type not match");
				}
			}
			else if(ty2->kind == Ty_array) {
				if (init.ty->kind == Ty_array && ty2 == init.ty) {
				}
				else {
					EM_error(e->pos, "array init fail:type not match");
				}
			}
			return expTy(NULL, ty);
		}
		default: {
			assert(0);
		}
	}
}

struct expty transVar(S_table venv, S_table tenv, A_var v) {
	switch (v->kind) {
		case A_simpleVar: {
			E_enventry x = S_look(venv, v->u.simple);
			if (x && x->kind == E_varEntry)
				return expTy(NULL, actual_ty(x->u.var.ty));
			else {
				return expTy(NULL, Ty_Int());
			}
		}
		case A_fieldVar: {
			struct expty res = transVar(venv, tenv, v->u.field.var);
			Ty_ty ty = res.ty;
			if (ty->kind == Ty_record) {
				Ty_fieldList fieldList = ty->u.record;
				while (fieldList != NULL) {
					if (fieldList->head->name == v->u.field.sym) {
						return expTy(NULL, actual_ty(fieldList->head->ty));
					}
					else {
						fieldList = fieldList->tail;
					}
				}
			}
			return expTy(NULL, Ty_Int());
		}
		case A_subscriptVar: {
			struct expty res = transVar(venv, tenv, v->u.subscript.var);
			struct expty index = transExp(venv, tenv, v->u.subscript.exp);
			Ty_ty ty = res.ty;
			if (ty->kind == Ty_array && index.ty->kind == Ty_int) {
				return expTy(NULL, actual_ty(ty->u.array));
			}
			else {
				EM_error(v->pos, "array index error");
				return expTy(NULL, Ty_Int());
			}
		}
		default: {
			assert(0);
		}
	}
}

Ty_ty transTy(S_table tenv, A_ty a) {
	assert(a != NULL);
	switch (a->kind) {
		case A_nameTy: {
			Ty_ty ty = S_look(tenv, a->u.name);
			if (ty != NULL) {
				return ty;
			}
			else {
				return Ty_Int();
			}
		}
		case A_recordTy: {
			Ty_ty ty,ty2;
			Ty_fieldList fields,tailptr;
			A_fieldList afields;
			if (a->u.record == NULL) {
				fields = NULL;
				ty = Ty_Record(fields);
				return ty;
			}
			else {
				fields = tailptr = NULL;
				afields = a->u.record;
				while (afields != NULL) {
					ty2 = S_look(tenv, afields->head->typ);
					if (ty2 != NULL) {
						if (fields == NULL) {
							fields = Ty_FieldList(Ty_Field(afields->head->name, ty2), NULL);
							tailptr = fields;
						}
						else {
							tailptr->tail = Ty_FieldList(Ty_Field(afields->head->name, ty2), NULL);
							tailptr = tailptr->tail;
						}
					}
					else {
						return Ty_Int();
					}
					afields = afields->tail;
				}
				ty = Ty_Record(fields);
				return ty;
			}
		}
		case A_arrayTy: {
			Ty_ty ty;
			ty = S_look(tenv, a->u.array);
			if (ty != NULL) {
				return Ty_Array(ty);
			}
			else {
				return Ty_Int();
			}
		}
		default: {
			assert(0);
		}
	}
}

void transDec(S_table venv, S_table tenv, A_dec d) {
	if (d == NULL) {
		return;
	}
	switch (d->kind) {
		case A_varDec: {
			struct expty e = transExp(venv, tenv, d->u.var.init);
			S_enter(venv, d->u.var.var, E_VarEntry(e.ty));
			break;
		}
		case A_typeDec: {
			A_nametyList ntl;
			for (ntl = d->u.type; ntl; ntl = ntl->tail) {
				S_enter(tenv, d->u.type->head->name, transTy(tenv, d->u.type->head->ty));
			}
			break;
		}
		case A_functionDec: {
			A_fundec f = d->u.function->head;
			Ty_ty resultTy = S_look(tenv, f->result);
			Ty_tyList formalTys = makeFormalTyList(tenv, f->params);
			S_enter(venv,  f->name, E_FunEntry(formalTys, resultTy));
			S_beginScope(venv);
			{
				A_fieldList l;
				Ty_tyList t;
				for (l = f->params, t = formalTys; l; l = l->tail, t = t->tail) {
					S_enter(venv, l->head->name, E_VarEntry(t->head));
				}
			}
			transExp(venv, tenv, d->u.function->head->body);
			S_endScope(venv);
			break;
		}
		default: {
			assert(0);
		}
	}
}
