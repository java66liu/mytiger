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
	return actual_ty(ty->u.name.ty);
}

int isSameTy(Ty_ty ty1, Ty_ty ty2) {
	if (ty1->kind == ty2->kind) {
		if (ty1->kind == Ty_array || ty1->kind == Ty_record) {
			if (ty1 == ty2) {
				return 1;
			}
			return 0;
		}
		return 1;
	}
	if (ty1->kind == Ty_nil && ty2->kind == Ty_record) {
		return 1;
	}
	if (ty1->kind == Ty_record && ty2->kind == Ty_nil) {
		return 1;
	}
	return 0;
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
			Ty_tyList ttl;
			A_expList el;
			for (ttl = fun->u.fun.formals, el = e->u.call.args; ttl || el; ttl = ttl->tail, el = el->tail) {
				if (ttl && !el) {
					EM_error(e->pos, (string)"formals are more than actuals");
					return expTy(NULL, Ty_Int());
				}
				if (!ttl && el) {
					EM_error(e->pos, (string)"formals are less than actuals");
					return expTy(NULL, Ty_Int());
				}
				struct expty arg = transExp(venv, tenv, el->head);
				if (ttl->head->kind != arg.ty->kind) {
					EM_error(e->pos, (string)"formals and actuals have different types");
					return expTy(NULL, Ty_Int());
				}
			}
			if (!fun->u.fun.result) {
				return expTy(NULL, Ty_Void());
			}
			return expTy(NULL, actual_ty(fun->u.fun.result));
		}
		case A_opExp: {
			A_oper oper = e->u.op.oper;
			struct expty left = transExp(venv, tenv, e->u.op.left);
			struct expty right = transExp(venv, tenv, e->u.op.right);
			if (oper == A_plusOp || oper == A_minusOp || oper == A_timesOp || oper == A_divideOp) {
				if (left.ty->kind != Ty_int || right.ty->kind != Ty_int) {
					EM_error(e->u.op.left->pos, (string)"integer required");
					return expTy(NULL, Ty_Int());
				}
				return expTy(NULL, Ty_Int());
			}
			if (oper == A_eqOp || oper == A_neqOp) {
				if (isSameTy(left.ty, right.ty)) {
					return expTy(NULL, Ty_Int());
				}
				EM_error(e->pos, (string)"same type required");
				return expTy(NULL, Ty_Int());
/*
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
					EM_error(e->pos, "compare record with array");
					return expTy(NULL, Ty_Int());
				}
*/
			}
			if (oper == A_ltOp || oper == A_leOp || oper == A_gtOp || oper == A_geOp) {
				if (left.ty->kind == Ty_int && right.ty->kind == Ty_int) {
					return expTy(NULL, Ty_Int());
				}
				if (left.ty->kind == Ty_string && right.ty->kind == Ty_string) {
					return expTy(NULL, Ty_Int());
				}
				EM_error(e->pos, "comparison of incompatible type");
				return expTy(NULL, Ty_Int());
			}
			assert(0);
		}
		case A_recordExp: {
			Ty_ty ty = (Ty_ty)S_look(tenv, e->u.record.typ);
			if (!ty) {
				EM_error(e->pos, "undefined record type '%s'", S_name(e->u.record.typ));
				return expTy(NULL, Ty_Nil());
			}
			ty = actual_ty(ty);
			if (ty->kind != Ty_record) {
				EM_error(e->pos, "'%s' is not a record", S_name(e->u.record.typ));
			}
			else {
				Ty_fieldList tfl;
				A_efieldList efl;
				for (tfl = ty->u.record, efl = e->u.record.fields; tfl || efl; tfl = tfl->tail, efl = efl->tail) {
					if (tfl && !efl) {
						EM_error(e->pos, "record field is less than need");
						return expTy(NULL, Ty_Nil());
					}
					if (!tfl && efl) {
						EM_error(e->pos, "record field is more than need");
						return expTy(NULL, Ty_Nil());
					}
					if (tfl->head->name != efl->head->name) {
						EM_error(e->pos, "record field not match");
						return expTy(NULL, Ty_Nil());
					}
					else {
						struct expty res = transExp(venv, tenv, efl->head->exp);
						Ty_ty fieldty = actual_ty(tfl->head->ty);
						if (!isSameTy(fieldty, res.ty)) {
							EM_error(e->pos, (string)"record field mismatch");
							return expTy(NULL, Ty_Nil());
						}
/*
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
							//return expTy(NULL, Ty_Nil());
						}
						if (fieldty->kind != res.ty->kind) {
							EM_error(e->pos, "record field not match");
							return expTy(NULL, Ty_Nil());
						}
*/
						}
				}
			}
			return expTy(NULL, ty);
		}
		case A_seqExp: {
			if (e->u.seq) {
				return expTy(NULL, transExp(venv, tenv, e->u.seq->head).ty);
			}
			return expTy(NULL, Ty_Void());
		}
		case A_assignExp: {
			struct expty left = transVar(venv, tenv, e->u.assign.var);
			struct expty right = transExp(venv, tenv, e->u.assign.exp);
			if (isSameTy(left.ty, right.ty)) {
				return expTy(NULL, Ty_Void());
			}
/*
			if (left.ty->kind == Ty_record) {
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
*/
			EM_error(e->pos, "type not match");
			return expTy(NULL, Ty_Void());
		}
		case A_ifExp: {
			struct expty test = transExp(venv, tenv, e->u.iff.test);
			if (test.ty->kind != Ty_int) {
				EM_error(e->pos, "if-else clause integer test required");
				return expTy(NULL, Ty_Void());
			}
			struct expty then = transExp(venv, tenv, e->u.iff.then);
			if (e->u.iff.elsee == NULL) {
				if (then.ty->kind != Ty_void) {
					EM_error(e->pos, "if-then returns non unit");
					return expTy(NULL, Ty_Void());
				}
				return expTy(NULL, Ty_Void());
			}
			struct expty elsee = transExp(venv, tenv, e->u.iff.elsee);
			if (isSameTy(then.ty, elsee.ty)) {
				return expTy(NULL, then.ty);
			}
/*
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
			}
			if (then.ty->kind == elsee.ty->kind) {
				return expTy(NULL, then.ty);
			}
*/
			EM_error(e->pos, "types of then - else differ");
			return expTy(NULL, Ty_Void());
		}
		case A_whileExp: {
			struct expty test = transExp(venv, tenv, e->u.whilee.test);
			struct expty body = transExp(venv, tenv, e->u.whilee.body);
			if (test.ty->kind != Ty_int) {
				EM_error(e->pos, "test clause integer required");
				return expTy(NULL, Ty_Void());
			}
			if (body.ty->kind != Ty_void) {
				EM_error(e->pos, "body of while not void");
				return expTy(NULL, Ty_Void());
			}
			return expTy(NULL, Ty_Void());
		}
		case A_forExp: {
			struct expty lo = transExp(venv, tenv, e->u.forr.lo);
			struct expty hi = transExp(venv, tenv, e->u.forr.hi);
			if (lo.ty->kind != Ty_int || hi.ty->kind != Ty_int) {
				EM_error(e->pos, "for clause lo and hi integer required");
				return expTy(NULL, Ty_Void());
			}
			S_beginScope(venv);
			if (!S_look(venv, e->u.forr.var)) {
				S_enter(venv, e->u.forr.var, E_VarEntry(Ty_Int()));
			}
			struct expty body = transExp(venv, tenv, e->u.forr.body);
			S_endScope(venv);
			if (body.ty->kind != Ty_void) {
				EM_error(e->pos, "for clause body no-value required");
				return expTy(NULL, Ty_Void());
			}
			return expTy(NULL, Ty_Void());
		}
		case A_breakExp: {
			return expTy(NULL, Ty_Void());
		}
		case A_letExp: {
			A_decList d;
			S_beginScope(venv);
			S_beginScope(tenv);
			for (d = e->u.let.decs; d; d = d->tail) {
				transDec(venv, tenv, d->head);
			}
			struct expty exp = transExp(venv, tenv, e->u.let.body);
			S_endScope(venv);
			S_endScope(tenv);
			return exp;
		}
		case A_arrayExp: {
			struct expty size = transExp(venv, tenv, e->u.array.size);
			struct expty init = transExp(venv, tenv, e->u.array.init);
			Ty_ty ty = (Ty_ty)S_look(tenv, e->u.array.typ);
			if (!ty) {
				EM_error(e->pos, "undefined array type %s", S_name(e->u.array.typ));
				return expTy(NULL, Ty_Void());
			}
			ty = actual_ty(ty);
			if (ty->kind != Ty_array) {
				EM_error(e->pos, "%s is not a array", S_name(e->u.array.typ));
				return expTy(NULL, Ty_Void());
			}
			if (ty->u.array->kind!= init.ty->kind) {
				EM_error(e->pos, "initializing exp and array type differ");
				return expTy(NULL, Ty_Void());
			}
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
					return expTy(NULL, Ty_Void());
				}
			}
			else if (ty2->kind == Ty_array) {
				if (init.ty->kind == Ty_array && ty2 == init.ty) {
					if (ty2->kind != init.ty->kind) {
						 EM_error(e->pos, "initializing exp and array type differ");
						 return expTy(NULL, Ty_Void());
					}
				}
				else {
					EM_error(e->pos, "array init fail:type not match");
					return expTy(NULL, Ty_Void());
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
				EM_error(v->pos, (string)"undeclared variable '%s'", S_name(v->u.simple));
				return expTy(NULL, Ty_Int());
			}
		}
		case A_fieldVar: {
			struct expty res;
			Ty_ty ty;
			res = transVar(venv, tenv, v->u.field.var);
			ty = res.ty;
			if (ty->kind == Ty_record) {
				Ty_fieldList tfl = ty->u.record;
				while (tfl) {
					if (tfl->head->name == v->u.field.sym) {
						return expTy(NULL, actual_ty(tfl->head->ty));
					}
					else {
						tfl = tfl->tail;
					}
				}
				EM_error(v->pos, "undefined field %s", S_name(v->u.field.sym));
				return expTy(NULL, Ty_Int());
			}
			EM_error(v->pos, "variable not record");
			return expTy(NULL, Ty_Int());
		}
		case A_subscriptVar: {
			struct expty res = transVar(venv, tenv, v->u.subscript.var);
			struct expty index = transExp(venv, tenv, v->u.subscript.exp);
			Ty_ty ty = res.ty;
			if (ty->kind != Ty_array) {
				EM_error(v->pos, "variable not array");
				return expTy(NULL, Ty_Int());
			}
			if (index.ty->kind != Ty_int) {
				EM_error(v->pos, "array index error");
				return expTy(NULL, Ty_Int());
			}
			return expTy(NULL, actual_ty(ty->u.array));
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
	if (!d) {
		return;
	}
	switch (d->kind) {
		case A_varDec: {
			struct expty e = transExp(venv, tenv, d->u.var.init);
			if (d->u.var.typ) {
				Ty_ty ty = (Ty_ty)S_look(tenv, d->u.var.typ);
				if (!ty) {
					EM_error(d->pos, "undefined type %s", S_name(d->u.var.typ));
					return;
				}
				ty = actual_ty(ty);
				if (ty->kind != e.ty->kind) {
					if (e.ty->kind != Ty_nil) {
						EM_error(d->pos, "type constraint and init value differ");
						return;
					}
				}
				if (ty != e.ty) {
					if (ty->kind != Ty_record && e.ty->kind == Ty_nil) {
						EM_error(d->pos, "initializing nil expressions not constrained by record type");
						return;
					}
				}
				S_enter(venv, d->u.var.var, E_VarEntry(ty));
				return;
			}
			if (e.ty->kind == Ty_nil) {
				EM_error(d->pos, "initializing nil expressions not constrained by record type");
				return;
			}
			S_enter(venv, d->u.var.var, E_VarEntry(e.ty));
			return;
		}
		case A_typeDec: {
			A_nametyList ntl_p, ntl_q;
			for (ntl_p = d->u.type; ntl_p && ntl_p->tail; ntl_p = ntl_p->tail) {
				for (ntl_q = ntl_p->tail; ntl_q; ntl_q = ntl_q->tail) {
					if (ntl_p->head->name == ntl_q->head->name) {
						EM_error(d->pos, "type '%s' redefined", S_name(ntl_q->head->name));
						return;
					}
				}
			}
			A_nametyList ntl;
			for (ntl = d->u.type; ntl; ntl = ntl->tail) {
				S_enter(tenv, ntl->head->name, transTy(tenv, ntl->head->ty));
			}
/*
			for (ntl_p = d->u.type; ntl_p; ntl_p = ntl_p->tail) {
				Ty_ty ty = S_look(tenv, ntl_p->head->name);
				ty->u.name.ty = transTy(tenv, ntl_p->head->ty);
			}

			for (ntl_p = d->u.type; ntl_p; ntl_p = ntl_p->tail) {
				Ty_ty ty = S_look(tenv, ntl_p->head->name);
				if (actual_ty(ty) == ty) {
					EM_error(d->pos, "infinite recursive type '%s'", S_name(ntl_p->head->name));
					return;
				}
			}
*/
			return;
		}
		case A_functionDec: {
			A_fundecList fdl_p, fdl_q;
			for (fdl_p = d->u.function; fdl_p && fdl_p->tail; fdl_p = fdl_p->tail) {
				for (fdl_q = fdl_p->tail; fdl_q; fdl_q = fdl_q = fdl_q->tail) {
					if (fdl_p->head->name == fdl_q->head->name) {
						EM_error(d->pos, "function '%s' redefined", S_name(fdl_q->head->name));
						return;
					}
				}
			}
			A_fundecList fdl;
			for (fdl = d->u.function; fdl; fdl = fdl->tail) {
				A_fundec fd = fdl->head;
				Ty_tyList formalTys = makeFormalTyList(tenv, fd->params);
				if (fd->result) {
					Ty_ty resultTy = S_look(tenv, fd->result);
					S_enter(venv,  fd->name, E_FunEntry(formalTys, resultTy));
				}
				else {
					S_enter(venv,  fd->name, E_FunEntry(formalTys, NULL));
				}
				S_beginScope(venv);
				{
					A_fieldList l;
					Ty_tyList t;
					for (l = fd->params, t = formalTys; l; l = l->tail, t = t->tail) {
						S_enter(venv, l->head->name, E_VarEntry(t->head));
					}
				}
				transExp(venv, tenv, d->u.function->head->body);
				S_endScope(venv);
			}
			break;
		}
		default: {
			assert(0);
		}
	}
}
