#include "semant.h"
#include "errormsg.h"
#include <stdlib.h>
#include <assert.h>
#include "env.h"

#define MAX_TYPE_RECUSIVE 4000

static int check_ty(Ty_ty ty) {
	int i;
	Ty_ty ty2 = ty;
	assert(ty != NULL);
	for(i = 0; i < MAX_TYPE_RECUSIVE; i++) {
		if(ty2 == NULL) {
			return 0;
		}
		else if(ty2->kind == Ty_name) {
			ty2 = ty2->u.name.ty;
		}
		else {
			return 1;
		}
	}
	return 0;
}

static Ty_ty actual_ty(Ty_ty ty) {
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
	A_fieldList ptr = params;
	Ty_tyList ret, ptr2;
	Ty_ty ty;

	ret = NULL; ptr2 = NULL;
	while(ptr != NULL) {
		ty = S_look(tenv, ptr->head->typ);
		if(ty != NULL) {
			if(ret == NULL) {
				ret = Ty_TyList(ty, NULL);
				ptr2 = ret;
			}
			else {
				ptr2->tail = Ty_TyList(ty, NULL);
				ptr2 = ptr2->tail;
			}
		}
		else {
			EM_error(ptr->head->pos, "undefined type %s", S_name(ptr->head->typ));
			exit(1);
		}
		ptr = ptr->tail;
	}
	return ret;
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
				//EM_error(e->pos, "undefined function %s", S_name(e->u.call.func));
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
			else if (oper == A_eqOp || oper == A_neqOp) {
				if (left.ty->kind == Ty_int && right.ty->kind == Ty_int) {
					return expTy(NULL, Ty_Int());
				}
				else if (left.ty->kind == Ty_string && right.ty->kind == Ty_string) {
					return expTy(NULL, Ty_Int());
				}
				else if (left.ty->kind == Ty_record && right.ty->kind == Ty_record) {
					if (left.ty == right.ty) {
						return expTy(NULL, Ty_Int());
					}
					else {
						EM_error(e->pos, "same record type required");
						return expTy(NULL, Ty_Int());
					}
				}
				else if (left.ty->kind == Ty_array && right.ty->kind == Ty_array) {
					if (left.ty == right.ty) {
						return expTy(NULL, Ty_Int());
					}
					else {
						EM_error(e->pos, "same array type required");
						return expTy(NULL, Ty_Int());
					}
				}
				else if (left.ty->kind == Ty_record && right.ty->kind == Ty_nil) {
					return expTy(NULL, Ty_Int());
				}
				else if (left.ty->kind == Ty_nil && right.ty->kind == Ty_record) {
					return expTy(NULL, Ty_Int());
				}
				else {
					EM_error(e->pos, "type not match");
					return expTy(NULL, Ty_Int());
				}
			}
			else if (oper == A_ltOp || oper == A_leOp || oper == A_gtOp || oper == A_geOp) {
				if (left.ty->kind == Ty_int && right.ty->kind == Ty_int) {
					return expTy(NULL, Ty_Int());
				}
				else {
					EM_error(e->pos, "type not match");
					return expTy(NULL, Ty_Int());
				}
			}
		}
		case A_recordExp: {
			Ty_ty ty = S_look(tenv, e->u.record.typ);
			if (ty != NULL) {
				ty = actual_ty(ty);
			}
			if (ty == NULL || ty->kind != Ty_record) {
				EM_error(e->pos, "undefined record type %s", S_name(e->u.record.typ));
				return expTy(NULL, Ty_Nil());
			}
			else {
				Ty_fieldList fieldList = ty->u.record;
				A_efieldList efieldList = e->u.record.fields;
				while (fieldList != NULL && efieldList != NULL) {
					if (fieldList->head->name != efieldList->head->name) {
						EM_error(e->pos, "record field not match");
						return expTy(NULL, Ty_Nil());
					}
					struct expty res = transExp(venv, tenv, efieldList->head->exp);
					Ty_ty fieldty = actual_ty(fieldList->head->ty);
					if (fieldty->kind == Ty_record && res.ty->kind == Ty_nil) {
					}
					else if (fieldty->kind == Ty_record && res.ty->kind == Ty_record) {
						if (fieldty == res.ty) {
						}
						else {
							EM_error(e->pos, "record field not match");
							return expTy(NULL, Ty_Nil());
						}
					}
					else if (fieldty->kind == Ty_array && res.ty->kind == Ty_array) {
						if (fieldty == res.ty) {
						}
						else {
							EM_error(e->pos, "record field not match");
							return expTy(NULL, Ty_Nil());
						}
					}
					else {
						if (fieldty->kind != res.ty->kind) {
							EM_error(e->pos, "record field not match");
							return expTy(NULL, Ty_Nil());
						}
					}
					fieldList = fieldList->tail;
					efieldList = efieldList->tail;
				}
				if (fieldList != NULL && efieldList != NULL) {
					EM_error(e->pos, "record field not match");
					return expTy(NULL, Ty_Nil());
				}
			}
			return expTy(NULL, ty);
		}
		case A_seqExp: {
			if (e->u.seq == NULL) {
				return expTy(NULL, Ty_Void());
			}
			else {
				return expTy(NULL, transExp(venv, tenv, e->u.seq->head).ty);
			}
		}
		case A_assignExp: {
			struct expty left = transVar(venv, tenv, e->u.assign.var);
			struct expty right = transExp(venv, tenv, e->u.assign.exp);
			if (left.ty->kind == Ty_nil) {
				if (right.ty->kind == Ty_nil) {
					return expTy(NULL, Ty_Void());
				}
				else if (right.ty->kind == Ty_record && left.ty == right.ty) {
					return expTy(NULL, Ty_Void());
				}
				else {
					EM_error(e->pos, "type not match");
					return expTy(NULL, Ty_Void());
				}
			}
			else if (left.ty->kind == Ty_array) {
				if (right.ty->kind == Ty_array && left.ty == right.ty) {
					return expTy(NULL, Ty_Void());
				}
				else {
					EM_error(e->pos, "type not match");
					return expTy(NULL, Ty_Void());
				}
			}
			else if (left.ty->kind == right.ty->kind) {
				return expTy(NULL, Ty_Void());
			}
			else {
				EM_error(e->pos, "type not match");
				return expTy(NULL, Ty_Void());
			}
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
					else {
						return expTy(NULL, Ty_Void());
					}
				}
				else {
					struct expty elsee = transExp(venv, tenv, e->u.iff.elsee);
					if (then.ty->kind == Ty_record) {
						if (elsee.ty->kind == Ty_record && then.ty == elsee.ty) {
							return expTy(NULL, then.ty);
						}
						else if (elsee.ty->kind == Ty_nil) {
							return expTy(NULL, then.ty);
						}
					}
					else if (then.ty->kind == Ty_nil && elsee.ty->kind == Ty_record) {
						return expTy(NULL, elsee.ty);
					}
					else if (then.ty->kind == Ty_array && elsee.ty->kind == Ty_array) {
						if (then.ty == elsee.ty) {
							return expTy(NULL, then.ty);
						}
						else {
							EM_error(e->pos, "then and else type not match");
							return expTy(NULL, Ty_Void());
						}
					}
					else if (then.ty->kind == elsee.ty->kind) {
						return expTy(NULL, then.ty);
					}
					else {
						EM_error(e->pos, "then and else type not match");
						return expTy(NULL, Ty_Void());
					}
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
				if (body.ty->kind != Ty_void) {
					//EM_error(e->pos, "while clause must produce no value");
					return expTy(NULL, Ty_Void());
				}
				else {
					return expTy(NULL, Ty_Void());
				}
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
					return expTy(NULL, Ty_Void());
				}
				else {
					return expTy(NULL, Ty_Void());
				}
			}
		}
		case A_breakExp: {
			return expTy(NULL, Ty_Void());
		}
		case A_letExp: {
			//S_beginScope(venv);
			//S_beginScope(tenv);
			struct expty exp = transExp(venv, tenv, e->u.let.body);
			//S_endScope(tenv);
			//S_endScope(venv);
			return expTy(NULL, exp.ty);
		}
		case A_arrayExp: {
			struct expty size = transExp(venv, tenv, e->u.array.size);
			struct expty init = transExp(venv, tenv, e->u.array.init);
			Ty_ty ty = actual_ty(S_look(tenv, e->u.array.typ));
			if (ty == NULL || ty->kind != Ty_array) {
				//EM_error(e->pos, "undefined array type %s", S_name(e->u.array.typ));
				return expTy(NULL, Ty_Void());
			}
			Ty_ty ty2 = actual_ty(ty->u.array);
			if (size.ty->kind != Ty_int) {
				EM_error(e->pos, "size int value required");
				return expTy(NULL, actual_ty(ty));
			}
			else if (ty2->kind == Ty_record) {
				if ((init.ty->kind == Ty_nil) || (init.ty->kind == Ty_record && ty2 == init.ty)) {
					return expTy(NULL, ty);
				}
				else {
					EM_error(e->pos, "array init fail:type not match");
					return expTy(NULL, ty);
				}
			}
			else if(ty2->kind == Ty_array) {
				if (init.ty->kind == Ty_array && ty2 == init.ty) {
					return expTy(NULL, ty);
				}
				else {
					EM_error(e->pos, "array init fail:type not match");
					return expTy(NULL, ty);
				}
			}
			else if (ty2->kind == init.ty->kind) {
				return expTy(NULL, ty);
			}
			else {
				EM_error(e->pos, "array init fail:type not match");
				return expTy(NULL, ty);
			}
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
				//EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
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
				//EM_error(v->pos, "undefined field %s", S_name(v->u.field.sym));
				return expTy(NULL, Ty_Int());
			}
			else {
				//EM_error(v->pos, "undefined field %s", S_name(v->u.field.sym));
				return expTy(NULL, Ty_Int());
			}
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
	switch(a->kind) {
	case A_nameTy: {
		Ty_ty ty = S_look(tenv, a->u.name);
		if(ty != NULL) {
			return ty;
		}
		else {
			//EM_error(a->pos, "undefined type %s", S_name(a->u.name));
			return Ty_Int();
		}
	}
	case A_recordTy: {
		Ty_ty ty,ty2;
		Ty_fieldList fields,tailptr;
		A_fieldList afields;
		if(a->u.record == NULL) {
			fields = NULL;
			ty = Ty_Record(fields);
			return ty;
		}
		else {
			fields = tailptr = NULL;
			afields = a->u.record;
			while(afields != NULL) {
				ty2 = S_look(tenv, afields->head->typ);
				if(ty2 != NULL)
				{
					if(fields == NULL)
					{
						fields = Ty_FieldList(Ty_Field(afields->head->name, ty2), NULL);
						tailptr = fields;
					}
					else 
					{
						tailptr->tail = Ty_FieldList(Ty_Field(afields->head->name, ty2), NULL);
						tailptr = tailptr->tail;
					}
				}
				else {
					//EM_error(a->pos, "undefined field type");
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
		if(ty != NULL) {
			return Ty_Array(ty);
		}
		else {
			//EM_error(a->pos, "undefined type %s", S_name(a->u.array));
			return Ty_Int();
		}
					}
	default: {
		assert(0);
			 }
	}
}

void transDec(S_table venv, S_table tenv, A_dec d) {
	if(d == NULL) {
		return;
	}
	switch(d->kind) {
	case A_varDec: {
		struct expty e = transExp(venv, tenv, d->u.var.init);
		S_enter(venv, d->u.var.var, E_VarEntry(e.ty));
		break;
	}
	case A_typeDec: {
		S_enter(venv, d->u.type->head->name, transTy(tenv, d->u.type->head->ty));
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
