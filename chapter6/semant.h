#ifndef _semant_
#define _semant_

#include "types.h"
#include "absyn.h"

struct expty transVar(S_table venv, S_table tenv, A_var v);
struct expty transExp(S_table venv, S_table tenv, A_exp a);
void transDec(S_table venv, S_table, A_dec d);
Ty_ty transTy(S_table tenv, A_ty a);

typedef void *Tr_exp;

struct expty
{
	Tr_exp exp;
	Ty_ty ty;
};

struct expty expTy(Tr_exp exp, Ty_ty ty);

#endif