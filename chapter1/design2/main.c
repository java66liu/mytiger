#include "main.h"

Table_ Table(string id, int value, Table_ tail) {
    Table_ t = checked_malloc(sizeof(t));
    t->id = id;
    t->value = value;
    t->tail = tail;
    return t;
}

IntAndTable_ IntAndTable(int value, Table_ t) {
    IntAndTable_ iat = checked_malloc(sizeof(iat));
    iat->value = value;
    iat->t = t;
    return iat;
}

Table_ interpStm(A_stm stm, Table_ t) {
    if (stm->kind == A_compoundStm) {
        return interpStm(stm->u.compound.stm2, interpStm(stm->u.compound.stm1, t));
    } else if (stm->kind == A_assignStm) {
        IntAndTable_ iat = interpExp(stm->u.assign.exp, t);
        return update(stm->u.assign.id, iat->value, iat->t);
    } else if (stm->kind == A_printStm) {
        if (stm->u.print.exps->kind == A_lastExpList) {
            printf("\n");
            return interpAndPrintExp(stm->u.print.exps->u.last, t)->t;
        } else if (stm->u.print.exps->kind == A_pairExpList) {
            A_expList list = stm->u.print.exps;
            while (list->kind == A_pairExpList) {
                t = interpAndPrintExp(list->u.pair.head, t)->t;
                list = list->u.pair.tail;
            }
            return interpAndPrintExp(list->u.last, t)->t;
        }
        exit(0);
    }
    exit(0);
}

IntAndTable_ interpExp(A_exp exp, Table_ t) {
    if (exp->kind == A_idExp) {
        return IntAndTable(lookUp(exp->u.id, t), t);
    } else if (exp->kind == A_numExp) {
        return IntAndTable(exp->u.num, t);
    } else if (exp->kind == A_opExp) {
        IntAndTable_ iatLeft = interpExp(exp->u.op.left, t);
        IntAndTable_ iatRight = interpExp(exp->u.op.right, iatLeft->t);
        switch (exp->u.op.oper) {
            case A_plus:
                return IntAndTable(iatLeft->value + iatRight->value, iatRight->t);
            case A_minus:
                return IntAndTable(iatLeft->value - iatRight->value, iatRight->t);
            case A_times:
                return IntAndTable(iatLeft->value * iatRight->value, iatRight->t);
            case A_div:
                return IntAndTable(iatLeft->value / iatRight->value, iatRight->t);
            default:
                exit(0);
        }

    } else if (exp->kind == A_eseqExp) {
        return interpExp(exp->u.eseq.exp, interpStm(exp->u.eseq.stm, t));
    }
    exit(0);
}

Table_ update(string id, int value, Table_ t) {
    return Table(id, value, t);
}

int lookUp(string key, Table_ t) {
    while (t) {
        if (strcmp(key, t->id) == 0) {
            return t->value;
        }
        t = t->tail;
    }
    return 0;
}

IntAndTable_ interpAndPrintExp(A_exp exp, Table_ t) {
    IntAndTable_ iat = interpExp(exp, t);
    printf("%d ", iat->value);
    return iat;
}

int main() {
     interpStm(prog(), NULL);
     printf("\n");
     return 0;
}

