#include "main.h"

Table_ increase(Table_ t) {
    t->count++;
    return t;
}

Table_ findStm(A_stm s, Table_ t) {
    if (s->kind == A_compoundStm) {
        return findStm(s->u.compound.stm2, findStm(s->u.compound.stm1, t));
    } else if (s->kind == A_assignStm) {
        return findExp(s->u.assign.exp, t);
    } else if (s->kind == A_printStm) {
        return findExpList(s->u.print.exps, increase(t));
    }
    return t;
}

Table_ findExp(A_exp e, Table_ t) {
    if (e->kind == A_opExp) {
        return findExp(e->u.op.left, findExp(e->u.op.right, t));
    } else if (e->kind == A_eseqExp) {
        return findExp(e->u.eseq.exp, findStm(e->u.eseq.stm, t));
    }
    return t;
}

Table_ findExpList(A_expList e, Table_ t) {
    if (e->kind == A_pairExpList) {
        return findExp(e->u.pair.head, findExpList(e->u.pair.tail, t));
    } else if (e->kind == A_lastExpList) {
        return findExp(e->u.last, t);
    }
    return t;
}

int main() {
    Table_ tt = checked_malloc(sizeof(tt));
    tt->count = 0;
    printf("%d", findStm(prog(), tt)->count);
    return 0;
}

