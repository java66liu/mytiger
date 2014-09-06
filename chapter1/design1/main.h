#include "prog1.h"

typedef struct table *Table_;

struct table {
    int count;
    Table_ t;
};

Table_ findStm(A_stm s, Table_ t);
Table_ findExp(A_exp e, Table_ t);
Table_ findExpList(A_expList e, Table_ t);
