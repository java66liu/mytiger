#include "prog1.h"

typedef struct table *Table_;

struct table {
    string id;
    int value;
    Table_ tail;
};

Table_ Table(string id, int value, Table_ tail);

Table_ interpStm(A_stm stm, Table_ t);

Table_ update(string id, int value, Table_ t);

typedef struct intAndTable *IntAndTable_;

struct intAndTable {
    int value;
    Table_ t;
};

IntAndTable_ IntAndTable(int value, Table_ t);

IntAndTable_ interpExp(A_exp exp, Table_ t);

IntAndTable_ interpAndPrintExp(A_exp exp, Table_ t);

int lookUp(string key, Table_ t);