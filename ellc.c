#include "ellc.h"

#include "grammar.c"

/**** Main ****/

int main()
{
    struct ellc_st *st = (struct ellc_st *) ell_alloc(sizeof(*st));
    st->globals = ell_util_make_list();
    st->lambdas = ell_util_make_list();

    struct ell_obj *stx_lst = ell_parse();
    struct ellc_ast_seq *ast_seq = ellc_norm(stx_lst);
    ellc_expl(st, ast_seq);
    ellc_emit(st, ast_seq);
}
