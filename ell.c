/***** Executable and Linkable Lisp *****/

#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <dlfcn.h>

#include "ellc.h"

__attribute__((weak)) struct ell_obj *ell_repl_fun;

static char *ell_line;
static size_t ell_len;
static size_t ell_off;

#define YY_INPUT(buf, result, max_size)                          \
    if (ell_off == ell_len) {                                    \
        result = 0;                                              \
    } else {                                                     \
        *(buf) = ell_line[ell_off++];                            \
        result = 1;                                              \
    }

#include "grammar.c"

int main()
{
    for(unsigned i = 0; ; i++) {
        ell_line = readline("> ");
        ell_len = strlen(ell_line);
        ell_off = 0;

        char filename[256];
        sprintf(filename, "./repl_%u.c", i);
        char outfilename[256];
        sprintf(outfilename, "./repl_%u.o", i);

        FILE *f;
        if (!(f = fopen(filename, "w"))) {
            printf("cannot write temp file");
            exit(EXIT_FAILURE);
        }
        
        struct ellc_st *st = ellc_make_st(f);

        struct ell_obj *stx_lst = ell_parse();

        // construct function wrapper around user-entered expression
        
        struct ellc_ast_seq *ast_seq = ellc_wrap_for_repl(ellc_norm(stx_lst));
        ellc_expl(st, ast_seq);
        ellc_emit(st, ast_seq);

        fclose(f);

        char cmdline[256];
        sprintf(cmdline, "gcc -std=c99 -shared -o %s %s", outfilename, filename);
        system(cmdline);
        if (!dlopen(outfilename, RTLD_NOW | RTLD_GLOBAL)) {
            printf("load error: %s\n", dlerror());
        }

        if (ell_repl_fun) {
            ELL_SEND(ell_call(ell_repl_fun, 0, 0, NULL), print_object);
            printf("\n");
        } else {
            printf("errorrororor\n");
        }

        free(ell_line);
    }
}
