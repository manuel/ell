/***** Executable and Linkable Lisp *****/

#include <stdio.h>
#include <readline/readline.h>

#include "ellc.h"

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

        struct ell_obj *result = ellc_eval(ell_parse());
        if (result != ell_unspecified) {
            ELL_SEND(result, print_object);
            printf("\n");
        }

        free(ell_line);
    }
}
