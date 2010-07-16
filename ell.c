/***** Executable and Linkable Lisp *****/

#include <stdio.h>
#include <readline/readline.h>
#include <unistd.h>

#include "ellcm.h"

static void
ell_repl(struct ellcm *cm)
{
    for(;;) {
        char *line = readline("> ");
        struct ell_obj *result = ellcm_eval(cm, line);
        if (result != ell_unspecified) {
            ELL_SEND(result, print_object);
            printf("\n");
        }
        free(line);
    }
}

int
main(int argc, char *argv[])
{
    struct ellcm *cm = ellcm_init();
    for (int i = 1; i < argc; i++) {
        ellcm_load_file(cm, argv[i]);
    }
    ell_repl(cm);
    return 0;
}
