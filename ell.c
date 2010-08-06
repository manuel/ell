/***** Executable and Linkable Lisp *****/

#include <getopt.h>
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
    opterr = 0;
    int c;
    char *faslfile, *cfaslfile;
    while ((c = getopt (argc, argv, "x:l:")) != -1) {
        switch (c) {
        case 'x':
            ellcm_compiletime_load_file(cm, optarg);
            break;
        case 'l':
            ellcm_load_file(cm, optarg);
            break;
        }
    }
    ell_repl(cm);
    return 0;
}
