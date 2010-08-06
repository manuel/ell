/***** File Compilation Tool *****/

/*
  ell-compile [file.lisp]*

  Produces FASL and CFASL files for input files.
*/

#define _GNU_SOURCE
#include <stdio.h>
#include <readline/readline.h>
#include <unistd.h>

#include "ellcm.h"

int
main(int argc, char *argv[])
{
    struct ellcm *cm = ellcm_init();
    for (int i = 1; i < argc; i++) {
        char *lisp_file = argv[i];
        char *fasl_file, *cfasl_file;
        if (asprintf(&fasl_file, "%s.load.fasl", lisp_file) == -1)
            ell_fail("allocate string\n");
        if (asprintf(&cfasl_file, "%s.syntax.fasl", lisp_file) == -1)
            ell_fail("allocate string\n");
        ellcm_compile_file(cm, lisp_file, fasl_file, cfasl_file);
        free(fasl_file);
        free(cfasl_file);
    }
    return 0;
}
