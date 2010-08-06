/***** Executable and Linkable Lisp File Compilation Tool *****/

/*
  ell-compile [-x compile-time.fasl]* [-c file.lisp]*
  
  Produces FASL and CFASL files for input files.

  Arguments:

  -c file.lisp---A file to compile.
  -x compile-time.fasl---A file to load at compile-time.
*/

#define _GNU_SOURCE
#include <stdio.h>
#include <readline/readline.h>
#include <unistd.h>
#include <dlfcn.h>

#include "ellcm.h"

int
main(int argc, char *argv[])
{
    struct ellcm *cm = ellcm_init();

    opterr = 0;
    int c;
    char *faslfile, *cfaslfile;
    while ((c = getopt(argc, argv, "x:c:")) != -1) {
        switch (c) {
        case 'x':
            ellcm_compiletime_load_file(cm, optarg);
            break;
        case 'c':
            if (asprintf(&faslfile, "%s.load.fasl", optarg) == -1)
                ell_fail("allocate string");
            if (asprintf(&cfaslfile, "%s.syntax.fasl", optarg) == -1)
                ell_fail("allocate string");
            ellcm_compile_file(cm, optarg, faslfile, cfaslfile);
            free(faslfile);
            free(cfaslfile);
            break;
        default:
            ell_fail("usage");
        }
    }

    return 0;
}
