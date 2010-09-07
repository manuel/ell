/***** Executable and Linkable Lisp Compilation Manager *****/

#include <dlfcn.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#include "ellcm.h"

void
ellcm_server_process(int sd, struct ellcm_tx *tx)
{
    struct ellcm_rx rx;
    if (tx->type == ELLCM_COMPILE) {
        fflush(stdout);
        rx.status = ellc_compile_file(tx->data.compile.infile,
                                      tx->data.compile.faslfile,
                                      tx->data.compile.cfaslfile);
    } else if (tx->type == ELLCM_LOAD) {
        fflush(stdout);
        dlerror();
        if (!dlopen(tx->data.load.file, RTLD_NOW | RTLD_GLOBAL)) {
            printf("%s\n", dlerror());
            ell_fail("can't load file %s\n", tx->data.load.file);
        }
        rx.status = 0;
    }
    if (rx.status == 0) {
        strcpy(rx.msg, "OK");
    } else {
        strcpy(rx.msg, "ERROR");        
    }
    char *buf = (char *) &rx;
    ssize_t bytes;
    size_t total = 0;
    while(total < sizeof(rx)) {
        bytes = write(sd, buf, sizeof(rx) - total);
        if (bytes <= 0) {
            ell_fail("write error\n");
        }
        total += bytes;
        buf += bytes;
    }
}

void
ellcm_server(int sd)
{
    struct ellcm_tx tx;
    char *buf = (char *) &tx;
    ssize_t bytes;
    size_t total = 0;
    for (;;) {
        bytes = read(sd, buf, sizeof(tx) - total);
        if (bytes <= 0) {
            ell_fail("read error\n");
        }
        total += bytes;
        buf += bytes;
        if (total == sizeof(tx)) {
            ellcm_server_process(sd, &tx);
            total = 0;
            buf = (char *) &tx;
        }
    }
}

struct ellcm *
ellcm_init()
{
    int sv[2];
    if (socketpair(AF_LOCAL, SOCK_STREAM, 0, sv) != 0) {
        ell_fail("can't create socket\n");
    }
    pid_t pid = fork();
    if (pid < 0) {
        ell_fail("fork error\n");
    }
    if (pid == 0) {
        ellcm_server(sv[1]); // never returns
        return NULL;
    } else {
        struct ellcm *cm = (struct ellcm *) ell_alloc(sizeof(*cm));
        cm->sd = sv[0];
        return cm;
    }
}

int
ellcm_compile_file(struct ellcm *cm, char *infile, char *faslfile, char *cfaslfile)
{
    struct ellcm_tx tx;
    tx.type = ELLCM_COMPILE;
    strcpy(tx.data.compile.infile, infile);
    strcpy(tx.data.compile.faslfile, faslfile);
    strcpy(tx.data.compile.cfaslfile, cfaslfile);
    char *buf = (char *) &tx;
    ssize_t bytes;
    size_t total = 0;
    while(total < sizeof(tx)) {
        bytes = write(cm->sd, buf, sizeof(tx) - total);
        if (bytes <= 0) {
            ell_fail("write error\n");
        }
        total += bytes;
        buf += bytes;
    }
    struct ellcm_rx rx;
    buf = (char *) &rx;
    total = 0;
    while(total < sizeof(rx)) {
        bytes = read(cm->sd, buf, sizeof(rx) - total);
        if (bytes <= 0) {
            ell_fail("read error\n");
        }
        total += bytes;
        buf += bytes;
    }
    return rx.status;
}

int
ellcm_compiletime_load_file(struct ellcm *cm, char *file)
{
    struct ellcm_tx tx;
    tx.type = ELLCM_LOAD;
    strcpy(tx.data.load.file, file);
    char *buf = (char *) &tx;
    ssize_t bytes;
    size_t total = 0;
    while(total < sizeof(tx)) {
        bytes = write(cm->sd, buf, sizeof(tx) - total);
        if (bytes <= 0) {
            ell_fail("write error\n");
        }
        total += bytes;
        buf += bytes;
    }
    struct ellcm_rx rx;
    total = 0;
    while(total < sizeof(rx)) {
        bytes = read(cm->sd, buf, sizeof(rx) - total);
        if (bytes <= 0) {
            ell_fail("read error\n");
        }
        total += bytes;
        buf += bytes;
    }
    return rx.status;
}

int
ellcm_compile_str(struct ellcm *cm, char *s, char *faslfile, char *cfaslfile)
{
    char *infile = ell_alloc(L_tmpnam);
    tmpnam(infile);
    FILE *in = fopen(infile, "w");
    fprintf(in, "%s", s);
    fclose(in);
    return ellcm_compile_file(cm, infile, faslfile, cfaslfile);
}

struct ell_obj *
ellcm_eval(struct ellcm *cm, char *s)
{
    char *faslfile = ell_alloc(L_tmpnam);
    char *cfaslfile = ell_alloc(L_tmpnam);
    tmpnam(faslfile);
    tmpnam(cfaslfile);
    ellcm_compile_str(cm, s, faslfile, cfaslfile);
    ell_result = ell_unspecified;
    dlerror();
    if (dlopen(faslfile, RTLD_NOW | RTLD_GLOBAL) == NULL) {
        printf("dlopen: %s\n", dlerror());
    }
    return ell_result;
}

const char ellcm_lisp_suffix[] = ".lisp";
const size_t ellcm_lisp_suffix_len = sizeof(ellcm_lisp_suffix) - 1;
const char ellcm_fasl_suffix[] = ".fasl";
const size_t ellcm_fasl_suffix_len = sizeof(ellcm_fasl_suffix) - 1;

bool
ellcm_is_source_file(struct ellcm *cm, char *file)
{
    size_t len = strlen(file);
    if (len < ellcm_lisp_suffix_len) return false;
    return 0 == strncmp(file + len - ellcm_lisp_suffix_len,
                        ellcm_lisp_suffix,
                        ellcm_lisp_suffix_len);
}

bool
ellcm_is_fasl_file(struct ellcm *cm, char *file)
{
    size_t len = strlen(file);
    if (len < ellcm_fasl_suffix_len) return false;
    return 0 == strncmp(file + len - ellcm_fasl_suffix_len,
                        ellcm_fasl_suffix,
                        ellcm_fasl_suffix_len);
}

void
ellcm_load_file(struct ellcm *cm, char *infile)
{
    fflush(stdout);
    if (ellcm_is_source_file(cm, infile)) {
        char *faslfile = ell_alloc(L_tmpnam);
        char *cfaslfile = ell_alloc(L_tmpnam);
        tmpnam(faslfile);
        tmpnam(cfaslfile);
        ellcm_compile_file(cm, infile, faslfile, cfaslfile);
        dlopen(faslfile, RTLD_NOW | RTLD_GLOBAL);
    } else if (ellcm_is_fasl_file(cm, infile)) {
        dlerror();
        if (!dlopen(infile, RTLD_NOW | RTLD_GLOBAL)) {
            printf("%s\n", dlerror());
            ell_fail("can't load file %s\n", infile);
        }
    } else {
        ell_fail("can't load file %s\n", infile);
    }
}
