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
    rx.status = ellc_compile_file(tx->infile, tx->faslfile, tx->cfaslfile);
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
        if (bytes == -1) {
            printf("write error\n");
            exit(EXIT_FAILURE);
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
        if (bytes == -1) {
            printf("read error\n");
            exit(EXIT_FAILURE);
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
        printf("can't create socket\n");
        exit(EXIT_FAILURE);
    }
    pid_t pid = fork();
    if (pid < 0) {
        printf("fork error\n");
        exit(EXIT_FAILURE);
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
    strcpy(tx.infile, infile);
    strcpy(tx.faslfile, faslfile);
    strcpy(tx.cfaslfile, cfaslfile);
    char *buf = (char *) &tx;
    ssize_t bytes;
    size_t total = 0;
    while(total < sizeof(tx)) {
        bytes = write(cm->sd, buf, sizeof(tx) - total);
        if (bytes == -1) {
            printf("write error\n");
            exit(EXIT_FAILURE);
        }
        total += bytes;
        buf += bytes;
    }
    struct ellcm_rx rx;
    total = 0;
    while(total < sizeof(rx)) {
        bytes = read(cm->sd, buf, sizeof(rx) - total);
        if (bytes == -1) {
            printf("read error\n");
            exit(EXIT_FAILURE);
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
    dlopen(faslfile, RTLD_NOW | RTLD_GLOBAL);
    return ell_result;
}

void
ellcm_load_file(struct ellcm *cm, char *infile)
{
    char *faslfile = ell_alloc(L_tmpnam);
    char *cfaslfile = ell_alloc(L_tmpnam);
    tmpnam(faslfile);
    tmpnam(cfaslfile);
    ellcm_compile_file(cm, infile, faslfile, cfaslfile);
    dlopen(faslfile, RTLD_NOW | RTLD_GLOBAL);
}
