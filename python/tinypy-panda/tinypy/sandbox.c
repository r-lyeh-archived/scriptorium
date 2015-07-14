
#ifdef TP_NORMALMAKE
#include "tp.h"
#include "builtins_auto.h"
#include "dict_auto.h"
#include "list_auto.h"
#include "misc_auto.h"
#include "string_auto.h"
#endif

void tp_sandbox(TP, double time_limit, unsigned long mem_limit) {
    tp->time_limit = time_limit;
    tp->mem_limit = mem_limit;
}

void tp_mem_update(TP) {
/*    static long maxmem = 0;
    if (tp->mem_used/1024 > maxmem) {
        maxmem = tp->mem_used/1024; 
        fprintf(stderr,"%ld k\n",maxmem);
    }*/
    if((!tp->mem_exceeded) &&
       (tp->mem_used > tp->mem_limit) && 
       (tp->mem_limit != TP_NO_LIMIT)) {
        tp->mem_exceeded = 1;
        tp_raise(,tp_string("(tp_mem_update) SandboxError: memory limit exceeded"));
    }
} 

void tp_time_update(TP) {
    clock_t tmp = tp->clocks;
    if(tp->time_limit != TP_NO_LIMIT)
    {
        tp->clocks = clock();
        tp->time_elapsed += ((double) (tp->clocks - tmp) / CLOCKS_PER_SEC) * 1000.0;
        if(tp->time_elapsed >= tp->time_limit)
            tp_raise(,tp_string("(tp_time_update) SandboxError: time limit exceeded"));
    }
}

#ifdef TP_SANDBOX

void *tp_malloc(TP, unsigned long bytes) {
    unsigned long *ptr = (unsigned long *) calloc(bytes + sizeof(unsigned long), 1);
    if(ptr) {
        *ptr = bytes;
        tp->mem_used += bytes + sizeof(unsigned long);
    }
    tp_mem_update(tp);
    return ptr+1;
}

void tp_free(TP, void *ptr) {
    unsigned long *temp = (unsigned long *) ptr;
    if(temp) {
        --temp;
        tp->mem_used -= (*temp + sizeof(unsigned long));
        free(temp);
    }
    tp_mem_update(tp);
}

void *tp_realloc(TP, void *ptr, unsigned long bytes) {
    unsigned long *temp = (unsigned long *) ptr;
    int diff;
    if(temp && bytes) {
        --temp;
        diff = bytes - *temp;
        *temp = bytes;
        tp->mem_used += diff;
        temp = (unsigned long *) realloc(temp, bytes+sizeof(unsigned long));
        return temp+1;
    }
    else if(temp && !bytes) {
        tp_free(tp, temp);
        return NULL;
    }
    else if(!temp && bytes) {
        return tp_malloc(tp, bytes);
    }
    else {
        return NULL;
    }
}

#endif

tp_obj tp_sandbox_(TP) {
    tp_num time = TP_NUM();
    tp_num mem = TP_NUM();
    tp_sandbox(tp, time, mem);
    tp_del(tp, tp->builtins, tp_string("sandbox"));
    tp_del(tp, tp->builtins, tp_string("mtime"));
    tp_del(tp, tp->builtins, tp_string("load"));
    tp_del(tp, tp->builtins, tp_string("save"));
    tp_del(tp, tp->builtins, tp_string("system"));
    return tp_None;
}

void tp_bounds(TP, tp_code *cur, int n) {
    char *s = (char *)(cur + n);
    tp_frame_ *frame = tp_frame_current(tp);
    if( frame ) {
        tp_obj code = frame->code;
        if (s < code.string.val || s > (code.string.val+code.string.len)) {
            tp_raise(,tp_string("(tp_bounds) SandboxError: bytecode bounds reached"));
        }
    }
}
