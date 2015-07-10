#include <stdio.h>
#include <neko_vm.h>
 
value load( char *file ) {
    value loader;
    value args[2];
    value exc = NULL;
    value ret;
    loader = neko_default_loader(NULL,0);
    args[0] = alloc_string(file);
    args[1] = loader;
    ret = val_callEx(loader,val_field(loader,val_id("loadmodule")),args,2,&exc);
    if( exc != NULL ) {
        buffer b = alloc_buffer(NULL);
        val_buffer(b,exc);
        printf("Uncaught exception - %s\n",val_string(buffer_to_string(b)));
        return NULL;
    }
    return ret;
}
 
void execute( value module ) {
    value x = val_field(module,val_id("x"));
    value f = val_field(module,val_id("f"));
    value ret;
    if( !val_is_int(x) )
         return; 
    printf("x = %d\n",val_int(x));
    if( !val_is_function(f) || val_fun_nargs(f) != 1 )
         return;
    ret = val_call1(f,x);
    if( !val_is_int(ret) )
         return;
    printf("f(x) = %d\n",val_int(ret));
}
 
 
int main( int argc, char *argv[] ) {
    neko_vm *vm;
    value module;
    neko_global_init(NULL);
    vm = neko_vm_alloc(NULL);
    neko_vm_select(vm);
 
    module = load(argc > 1 ? argv[1] : "mymodule.n");
    if( module == NULL ) {
         printf("Failed to load module !\n");
         return -1;
    }
    execute(module);
 
    neko_global_free();    
    return 0;
}
