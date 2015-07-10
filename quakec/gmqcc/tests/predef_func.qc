void main() {
#ifdef SIMPLE
    print(__FUNC__, "\n");
#elifdef CONCATENATED
    print(__FUNC__ "\n");
#else
#   error this is wrong
#endif
}
