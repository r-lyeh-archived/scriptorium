#define CAT(X, Y) X##Y
CAT(hello, world)

#define REDIR(X, Y) CAT(X, Y)
REDIR(CAT(hello, world), CAT(world, hello))

#define SCONS(X, ...) REDIR(X, __VA_ARGS__)
SCONS(hello, world)

#define FOO(X) X##X
#define BAR(X) FOO(X)FOO(X)

REDIR(BAR(hello),BAR(world))
