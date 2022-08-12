def foo(x):
    if x == 0:
        return False
    else:
        return 1 + [baz(x)]
x = 0
foo(x)
