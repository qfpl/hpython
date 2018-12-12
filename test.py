def b():
    a = 2
    del a
    def c():
        nonlocal a
        a = 3
    c()
    return a
print(b())
