def get_defined_ids(__foo_code):
    __foo_comp = compile(__foo_code, "<string>", "exec")
    exec(__foo_comp)

    # can't just index foo
    __just_defined = set(["__foo_code", "__foo_comp", "__just_defined"])
    print(globals().keys())
    print(locals().keys())
    funcs = (locals().keys() - globals().keys() - __just_defined)
    if len(funcs) == 0:
        print("WARNING: no ids defined")
        return set()
    func = funcs.pop()
    func_obj = locals()[func]
    defined = func_obj.__code__.co_varnames + func_obj.__code__.co_cellvars
    return defined
