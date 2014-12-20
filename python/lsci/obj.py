import importlib


def attr(obj, attr):
    return getattr(obj, attr.decode("utf-8"))


def kwargs_keys_to_strings(kwargs):
    return [(x.decode("utf-8"), y) for (x, y) in kwargs]


def call_method(obj, funcname, args=[], kwarg_list=None):
    if len(kwarg_list) == 0:
        kwarg_list = []
    new_kwarg_list = []
    for (key, val) in kwarg_list:
        if isinstance(val, bytes):
            val = val.decode("utf-8")
        new_kwarg_list.append([key, val])
    kwargs = dict(kwargs_keys_to_strings(new_kwarg_list))
    new_args = []
    for arg in args:
        if isinstance(arg, bytes):
            arg = arg.decode("utf-8")
        new_args.append(arg)
    return attr(obj, funcname)(*new_args, **kwargs)


def call_func(modname, funcname, args=[], kwarg_list=None):
    module = importlib.import_module(modname.decode("utf-8"))
    return call_method(module, funcname, args, kwarg_list)


def call_callable(func, args=[], kwarg_list=None):
    return call_method(func, "__call__", args, kwarg_list)
