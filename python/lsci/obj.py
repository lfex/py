import importlib


def attr(obj, attr):
    return getattr(obj, attr.decode("utf-8"))


def kwargs_keys_to_strings(kwargs):
    return [(x.decode("utf-8"), y) for (x, y) in kwargs]


def decode_args(arg_list):
    new_args = []
    for arg in arg_list:
        if isinstance(arg, bytes):
            arg = arg.decode("utf-8")
        new_args.append(arg)
    return new_args


def decode_kwargs(kwarg_list):
    new_kwarg_list = []
    for (key, val) in kwarg_list:
        if isinstance(val, bytes):
            val = val.decode("utf-8")
        new_kwarg_list.append([key, val])
    return dict(kwargs_keys_to_strings(new_kwarg_list))


def call_method(obj, funcname, args=[], kwarg_list=None):
    if len(kwarg_list) == 0:
        kwarg_list = []
    kwargs = decode_kwargs(kwarg_list)
    args = decode_args(args)
    return attr(obj, funcname)(*args, **kwargs)


def call_func(modname, funcname, args=[], kwarg_list=None):
    module = importlib.import_module(modname.decode("utf-8"))
    return call_method(module, funcname, args, kwarg_list)


def call_callable(func_obj, args=[], kwarg_list=None):
    return call_method(func_obj, b"__call__", args, kwarg_list)
