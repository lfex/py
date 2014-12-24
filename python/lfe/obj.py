import importlib

from lfe import decoders, erlang, logger


def init():
    "Set up module in ErlPort Python namespace."


def echo(*args, **kwargs):
    "Useful for debugging module-level issues."
    return (args, kwargs)


def get_module(modname):
    modname = modname.decode("utf-8")
    try:
        module = importlib.import_module(modname)
    except ImportError as err:
        parts = modname.split(".")
        modname = parts[0]
        submod = parts[1]
        try:
            parent_module = importlib.import_module(modname)
            module = attr(parent_module, bytes(submod, "utf-8"))
        except:
            raise(err)
    return module


def attr(obj, attr_name):
    return getattr(obj, attr_name.decode("utf-8").replace("-", "_"))


def const(modname, attr_name):
    return attr(get_module(modname), attr_name)


def kwargs_keys_to_strings(kwargs):
    return [(x.decode("utf-8"), y) for (x, y) in kwargs]


def decode_args(arg_list):
    new_args = []
    for arg in arg_list:
        if isinstance(arg, bytes):
            arg = arg.decode("utf-8")
        arg = decoders.decode(arg)
        new_args.append(arg)
    return new_args


def decode_kwargs(kwarg_list):
    new_kwarg_list = []
    for (key, val) in kwarg_list:
        if isinstance(val, bytes):
            val = val.decode("utf-8")
        val = decoders.decode(val)
        new_kwarg_list.append([key, val])
    return dict(kwargs_keys_to_strings(new_kwarg_list))


def call_method(obj, funcname, args=[], kwarg_list=None):
    if len(kwarg_list) == 0:
        kwarg_list = []
    kwargs = decode_kwargs(kwarg_list)
    args = decode_args(args)
    return attr(obj, funcname)(*args, **kwargs)


def call_func(modname, funcname, args=[], kwarg_list=None):
    module = get_module(modname)
    return call_method(module, funcname, args, kwarg_list)


def call_callable(func_obj, args=[], kwarg_list=None):
    return call_method(func_obj, b"__call__", args, kwarg_list)
