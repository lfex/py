def attr(obj, attr):
    return getattr(obj, attr.decode("utf-8"))


def kwargs_keys_to_strings(kwargs):
    return [(x.decode("utf-8"), y) for (x, y) in kwargs]


def call(obj, funcname, args=[], kwarg_list=None):
    if len(kwarg_list) == 0:
        kwarg_list = []
    kwargs = dict(kwargs_keys_to_strings(kwarg_list))
    return attr(obj, funcname)(*args, **kwargs)
