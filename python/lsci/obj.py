def attr(obj, attr):
    return getattr(obj, attr.decode("utf-8"))


def call(obj, funcname, args=[], kwarg_list=None):
    if len(kwarg_list) == 0:
        kwarg_list = []
    kwargs = dict(kwarg_list)
    return attr(obj, funcname)(*args, **kwargs)
