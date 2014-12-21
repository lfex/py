from erlport.erlterms import Atom

from scipy.interpolate import interpolate


def interp1d(value):
    if isinstance(value, interpolate.interp1d):
        data = (value.x, value.y, value._kind, value.axis, value.copy,
                value.bounds_error)
        value = (b"interp1d", data)
    return value
