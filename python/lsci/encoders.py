from erlport.erlterms import Atom

from scipy.interpolate import interpolate

from lsci import logger


def interp1d(value):
    if isinstance(value, interpolate.interp1d):
        logger.debug(value)
        kind = bytes(value._kind, "utf-8")
        data = (value.x, value.y, kind, value.axis, value.copy,
                value.bounds_error)
        value = (b"interp1d", data)
    return value
