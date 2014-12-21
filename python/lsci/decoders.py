from scipy.interpolate import interpolate

from lsci import logger


def interp1d(value):
    if isinstance(value, tuple) and len(value) == 2:
        if value[0] == b"interp1d":
            logger.debug(value)
            (x, y, kind, axis, copy, bounds_error) = value[1]
            kind = kind.decode("utf-8")
            value = interpolate.interp1d(
                x, y, kind=kind, axis=axis, copy=copy,
                bounds_error=bounds_error)
    return value

