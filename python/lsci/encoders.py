from erlport.erlterms import Atom

from scipy.interpolate import interpolate

from cytoolz.functoolz import compose

from lsci import logger


def dicts(value):
    if isinstance(value, dict):
        value = [x for x in value.items()]
    return value


def interp1d(value):
    if isinstance(value, interpolate.interp1d):
        logger.debug(value)
        kind = bytes(value._kind, "utf-8")
        data = (value.x, value.y, kind, value.axis, value.copy,
                value.bounds_error)
        value = (b"interp1d", data)
    return value


def get_all(value):
    return compose(dicts,
                   interp1d)(value)
