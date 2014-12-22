from scipy.interpolate import interpolate

import numpy as np

from cytoolz.functoolz import compose

from lsci import logger


def floats(value):
    float_types = [np.float16, np.float32, np.float64, np.float128, np.float]
    if any([isinstance(value, type) for type in float_types]):
        value = float(value)
    return value


def ints(value):
    int_types = [np.int8, np.int16, np.int32, np.int64,
                 np.uint8, np.uint16, np.uint32, np.uint64]
    if any([isinstance(value, type) for type in int_types]):
        value = int(value)
    return value


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


def all(value):
    return compose(#floats,
                   #ints,
                   interp1d)(value)
