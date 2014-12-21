from scipy.interpolate import interpolate


def interp1d(value):
    if isinstance(value, tuple) and len(value) == 2:
        if value[0] == b"interp1d":
            (x, y, kind, axis, copy, bounds_error,
             fill_value, assume_sorted) = value[1]
            value = interpolate.interp1d(
                x, y, kind=kind, axis=axis, copy=copy,
                bounds_error=bounds_error)
    return value

