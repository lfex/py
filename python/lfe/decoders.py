from cytoolz.functoolz import compose

from lfe import logger


def dicts(value):
    if (isinstance(value, list)
        and all([isinstance(x, tuple) for x in value])
        and all([len(x) == 2 for x in value])):
        value = dict(value)
    return value


def get_all(value):
    return compose(dicts)(value)
