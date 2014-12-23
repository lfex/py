from cytoolz.functoolz import compose

from lfe import logger


def dicts(value):
    if isinstance(value, dict):
        value = [x for x in value.items()]
    return value


def get_all(value):
    return compose(dicts)(value)
