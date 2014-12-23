import collections
from datetime import date, datetime, time, timedelta

from cytoolz.functoolz import compose

from lfe import logger


def dicts(value):
    dict_types = [dict, collections.UserDict, collections.OrderedDict,
                  collections.defaultdict, collections.ChainMap]
    if any([isinstance(value, x) for x in dict_types]):
        value = (type(value).__name__, [x for x in value.items()])
    return value


def dates(value):
    if isinstance(value, date):
        value = ("date", (value.year, value.month, value.day))
    return value


def datetimes(value):
    if isinstance(value, datetime):
        value = ("datetime", (value.year, value.month, value.day,
                               value.hour, value.minute, value.second,
                               value.microsecond, value.tzinfo))
    return value


def times(value):
    if isinstance(value, time):
        value = ("time", (value.hour, value.minute, value.second,
                           value.microsecond, value.tzinfo))
    return value


def timedeltas(value):
    if isinstance(value, timedelta):
        value = ("timedelta", value.days, value.seconds, value.microseconds)
    return value


def get_all(value):
    return compose(dicts,
                   dates,
                   datetimes,
                   times,
                   timedeltas)(value)
