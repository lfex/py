from datetime import date, datetime, time, timedelta

from cytoolz.functoolz import compose

from lfe import logger, erlang


def dicts(value):
    dict_names = ["dict", "UserDict", "OrderedDict", "defaultdict", "ChainMap"]
    dict_types = [dict, collections.UserDict, collections.OrderedDict,
                  collections.defaultdict, collections.ChainMap]
    if (isinstance(value, list)
        and len(value) == 2
        and any([value[0] == x for x in dict_types])):
        index = dict_names.index(value[0])
        value = dict_types[index](*value[1])
    return value


def dates(value):
    if (isinstance(value, tuple)
        and len(value) == 2
        and value[0] == erlang.List("date")):
        value = date(*list(value[1]))
    return value


def datetimes(value):
    if (isinstance(value, tuple)
        and len(value) == 2
        and value[0] == erlang.List("datetime")):
        (year, month, day, hour, minute, second, micro, tz) = value[1]
        value = datetime(year, month, day, hour, minute, second, micro, tz)
    return value


def times(value):
    if (isinstance(value, tuple)
        and len(value) == 2
        and value[0] == erlang.List("time")):
        value = time(*list(value[1]))
    return value


def timedeltas(value):
    if (isinstance(value, tuple)
        and len(value) == 2
        and value[0] == erlang.List("timedelta")):
        value = timedelta(*list(value[1]))
    return value


def get_all(value):
    return compose(dicts,
                   dates,
                   datetimes,
                   times,
                   timedeltas)(value)
