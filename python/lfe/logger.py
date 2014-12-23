import logging


logging.basicConfig(
    format='[%(asctime)s] [lfe:py] [%(levelname)s] %(message)s',
    datefmt='%Y-%m-%d %H:%M',
    level=logging.WARN)


warn = logging.warning
info = logging.info
debug = logging.debug
err = logging.error
crit = logging.critical
