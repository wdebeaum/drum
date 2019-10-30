# pykqml __init__.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.
# Slightly modified to better match the equivalent libraries in other languages
# by William de Beaumont.

__version__ = '1.0'

class KQMLObject(object):
    """This is the parent class for KQML classes representing messages."""
    pass

from .kqml_exceptions import *
from .kqml_quotation import KQMLQuotation
from .kqml_token import KQMLToken
from .kqml_string import KQMLString
from .kqml_list import KQMLList
from .kqml_performative import KQMLPerformative
from .kqml_reader import KQMLReader
from .kqml_dispatcher import KQMLDispatcher
