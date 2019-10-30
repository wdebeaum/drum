# pykqml kqml_quotation.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.

from io import StringIO
from KQML import KQMLObject


class KQMLQuotation(KQMLObject):
    def __init__(self, quote_type, kqml_object):
        self.quote_type = quote_type
        self.kqml_object = kqml_object

    def get_type(self):
        return self.quote_type

    def get_object(self):
        return self.kqml_object

    def write(self, out):
        out.write(self.quote_type)
        self.kqml_object.write(out)

    def to_string(self):
        out = StringIO()
        self.write(out)
        return out.getvalue()
