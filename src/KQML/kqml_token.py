# pykqml kqml_token.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.
# Slightly modified to better match the equivalent libraries in other
# languages, and to retain greater compatibility with old versions, by William
# de Beaumont.

import re
from KQML import KQMLObject

class KQMLToken(KQMLObject):
    def __init__(self, s=None):
        if s is None:
            self.data = ''
        else:
            self.data = s

    def length(self):
        return len(self.data)

    def __len__(self):
        return len(self.data)

    def equals_ignore_case(self, s):
        if isinstance(s, KQMLToken) or isinstance(s, basestring):
            return (self.data.lower() == s.lower())

    def lower(self):
        return self.data.lower()

    def upper(self):
        return self.data.upper()

    def write(self, out):
        out.write(self.data)

    def to_string(self):
        return self.data

    def string_value(self):
        return self.data

    def has_package(self):
        pkg = self.get_package()
        return (pkg is not None)

    def get_package(self):
        package, bare_name = self.parse_package()
        return package

    def get_name(self):
        package, bare_name = self.parse_package()
        return bare_name

    def is_keyword(self):
        return self.data.startswith(':')

    def parse_package(self):
        g1 = re.match('([^:]+)::([^:]+)$', self.data)
        g2 = re.match('([^:]+)::(\|[^\|]*\|)$', self.data)
        if g1:
            package, bare_name = g1.groups()
        elif g2:
            package, bare_name = g2.groups()
        else:
            package, bare_name = (None, self.data)
        return package, bare_name

    def __getitem__(self, *args):
        return self.data.__getitem__(*args)

    def __str__(self):
        return self.to_string()

    def __repr__(self):
        return self.to_string()

    def __eq__(self, other):
        if isinstance(other, KQMLToken):
            return self.data == other.data
        else:
            return self.data == other
