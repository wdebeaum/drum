# pykqml kqml_string.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.
# Slightly modified to retain greater compatibility with old versions by
# William de Beaumont.

from io import BytesIO
from KQML import KQMLObject
from .util import safe_decode


class KQMLString(KQMLObject):
    def __init__(self, data=None):
        if data is None:
            self.data = ''
        else:
            self.data = safe_decode(data)

    def length(self):
        return len(self.data)

    def __len__(self):
        return len(self.data)

    def char_at(self, n):
        return self.data[n]

    def equals(self, obj):
        if not isinstance(obj, KQMLString):
            return False
        else:
            return obj.data == self.data

    def write(self, out):
        out.write(b'"')
        for ch in self.data:
            if ch == '"':
                out.write(b'\\')
            out.write(ch.encode())
        out.write(b'"')

    def to_string(self):
        out = BytesIO()
        self.write(out)
        return safe_decode(out.getvalue())

    def string_value(self):
        return self.data

    def __str__(self):
        return safe_decode(self.to_string())

    def __repr__(self):
        s = self.__str__()
        s = s.replace('\n', '\\n')
        return s

    def __getitem__(self, *args):
        return self.data.__getitem__(*args)
