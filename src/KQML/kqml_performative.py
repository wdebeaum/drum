# pykqml kqml_performative.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.
# Slightly modified to retain greater compatibility with old versions by
# William de Beaumont.

from io import BytesIO
from KQML import KQMLObject
from . import kqml_reader
from . import kqml_list
from .kqml_token import KQMLToken
from .kqml_exceptions import KQMLBadPerformativeException
from .util import safe_decode, safe_encode


class KQMLPerformative(KQMLObject):
    def __init__(self, objects):
        if not objects:
            raise KQMLBadPerformativeException('no elements for initialization')
        # If we get a string then we start a list with the string as the head
        if isinstance(objects, str):
            self.data = kqml_list.KQMLList(objects)
        elif isinstance(objects, kqml_list.KQMLList):
            self._validate(objects)
            self.data = objects

    @staticmethod
    def _validate(objects):
        if not isinstance(objects[0], KQMLToken):
            raise KQMLBadPerformativeException('list doesn\'t start ' +
                'with KQMLToken: ' + str(objects[0]))
        i = 1
        while i < len(objects):
            if not isinstance(objects[i], KQMLToken) or objects[i][0] != ':':
                raise KQMLBadPerformativeException('performative ' +
                    'element not a keyword: ' + objects[i])
            # Increment counter after keyword
            i += 1
            if i == len(objects):
                raise KQMLBadPerformativeException('missing value ' +
                    'for keyword: ' + objects[i-1])
            i += 1

    def __len__(self):
        return len(self.data)

    def head(self):
        return self.data.head()

    def get(self, keyword):
        return self.data.get(keyword)

    def gets(self, keyword):
        return self.data.gets(keyword)

    def set(self, keyword, value):
        self.data.set(keyword, value)

    def get_verb(self):
        return self.data[0].to_string()

    def get_parameter(self, keyword):
        for i, key in enumerate([d.to_string() for d in self.data[:-1]]):
            if key.lower() == keyword.lower():
                return self.data[i+1]
        return None

    def set_parameter(self, keyword, value):
        found = False
        for i, key in enumerate([d.to_string() for d in self.data[:-1]]):
            if key.lower() == keyword.lower():
                found = True
                self.data[i+1] = value
        if not found:
            self.data.add(keyword)
            self.data.add(value)

    def remove_parameter(self, keyword, value):
        for i, key in enumerate([d.to_string() for d in self.data[:-1]]):
            if key.lower() == keyword.lower():
                del self.data[i]
                del self.data[i]
                # Here we might want to continue
                return

    def sets(self, keyword, value):
        self.data.sets(keyword, value)

    def to_list(self):
        return self.data

    def write(self, out):
        self.data.write(out)

    def to_string(self):
        return safe_decode(self.data.to_string())

    @classmethod
    def from_string(cls, s):
        s = safe_encode(s)
        sreader = BytesIO(s)
        kreader = kqml_reader.KQMLReader(sreader)
        return cls(kreader.read_list())

    def __str__(self):
        return safe_decode(self.to_string())

    def __repr__(self):
        return self.data.__repr__()
