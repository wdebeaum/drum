# pykqml kqml_list.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.
# Slightly modified to retain greater compatibility with old versions by
# William de Beaumont.

from io import BytesIO
from . import KQMLObject
from .kqml_token import KQMLToken
from .kqml_string import KQMLString
import KQML.kqml_reader as kqml_reader
from .util import safe_decode, safe_encode


class KQMLList(KQMLObject):
    def __init__(self, objects=None):
        self.data = []
        # If no objects are passed, we start with an empty list
        if objects is None:
            objects = []
        # If a list is passed, the elements are one-by-one added to the list
        if isinstance(objects, list):
            for o in objects:
                self.append(o)
        # If a string is passed, it becomes the "head" of the list
        elif isinstance(objects, str):
            self.append(objects)

    def __str__(self):
        return safe_decode('(' + ' '.join([d.__str__()
                                           for d in self.data]) + ')')

    def __repr__(self):
        return '(' + ' '.join([d.__repr__() for d in self.data]) + ')'

    def __getitem__(self, *args):
        return self.data.__getitem__(*args)

    def __len__(self):
        return len(self.data)

    def add(self, obj):
        self.append(obj)

    def head(self):
        """Return the first element of the list as string.

        Example:
            kl = KQMLList.from_string('(FAILURE :reason INVALID_PARAMETER)')
            kl.head() # "FAILURE"
        """
        return self.data[0].to_string()

    def get(self, keyword):
        """Return the element of the list after the given keyword.

        Parameters
        ----------
        keyword : str
            The keyword parameter to find in the list.
            Putting a colon before the keyword is optional, if no colon is
            given, it is added automatically (e.g. "keyword" will be found as
            ":keyword" in the list).

        Returns
        -------
        obj : KQMLObject
            The object corresponding to the keyword parameter

        Example:
            kl = KQMLList.from_string('(FAILURE :reason INVALID_PARAMETER)')
            kl.get('reason') # KQMLToken('INVALID_PARAMETER')
        """
        if not keyword.startswith(':'):
            keyword = ':' + keyword
        for i, s in enumerate(self.data):
            if s.to_string().upper() == keyword.upper():
                if i < len(self.data)-1:
                    return self.data[i+1]
                else:
                    return None
        return None

    def gets(self, keyword):
        """Return the element of the list after the given keyword as string.

        Parameters
        ----------
        keyword : str
            The keyword parameter to find in the list.
            Putting a colon before the keyword is optional, if no colon is
            given, it is added automatically (e.g. "keyword" will be found as
            ":keyword" in the list).

        Returns
        -------
        obj_str : str
            The string value corresponding to the keyword parameter

        Example:
            kl = KQMLList.from_string('(FAILURE :reason INVALID_PARAMETER)')
            kl.gets('reason') # 'INVALID_PARAMETER'
        """
        param = self.get(keyword)
        if param is not None:
            return safe_decode(param.string_value())
        return None

    def append(self, obj):
        """Append an element to the end of the list.

        Parameters
        ----------
        obj : KQMLObject or str
            If a string is passed, it is instantiated as a
            KQMLToken before being added to the list.
        """
        if isinstance(obj, str):
            obj = KQMLToken(obj)
        self.data.append(obj)

    def push(self, obj):
        """Prepend an element to the beginnging of the list.

        Parameters
        ----------
        obj : KQMLObject or str
            If a string is passed, it is instantiated as a
            KQMLToken before being added to the list.
        """
        if isinstance(obj, str):
            obj = KQMLToken(obj)
        self.data.insert(0, obj)

    def insert_at(self, index, obj):
        """Add an element to list at a given position.

        Parameters
        ----------
        obj : KQMLObject or str
            If a string is passed, it is instantiated as a
            KQMLToken before being added to the list.
        index : int
            The index to insert the element at
        """
        self.data.insert(index, obj)

    def remove_at(self, index):
        """Delete the element of the list at the given position.

        Parameters
        ----------
        index : int
            The position to remove the element at.
        """
        del self.data[index]

    def nth(self, n):
        return self.data[n] 

    def set(self, keyword, value):
        """Set the element of the list after the given keyword.

        Parameters
        ----------
        keyword : str
            The keyword parameter to find in the list.
            Putting a colon before the keyword is optional, if no colon is
            given, it is added automatically (e.g. "keyword" will be found as
            ":keyword" in the list).

        value : KQMLObject or str
            If the value is given as str, it is instantiated as a KQMLToken

        Example:
            kl = KQMLList.from_string('(FAILURE)')
            kl.set('reason', 'INVALID_PARAMETER')
        """
        if not keyword.startswith(':'):
            keyword = ':' + keyword
        if isinstance(value, str):
            value = KQMLToken(value)
        if isinstance(keyword, str):
            keyword = KQMLToken(keyword)
        found = False
        for i, key in enumerate(self.data):
            if key.to_string().lower() == keyword.lower():
                found = True
                if i < len(self.data)-1:
                    self.data[i+1] = value
                break
        if not found:
            self.data.append(keyword)
            self.data.append(value)

    def remove(self, obj):
        self.data.remove(obj)

    def sets(self, keyword, value):
        """Set the element of the list after the given keyword as string.

        Parameters
        ----------
        keyword : str
            The keyword parameter to find in the list.
            Putting a colon before the keyword is optional, if no colon is
            given, it is added automatically (e.g. "keyword" will be found as
            ":keyword" in the list).

        value : str
            The value is instantiated as KQMLString and added to the list.

        Example:
            kl = KQMLList.from_string('(FAILURE)')
            kl.sets('reason', 'this is a custom string message, not a token')
        """
        if isinstance(value, str):
            value = KQMLString(value)
        self.set(keyword, value)

    def write(self, out):
        full_str = '(' + ' '.join([str(s) for s in self.data]) + ')'
        out.write(full_str.encode())

    def to_string(self):
        out = BytesIO()
        self.write(out)
        return safe_decode(out.getvalue())

    @classmethod
    def from_string(cls, s):
        s = safe_encode(s)
        sreader = BytesIO(s)
        kreader = kqml_reader.KQMLReader(sreader)
        return kreader.read_list()

    def sublist(self, from_idx, to_idx=None):
        if to_idx is None:
            to_idx = len(self)
        return KQMLList(self.data[from_idx:to_idx])

    def index_of(self, obj):
        if isinstance(obj, str):
            return self.index_of_string(obj)
        else:
            try:
                idx = self.data.index(obj)
                return idx
            except ValueError:
                return -1

    def index_of_ignore_cae(self, keyword):
        for i, s in enumerate(self.data):
            if s.lower() == keyword.lower():
                return i
        return -1

    def index_of_string(self, s):
        try:
            idx = self.data.index(s)
            return idx
        except ValueError:
            return -1

