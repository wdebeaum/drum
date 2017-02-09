# pykqml kqml_exceptions.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.

class KQMLException(Exception):
    pass

class KQMLBadCharacterException(KQMLException):
    pass

class KQMLBadCloseException(KQMLException):
    pass

class KQMLBadCommaException(KQMLException):
    pass

class KQMLBadHashException(KQMLException):
    pass

class KQMLBadOpenException(KQMLException):
    pass

class KQMLBadPerformativeException(KQMLException):
    pass

class KQMLExpectedListException(KQMLException):
    pass

class KQMLExpectedWhitespaceException(KQMLException):
    pass

