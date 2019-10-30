# pykqml util.py
# Copyright (c) 2016, Benjamin M. Gyori
# From the pykqml library available at:
# https://github.com/bgyori/pykqml
# Relicensed under GPL 2+ (same as TRIPS) with permission.

def safe_decode(txt):
    """Return decoded text if it's not already bytes."""
    try:
        return txt.decode()
    except AttributeError:
        return txt


def safe_encode(txt):
    """Return encoded text if it's not already str."""
    try:
        return txt.encode()
    except AttributeError:
        return txt
