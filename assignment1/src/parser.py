#! /usr/bin/env python

def to_ascii(s):
  codes = []
  for c in str(s):
    codes.append(ord(c))
  return codes