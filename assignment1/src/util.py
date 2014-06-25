#! /usr/bin/env python
import sys


def is_number(val):
    '''Rest if val is an integer.'''
    try:
        int(val)
        return True
    except ValueError:
        return False
  
def expectBool(val):
    '''Test if the given value is boolean, i.e., 0 or 1.
    
    If val is not boolean, print an error message and exit. Otherwise, this 
    method has no effect.'''
    if (val != 0 and val != 1):
        raise ValueError("expected boolean value!")

def expectNonZero(val):
    '''Test if the given integer value != 0.
    
    If val == 0, print an error message and exit. Otherwise, this method has no 
    effect.'''
    if val == 0:
        raise ValueError("expected non-zero integer value!")
    
def expectInt(val):
    '''Test if the given value is an integer.
    
    If val is not an integer, print an error message and exit. Otherwise, this 
    method has no effect.'''
    try: 
        int(val)
    except ValueError:
        raise ValueError("expected integer value!")
    
def expectPosIntSmallerThan(val, other):
    '''Test if the given < other.
    
    If val is not an integer, print an error message and exit. Otherwise, return
    True iff val < other'''
    try: 
        int(val)
    except ValueError:
        raise ValueError("expected integer value!")
    if val < 0:
        raise ValueError("expected positive integer value!")
    if val >= other:
        msg = "expected value to be smaller than " + str(other) + " but was: " + str(val)
        raise ValueError(msg)
