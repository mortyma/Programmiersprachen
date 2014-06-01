#! /usr/bin/env python
import sys

# test if val is an integer 
def is_number(val):
  try:
    int(val)
    return True
  except ValueError:
    return False
  
# Parse val to an integer. 
# Print an error and exit if this is not possible.
def parse_int(val):
  return int(val)

# Test if the given value is boolean, i.e., 0 or 1
# If val is not boolean, print an error message and exit. Otherwise, this method has no effect.
def expectBool(val):
  if (val != 0 and val != 1):
    raise ValueError("expected boolean value!")

# Test if the given integer value != 0.
# If val == 0, print an error message and exit. Otherwise, this method has no effect.
def expectNonZero(val):
  if val == 0:
    raise ValueError("expected non-zero integer value!")
    
# Test if the given value is an integer.
# If val is not an integer, print an error message and exit. Otherwise, this method has no effect.
def expectInt(val):
  try: 
    int(val)
  except ValueError:
    raise ValueError("expected integer value!")
    
# Test if the given value is a positive integer and strictly smaller than other.
# If val is not an integer, print an error message and exit. Otherwise, this method has no effect.  
def expectPosIntSmallerThan(val, other):
  try: 
    int(val)
  except ValueError:
    raise ValueError("expected integer value!")
  if val < 0:
    raise ValueError("expected positive integer value!")
  if val >= other:
    msg = "expected value to be smaller than " + str(other) + " but was: " + str(val)
    raise ValueError(msg)
