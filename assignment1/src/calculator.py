#! /usr/bin/env python
import stack

class Calculator():
  """The Calculator"""
  
  def __init__(self):
    self.NR_LINES = 25
    self.NR_COLUMNS = 80
    self.data = stack.Stack()
    self.code = stack.Stack()
  
  # push code onto the calculator's code stack
  def push_code(self, code):
    # split the code by any whitespace
    self.tokens = code.split() 
    self.tokens.reverse()
    for p in self.tokens:
      self.code.push(p)
  
  # print the state of the calculator
  # (same format as in the assignment specification)
  def print_state(self):
    # printing a list in python 2.7 is not that easy...
    print self.code_to_string() + " ^ " + self.data_to_string()

  def code_to_string(self):
    return ' '.join(str(p) for p in self.data)
    
  def data_to_string(self):
    return ' '.join(str(p) for p in reversed(self.code))