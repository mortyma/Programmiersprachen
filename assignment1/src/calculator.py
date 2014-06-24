#! /usr/bin/env python
import operator

from parser import *
import stack
import stream
import util



class Calculator():
  """The Calculator"""
  # constants
  NR_LINES = 25
  NR_COLUMNS = 80
    
  def __init__(self, iS, oS):
    # code and data stack
    # the stacks contain integers, characters (that represent operations) and blocks only
    self.code = stack.Stack()   
    self.data = stack.Stack()
    # input and output streams. Format: interger ascii codes (decimal).
    # TODO: for now streams are just fifo-queues
    self.iS = iS
    self.oS = oS
    #unary operators
    self.unaryOps = ["~"]
    # set of binary operators 
    self.binaryOps = {"+": operator.add,
          "-": operator.sub,
          "*": operator.mul,
          "/": self.div,
          "%": self.mod,
          "&": self.land,
          "|": self.lor,
          "<": operator.lt,
          ">": operator.gt,
          "=": self.eq
          }
    # commands
    self.commands = ["a", "c", "d", "r", "w", "b", "g", "x"]
    # union of commands, unaryOps and binaryOps
    self.validChars = self.unaryOps + self.binaryOps.keys() + self.commands
    
  # push code onto the calculator's code stack
  # commands assumed to be separated by whitespace
  # Left-most command will be at the top of the stack
  def push_code(self, codeStr):
    # split the code by any whitespace
    tokens = codeStr.split() 
    tokens.reverse()
    for p in tokens:
      self.code.push(p)
  
  # execute the "program" on the code stack
  def execute(self):
    while (not self.code.isEmpty()):
      token = self.code.pop()
      if util.is_number(token):
        # integers are simply pushed onto the data stack
        self.data.push(int(token))
      elif token == "x":
        # exit
        return
      elif token in self.binaryOps.keys():
        # binary operators: {+,-,*,/,%,&,|,=,<,>}
        # pop to items from data stack 
        a = self.data.pop()
        b = self.data.pop()
        # get the operator
        op_func = self.binaryOps[token]
        # "=" does not require a and b to be integers
        if(token != "="):
          #check that they are integers
          util.expectInt(a)
          util.expectInt(b)
          # apply the operator...
          c = int(op_func(a, b)) # some operators (i.e., "<" and ">" return "True" or "False"; so parse it to int
          # and push the result onto the data stack
          self.data.push(c)
      elif token == "~":
        # negation
        val = util.parse_int(self.data.pop())
        self.data.push(-val)  
      elif token == "c":
        # copy operator
        idx = self.data.peek() #can't use pop to get parameter n, because copy might want to use the 1st element
        util.expectPosIntSmallerThan(idx, self.data.size())
        val = self.data[idx] #get the value at stack depth n
        self.data.pop() #pop parameter n
        self.data.push(val) #push the new value
      elif token == "d":
        # delete operator
        idx = self.data.peek() #can't use pop to get parameter n, because delete might want to use the 1st element
        util.expectPosIntSmallerThan(idx, self.data.size())
        val = self.data[idx] #get the value at stack depth n
        if(idx != 0):
          self.data.remove(idx)
        self.data.pop() #pop parameter n
      elif token == "r":
        # read from input stream 
        item = chr(self.iS.read())
        # only digits, commands and operators may be read 
        if util.is_number(item):
          self.data.push(util.parse_int(item))
        elif item in self.validChars:
          self.data.push(item)
      elif token == "w":
        # make sure that the top element of the data stack is an integer or a command
        val = self.data.pop()
        if not util.is_number(val) and val not in self.validChars:
          raise ValueError("Cannot write to output stream" + val)
        for byte in to_ascii(val):
          self.oS.write(byte)
      else:
        msg = "Unknown command: " + token
        raise ValueError(msg)
      
  # operator "/"
  def div(self, a, b):
    return self.op_div(operator.div, a, b)
  
  # operator "%"
  def mod(self, a, b):
    return self.op_div(operator.mod, a, b)
  
  # Check that b is not zero; then apply op_func to a and b 
  def op_div(self, op_func, a, b):
    util.expectNonZero(b)
    return op_func(a, b)
  
  # operator "&"
  def land(self, a, b):
    return self.boolean_op(operator.and_, a, b)
 
  # operator "|"
  def lor(self, a, b):
    return self.boolean_op(operator.or_, a, b)
  
  # Check that a and b are boolean values; then apply op_func to a and b
  def boolean_op(self, op_func, a, b):
    util.expectBool(a)
    util.expectBool(b)
    return op_func(a, b)
    
  # operator "="
  # If "=" is applied to two equal blocks or two equal integers, 
  # then the result equals 1, otherwise 0
  def eq(self, a, b):
    return a == b #TODO: does this work for blocks?
    
  # Print the state of the calculator
  # (same format as in the assignment specification)
  def print_state(self):
    print self.data.toString(True) + " ^ " + self.code.toString()