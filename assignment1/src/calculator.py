#! /usr/bin/env python
import stack
import util
import operator

class Calculator():
  """The Calculator"""
  # constants
  NR_LINES = 25
  NR_COLUMNS = 80
    
  def __init__(self):
    # code and data stack
    self.code = stack.Stack()
    self.data = stack.Stack()
    # set of binary operators 
    #set('+-*/%&|=<>')
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
    
  # push code onto the calculator's code stack
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
      elif token in self.binaryOps.keys():
	# binary operators: {+,-,*,/,%,&,|,=,<,>}
	# pop to items from data stack 
	a = self.data.pop()
	b = self.data.pop()
	
	# get the operator
	op_func = self.binaryOps[token]
	
	# "=" does not requires a and b to be integers
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
	val = self.data[self.data.size() - idx] #get the value at stack depth n
	self.data.pop() #pop parameter n
	self.data.push(val) #push the new value
      
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
    # printing a list in python 2.7 is not that easy...
    print self.code_to_string() + " ^ " + self.data_to_string()

  def code_to_string(self):
    return ' '.join(str(p) for p in self.data)
    
  def data_to_string(self):
    return ' '.join(str(p) for p in reversed(self.code))