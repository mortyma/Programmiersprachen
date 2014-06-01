#! /usr/bin/env python
class Stack(list):
  """A simple stack"""
 
  # 
  def push(self, item):
    self.append(item)
  
  # 
  def isEmpty(self):
    return not self
  
  #
  def size(self):
    return len(self)
  
  #return the item on top of the stack, without popping it
  def peek(self):
    return self[-1] # python magic: gets the last element in the list, which is the top element of the stack
    
  #pop is in list
 