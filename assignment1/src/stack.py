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
  
  # Contents of the stack as a string. 
  # Top-most element is first element in the string if reverse is False (default);
  # otherwise, top-most element is the last in the string
  # Elements are separated by space
  def toString(self, reverse = False):
    if(reverse):
      l = self
    else:
      l = reversed(self)
    return ' '.join(str(p) for p in l)
 