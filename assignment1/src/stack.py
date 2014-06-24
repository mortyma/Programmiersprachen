#! /usr/bin/env python
class Stack(list):
  """A simple stack. Top-most element is first element in the list."""
 
  # 
  def push(self, item):
    self.insert(0, item)
  
  # 
  def isEmpty(self):
    return not self
  
  #
  def size(self):
    return len(self)
  
  # return the item on top of the stack, without popping it
  def peek(self):
    return self[0]
  
  # remove element at position idx from the stack.
  def remove(self, idx):
    return super(Stack, self).pop(idx)
    
  # pop is in list
  def pop(self):
    return super(Stack, self).pop(0)
  
  # Contents of the stack as a string. 
  # Top-most element is first element in the string if reverse is False (default);
  # otherwise, top-most element is the last in the string
  # Elements are separated by space
  def toString(self, reverse = False):
    if(reverse):
      l = reversed(self)
    else:
      l = self
    return ' '.join(str(p) for p in l)
 