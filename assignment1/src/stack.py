#! /usr/bin/env python
class Stack(list):
  """A simple stack"""
 
  # 
  def push(self, item):
    self.append(item)
  
  # 
  def isEmpty(self):
    return not self
    
  #pop is in list
 