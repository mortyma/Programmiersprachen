#! /usr/bin/env python
import unittest
import stack

class TestStack(unittest.TestCase):
  
  def setUp(self):
    self.stack = stack.Stack()

  def test_basicStack(self):
    self.stack.push(1)
    self.stack.push(2)
    self.assertEqual("2 1", self.stack.toString())
    self.assertEqual(2, self.stack.pop())
    self.assertEqual(1, self.stack.pop())
    
  def test_peek(self):
    self.stack.push(1)
    self.stack.push(2)
    self.assertEqual(2, self.stack.peek())
    
if __name__ == '__main__':
  unittest.main()