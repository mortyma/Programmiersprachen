#! /usr/bin/env python
import unittest
import calculator

class TestCalculator(unittest.TestCase):
  
  def setUp(self):
    self.calculator = calculator.Calculator()
    
  # 
  def test_copy1(self):
    self.calculator.push_code("11 12 13 14 1 c")
    self.calculator.execute()
    self.assertEqual(5, self.calculator.data.size())
    self.assertEqual(1, self.calculator.data.pop())
    
  def test_copy3(self):
    self.calculator.push_code("11 12 13 14 3 c")
    self.calculator.execute()
    self.assertEqual(5, self.calculator.data.size())
    self.assertEqual(13, self.calculator.data.pop())
    
  # print the calculator's state before and after execution
  def debug_exec(self):
    print "Before:"
    self.calculator.print_state()
    self.calculator.execute()
    print "After:"
    self.calculator.print_state()
 
    
if __name__ == '__main__':
  unittest.main()