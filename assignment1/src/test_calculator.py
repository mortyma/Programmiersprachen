#! /usr/bin/env python
import unittest
import calculator

class TestCalculator(unittest.TestCase):
  
  def setUp(self):
    self.calculator = calculator.Calculator()
    
  # test copy operator
  def test_copy0(self):
    self.calculator.push_code("11 12 13 14 0 c")
    self.calculator.execute()
    self.assertEqual([0, 14, 13, 12, 11], self.calculator.data)
    
  def test_copy1(self):
    self.calculator.push_code("11 12 13 14 1 c")
    self.calculator.execute()
    self.assertEqual([14, 14, 13, 12, 11], self.calculator.data)
    
  def test_copy3(self):
    self.calculator.push_code("11 12 13 14 3 c")
    self.calculator.execute()
    self.assertEqual([12, 14, 13, 12, 11], self.calculator.data)
    
  def test_copy_idxOutOrRange(self):
    self.calculator.push_code("11 12 13 14 42 c")
    self.assertRaises(ValueError, self.calculator.execute)
    
  # test delete operator
  def test_delete0(self):
    self.calculator.push_code("11 12 13 14 0 d")
    self.calculator.execute()
    self.assertEqual([14, 13, 12, 11], self.calculator.data)
    
  def test_delete1(self):
    self.calculator.push_code("11 12 13 14 1 d")
    self.calculator.execute()
    self.assertEqual([13, 12, 11], self.calculator.data)
    
  def test_delete4(self):
    self.calculator.push_code("11 12 13 14 4 d")
    self.calculator.execute()
    self.assertEqual([14, 13, 12], self.calculator.data)
  
  def test_delete_idxOutOrRange(self):
    self.calculator.push_code("11 12 13 14 42 d")
    self.assertRaises(ValueError, self.calculator.execute)

  # test exit command "x"
  def test_exit(self):
    self.calculator.push_code("x 11 12 a")
    self.calculator.execute()
    self.assertEqual(["11", "12", "a"], self.calculator.code)
  
  # print the calculator's state before and after execution
  def debug_exec(self):
    print "Before:"
    self.calculator.print_state()
    self.calculator.execute()
    print "After:"
    self.calculator.print_state()
 
    
if __name__ == '__main__':
  unittest.main()