#! /usr/bin/env python
import unittest
import calculator

class TestCalculator(unittest.TestCase):
  
  def setUp(self):
    self.calculator = calculator.Calculator()
    
  def test_printState(self):
    self.calculator.push_code("1 2 3")
    self.calculator.print_state()
    
if __name__ == '__main__':
  unittest.main()