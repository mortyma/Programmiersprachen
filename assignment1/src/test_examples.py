#! /usr/bin/env python

import unittest

import calculator
import stream

class TestExamples(unittest.TestCase):
  
  def setUp(self):
    self.iS = stream.Stream()
    self.oS = stream.Stream()
    self.calculator = calculator.Calculator(self.iS, self.oS)
 
  def test_conditional_exec(self):
    code = "[9~][9][3c4d1+da]a"
    
    self.calculator.push_code("1" + code)
    self.calculator.execute()
    self.assertEqual([9], self.calculator.data)
    
    self.calculator.push_code("0" + code)
    self.calculator.execute()
    self.assertEqual([-9, 9], self.calculator.data)
     
  def test_3_factorial(self):
    code = "[2c1 3c-1c1=3c[][3c4d1+da]a2d*]2c3d2ca2d"
    self.calculator.push_code("3"+ code)
    self.calculator.execute()
    self.assertEqual([6], self.calculator.data)
if __name__ == "__main__":
  unittest.main()