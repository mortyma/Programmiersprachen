#! /usr/bin/env python

import unittest
import test_base


class TestExamples(test_base.TestBase, unittest.TestCase):
  
    def test_conditional_exec(self):
        code = "[9~][9][3c4d1+da]a"
        self.exec_expect_data("1" + code + "0" + code, [-9, 9])
               
    def test_3_factorial(self):
        code = "[2c1 3c-1c1=3c[][3c4d1+da]a2d*]2c3d2ca2d"
        self.exec_expect_data("3"+ code, [6])
        
        
if __name__ == "__main__":
    unittest.main()