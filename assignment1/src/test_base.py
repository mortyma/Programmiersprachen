#! /usr/bin/env python

import unittest

import calculator
import stream


class TestBase(unittest.TestCase):
  
    def setUp(self):
        self.iS = stream.Stream()
        self.oS = stream.Stream()
        self.iS.write(ord("*")) 
        self.iS.write(50)
        self.calculator = calculator.Calculator(self.iS, self.oS)
    
    def exec_expect_data(self, code, expected):
        ''' Execute the given code and check afterwards if data stack contains 'expected' '''
        self.exec_expect_on(code, expected, self.calculator.data)
        
    def exec_expect_code(self, code, expected):
        ''' Execute the given code and check afterwards if code stack contains 'expected' '''
        self.exec_expect_on(code, expected, self.calculator.code)
        
    def exec_expect_on(self, code, expected, stack):
        ''' Execute the given code and check afterwards if stack contains 'expected' '''
        self.calculator.push_code(code)
        self.calculator.execute()
        self.assertEqual(expected, stack)    
        
    def exec_expect_error(self, code, error):
        self.calculator.push_code(code)
        self.assertRaises(error, self.calculator.execute)        
        
if __name__ == "__main__":
    unittest.main()