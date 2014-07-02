#! /usr/bin/env python

import unittest

import calculator
import stream
import ui


class TestUi(unittest.TestCase):

    def setUp(self):
        self.iS = stream.Stream()
        self.oS = stream.Stream()
        self.calculator = calculator.Calculator(self.iS, self.oS)
        self.ui = ui.Ui(self.calculator)

#    def test_single_digit(self):
#        '''input stream holds one digit and a write, expect the digit to be on the stack in the end.
#        the digit is represented by an ascii code'''
#        expected = [ord("5")]
#        self.calculator.iS.write(ord("5"))
#        self.calculator.iS.write(ord("w"))
#        self.calculator.iS.write(10)
#        self.ui.runSilent()
#        self.assertEqual(expected, self.calculator.oS)
        
#    def test_single_digit(self):
#        '''input stream holds one digit and a write, expect the digit to be on the stack in the end.
#        the digit is represented by an ascii code'''
#        self.exec_ui("5w", "5")
        
    def test_multiple_digit(self):
        '''input stream holds one digit and a write, expect the digit to be on the stack in the end.
        the digit is represented by an ascii code'''
        self.exec_ui("123w", "123")
        
#    def test_no_output(self):
#        '''input stream holds one digit and a write, expect the digit to be on the stack in the end.
#        the digit is represented by an ascii code'''
#        self.exec_ui("6 7+", "")
        
#    def test_no_simple_addition(self):
#        '''input stream holds one digit and a write, expect the digit to be on the stack in the end.
#        the digit is represented by an ascii code'''
#        self.exec_ui("6 7+w", "13")
        
    def exec_ui(self, actualInput, expectedOutput):
        '''input stream holds one digit and a write, expect the digit to be on the stack in the end.
        the digit is represented by an ascii code'''
#TODO: remove debug        
        print "input stream raw: {0}".format(actualInput)
        for a in actualInput:
            self.calculator.iS.write(ord(a))
        self.calculator.iS.write(10)
        expected = map(lambda x: ord(x), expectedOutput)

#TODO: remove debug        
        print "input stream: {0}".format(self.calculator.iS)
        
        self.ui.runSilent()
        self.assertEqual(expected, self.calculator.oS)
        
if __name__ == "__main__":
    unittest.main()
