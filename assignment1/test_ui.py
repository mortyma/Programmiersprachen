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
        
    def test_single_digit_W(self):
        '''input stream holds one digit and a number write, expect the digit to be on the stack in the end.
        the digit is represented by an ascii code'''
        self.exec_ui_conv_os("5W", "5") #5W = [53, 87, 10]
        
    def test_single_digit_w(self):
        '''input stream holds one digit and a char write, expect the digit to be on the stack in the end.
        the digit is represented by an ascii code'''
        self.exec_ui_plain_os("5w", [5]) #5w = [53, 119, 10]
        
    def test_composed_char(self):
        '''input stream holds a number and a char write, expect to return the number as ascii code = z'''
        self.exec_ui_conv_os("122w", "z")
        
    def test_composed_number(self):
        '''input stream holds a number and a char write, expect to return the number as ascii code = z'''
        self.exec_ui_conv_os("122W", "122")
        
    def test_no_output(self):
        '''s simple addition, but no write operation'''
        self.exec_ui_conv_os("6 7+", "")
        
    def test_simple_addition_W(self):
        '''simple addition followed by a number write, expected the number 13, [49,51] as ascii'''
        self.exec_ui_conv_os("6 7+W", "13") # = [54,32,55,43,87,10]
        
    def test_simple_addition_w(self):
        '''simple addition followed by a char write, expected the ascii code 13'''
        self.exec_ui_plain_os("6 7+w", [13])

    def test_simple_block(self):
        '''simple addition followed by a char write, expected the ascii code 13'''
        self.exec_ui_plain_os("[6w]a", [6]) # = [91,54,119,93,97]

    def test_subtraction_block(self):
        '''simple addition followed by a char write, expected the ascii code 13'''
        self.exec_ui_plain_os("[6 7-w]a", [1])

    def test_nested_block(self):
        '''simple addition followed by a char write, expected the ascii code 13'''
        self.exec_ui_plain_os("[[7w]a]a", [7])

    def test_complex_hello(self):
        '''some calculation, expect the return string "hello"'''
        self.exec_ui_conv_os("26 4*w 101w [100 8+]1cawaw 111w", "hello") # = 050 054 032 052 042 119 032 049 048 049 119 032 091 049 048 048 032 056 043 093 049 099 097 050 099 051 100 097 032 049 049 049 119

    def exec_ui_conv_os(self, actualInput, expectedOutput):
        '''input stream holds one digit and a write, expect the digit to be on the stack in the end.
        the digit is represented by an ascii code. output is converted to char values'''
        expected = map(lambda x: ord(x), expectedOutput)
        self.exec_ui_plain_os(actualInput, expected)

    def exec_ui_plain_os(self, actualInput, expectedOutput):
        '''input stream holds one digit and a write, expect the digit to be on the stack in the end.
        the digit is represented by an ascii code'''
        for a in actualInput:
            self.calculator.iS.write(ord(a))
        self.calculator.iS.write(10)
        self.ui.runSilent()
        self.assertEqual(expectedOutput, self.calculator.oS)
        
if __name__ == "__main__":
    unittest.main()
