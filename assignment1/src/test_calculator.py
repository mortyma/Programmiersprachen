#! /usr/bin/env python
import unittest
import test_base


import calculator
import stream

class TestCalculator(test_base.TestBase, unittest.TestCase):
        
    # test pushing some code (including numbers, commands, operators and blocks
    def test_push_block(self):
        self.calculator.push_code("1 2+3c45d~<[678+]a")
        self.assertEqual([1, 2, "+", 3, "c", 45, "d", "~", "<", "[678+]", "a"], self.calculator.code)
        self.assertEqual([], self.calculator.data)
  
    # test pushing blocks
    def test_push_block_onto_data(self):
        self.exec_expect_data("[2*]", ["[2*]"])
        
    def test_push_two_blocks(self):
        self.exec_expect_data("[2*][3+]", ["[3+]", "[2*]"])
        
    def test_push_block_in_block(self):
        self.exec_expect_data("[2*[3+]]", ["[2*[3+]]"])
        
    # test the application operator
    def test_apply_on_block(self):
        self.exec_expect_data("1[3-]a", [2])
    
    def test_apply_on_non_block(self):
        self.exec_expect_data("3a", [3])
        
    # test copy operator
    def test_copy0(self):
        self.exec_expect_data("11 12 13 14 0 c", [0, 14, 13, 12, 11])
        
    def test_copy1(self):
        self.exec_expect_data("11 12 13 14 1 c", [14, 14, 13, 12, 11])
        
    def test_copy3(self):
        self.exec_expect_data("11 12 13 14 3 c", [12, 14, 13, 12, 11])
        
    def test_copy_idxOutOrRange(self):
        self.exec_expect_error("11 12 13 14 42 c", ValueError)
        
    # test delete operator
    def test_delete0(self):
        self.exec_expect_data("11 12 13 14 0 d", [14, 13, 12, 11])
        
    def test_delete1(self):
        self.exec_expect_data("11 12 13 14 1 d", [13, 12, 11])
        
    def test_delete4(self):
        self.exec_expect_data("11 12 13 14 4 d", [14, 13, 12])
    
    def test_delete_idxOutOrRange(self):
        self.exec_expect_error("11 12 13 14 42 d", ValueError)

    # test exit command "x"
    def test_exit(self):
        self.exec_expect_code("x 11 12 a", [11, 12, "a"])
    
    # test write command
    def test_write(self):
        self.exec_expect_code("1 w 2 w", [])
        self.assertEqual([49, 50], self.calculator.oS)
        
    def test_write_invalidArg(self):
        self.exec_expect_error("[1+]w", ValueError)
    
    # test read command
    def test_read(self):
        self.exec_expect_data("r r", [2, "*"])

    # test group command
    def test_group_two_blocks(self):
        self.exec_expect_data("[1+][2*]g", ["[1+2*]"])
        
    def test_group_block_and_int(self):
        self.exec_expect_data("[1*]2g", ["[1*2]" ""])
    
    def test_group_int_and_block(self):
        self.exec_expect_data("1[2*]g", ["[1 2*]"])
        
    def test_group_two_ints(self):
        self.exec_expect_data("1 2g", ["[1 2]"])
        
    def test_group_example1(self):
        #"1 3ga-" has to give the same as "1 3-", i.e., 1 (TODO: recheck this)
        self.calculator.push_code("1 3g")
        self.calculator.execute()
        self.assertEqual(["[1 3]"], self.calculator.data)
        self.calculator.push_code("a")
        self.calculator.execute()
        self.assertEqual([3, 1], self.calculator.data)
        self.calculator.push_code("-")
        self.calculator.execute()
        self.assertEqual([2], self.calculator.data)
    
    # text build
    def test_build_block_from_block(self):
        self.exec_expect_data("[2*]b", ["[[2*]]"])
    
    def test_build_block_from_operator(self):
        self.exec_expect_data("rb", ["[*]"])
        
    def test_build_block_from_int(self):
        self.exec_expect_error("1b", ValueError)
    
    # print the calculator's state before and after execution
    def debug_exec(self):
        print "Before:"
        self.calculator.print_state()
        self.calculator.execute()
        print "After:"
        self.calculator.print_state()
    
        
if __name__ == '__main__':
    unittest.main()