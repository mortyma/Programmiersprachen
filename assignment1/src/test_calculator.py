#! /usr/bin/env python
import unittest

import calculator
import stream

class TestCalculator(unittest.TestCase):
  
  def setUp(self):
    self.iS = stream.Stream()
    self.oS = stream.Stream()
    self.iS.write(49)
    self.iS.write(50)
    self.calculator = calculator.Calculator(self.iS, self.oS)
    
  # test pushing some code (including numbers, commands, operators and blocks
  def test_push_block(self):
    self.calculator.push_code("1 2+3c45d~<[678+]a")
    self.assertEqual([1, 2, "+", 3, "c", 45, "d", "~", "<", "[678+]", "a"], self.calculator.code)
    self.assertEqual([], self.calculator.data)
  
  # test pushing blocks
  def test_push_block_onto_data(self):
    self.calculator.push_code("[2*]")
    self.calculator.execute()
    self.assertEqual([], self.calculator.code)
    self.assertEqual(["[2*]"], self.calculator.data)
    
  def test_push_two_blocks(self):
    self.calculator.push_code("[2*][3+]")
    self.calculator.execute()
    self.assertEqual(["[3+]", "[2*]"], self.calculator.data)
    
  def test_push_block_in_block(self):
    self.calculator.push_code("[2*[3+]]")
    self.calculator.execute()
    self.assertEqual(["[2*[3+]]"], self.calculator.data)
    
  # test the application operator
  def test_apply_on_block(self):
    self.calculator.push_code("1[3-]a")
    self.calculator.execute()
    self.assertEqual([], self.calculator.code)
    self.assertEqual([2], self.calculator.data)
  
  def test_apply_on_non_block(self):
    self.calculator.push_code("3a")
    self.calculator.execute()
    self.assertEqual([], self.calculator.code)
    self.assertEqual([3], self.calculator.data)
     
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
    self.assertEqual([11, 12, "a"], self.calculator.code)
  
  # test write command
  def test_write(self):
    self.calculator.push_code("1 w 2 w")
    self.calculator.execute()
    self.assertEqual([], self.calculator.code)
    self.assertEqual([49, 50], self.calculator.oS)
    
  def test_write_invalidArg(self):
    # TODO: for this we need to be able to push blocks onto the stacks
    self.calculator.push_code("[1+]w")
    self.assertRaises(ValueError, self.calculator.execute)
  
  # test read command
  def test_read(self):
    self.calculator.push_code("r r")
    self.calculator.execute()
    self.assertEqual([2, 1], self.calculator.data)

  # test group command
  def test_group_two_blocks(self):
    self.calculator.push_code("[1+][2*]g")
    self.calculator.execute()
    self.assertEqual(["[1+2*]"], self.calculator.data)
    
  def test_group_block_and_int(self):
    self.calculator.push_code("[1*]2g")
    self.calculator.execute()
    self.assertEqual(["[1*2]" ""], self.calculator.data)
  
  def test_group_int_and_block(self):
    self.calculator.push_code("1[2*]g")
    self.calculator.execute()
    self.assertEqual(["[1 2*]"], self.calculator.data)
     
  def test_group_two_ints(self):
    self.calculator.push_code("1 2g")
    self.calculator.execute()
    self.assertEqual(["[1 2]"], self.calculator.data)
    
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
    
  # print the calculator's state before and after execution
  def debug_exec(self):
    print "Before:"
    self.calculator.print_state()
    self.calculator.execute()
    print "After:"
    self.calculator.print_state()
 
    
if __name__ == '__main__':
  unittest.main()