import unittest

import test_examples
import test_calculator
import test_stack

def suite(self):
    pass

if __name__ == '__main__':
    suite1 = unittest.TestLoader().loadTestsFromTestCase(test_calculator.CalculatorTest)
    suite2 = unittest.TestLoader().loadTestsFromTestCase(test_examples.ExamplesTest)
    suite3 = unittest.TestLoader().loadTestsFromTestCase(test_stack.StackTest)
    alltests = unittest.TestSuite([suite1, suite2, suite3])
    unittest.TextTestRunner(verbosity=2).run(alltests)
