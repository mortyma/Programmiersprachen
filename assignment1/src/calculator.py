#! /usr/bin/env python
import operator
import sys

from parser import *
import stack
import stream
import util



class Calculator():
    """The Calculator"""
    
    # constants
    NR_LINES = 25
    NR_COLUMNS = 80
    
    def __init__(self, iS, oS):
        '''Code and data stack.
    
        Rhe stacks contain integers, characters (that represent operations) and 
        blocks only. The top-most element is at position 0.'''
        
        self.code = stack.Stack()   
        self.data = stack.Stack()
        # input and output streams. Format: interger ascii codes (decimal).
        # TODO: for now streams are just fifo-queues
        self.iS = iS
        self.oS = oS
        #unary operators
        self.unaryOps = ["~"]
        # set of binary operators 
        self.binaryOps = {
            "+": operator.add,
            "-": operator.sub,
            "*": operator.mul,
            "/": self.div,
            "%": self.mod,
            "&": self.land,
            "|": self.lor,
            "<": operator.lt,
            ">": operator.gt,
            "=": self.eq
            }
        # commands
        self.commands = {
            "a": self.applyBlock,
            "c": self.copy, 
            "d": self.delete, 
            "r": self.read, 
            "w": self.write, 
            "b": self.build, 
            "g": self.group, 
            "x": self.exit
            }
        # union of commands, unaryOps and binaryOps
        self.operations = self.unaryOps + self.binaryOps.keys() + self.commands.keys()
    
    def push_code(self, s):
        '''push code onto the calculator's code stack.
      
        Left-most command will be at the top of the code stacks'''
        i = 0
        l = []
        while i < len(s):
            if util.is_number(s[i]):
                n = int(s[i])
                while i + 1 < len(s) and util.is_number(s[i + 1]):
                    i = i + 1
                    n = n * 10 + int(s[i])
                l.append(n)
            elif s[i] == "[":
                end = i
                cnt = 1
                while cnt != 0 or s[end] != "]":
                    end += 1   
                    if s[end] == "[":
                        cnt += 1
                    elif s[end] == "]":
                        cnt -= 1                
                block = s[i:end + 1]
                i = end
                l.append(block)
            elif s[i] in self.operations:
                l.append(s[i])
            elif s[i].isspace():
                pass
            else:
                msg = "Invalid code : " + s[i:]
                raise ValueError(msg)
            i = i + 1 
            
        for i in reversed(l):
            self.code.push(i)
  
    def execute(self):
        '''execute the "program" on the code stack'''
        while (not self.code.isEmpty()):
            token = self.code.pop()
            if util.is_number(token):
                # integers are simply pushed onto the data stack
                self.data.push(int(token))
            elif token in self.binaryOps.keys():
                # binary operators: {+,-,*,/,%,&,|,=,<,>}
                # pop to items from data stack 
                a = self.data.pop()
                b = self.data.pop()
                # get the operator
                op_func = self.binaryOps[token]
                if(token != "="):  # "=" does not require a and b to be integers
                    #check that they are integers
                    util.expectInt(a)
                    util.expectInt(b)
                # apply the operator...
                c = int(op_func(a, b)) # some operators (i.e., "<" and ">" return "True" or "False"; so parse it to int
                # and push the result onto the data stack
                self.data.push(c)
            elif token == "~":
                # negation
                val = int(self.data.pop())
                self.data.push(-val)  
            elif token == "x":
                return #quick fix: exit gracefully
            elif token in self.commands.keys():
                func = self.commands[token]
                func()
            elif token.startswith("[") and token.endswith("]"):
                self.data.push(token)
            else:
                msg = "Unknown command: " + token
                raise ValueError(msg)
      
    def exit(self):
        sys.exit(0)
      
    def copy(self):
        ''' operator "c" '''
        idx = self.data.peek() #can't use pop to get parameter n, because copy might want to use the 1st element
        util.expectPosIntSmallerThan(idx, self.data.size())
        val = self.data[idx] #get the value at stack depth n
        self.data.pop() #pop parameter n
        self.data.push(val) #push the new value
    
    def delete(self):
        ''' operator "s"'''
        idx = self.data.peek() #can't use pop to get parameter n, because delete might want to use the 1st element
        util.expectPosIntSmallerThan(idx, self.data.size())
        val = self.data[idx] #get the value at stack depth n
        if(idx != 0):
            self.data.remove(idx)
        self.data.pop() #pop parameter n
    
    def read(self):
        # read from input stream 
        item = chr(self.iS.read())
        # only digits, commands and operators may be read 
        if util.is_number(item):
            self.data.push(int(item))
        elif item in self.operations:
            self.data.push(item)
    
    def write(self):
        '''operation "w"'''
        # make sure that the top element of the data stack is an integer or a command
        val = self.data.pop()
        if not util.is_number(val) and val not in self.operations:
            raise ValueError("Cannot write to output stream" + val)
        for byte in to_ascii(val):
            self.oS.write(byte)
    
    def applyBlock(self):  #note that "apply" is a reserved word
        ''' operation "a" '''
        if self.is_block(self.data.peek()):
            p = self.data.pop()
            self.push_code(p[1:len(p) - 1])
      
    def build(self):
        ''' operation "b" '''
        arg = self.data.pop()
        if self.is_block(arg) or arg in self.operations:
            self.data.push("[" + arg + "]")
        else:
            raise ValueError("Invalid argument for operation 'build block':" + str(arg))
      
    def group(self):
        ''' operation "g" '''
        arg1 = self.data.pop() # the argument on top of the stack will be at the end of the block
        arg2 = self.data.pop()
        if self.is_block(arg1) and self.is_block(arg2):
            # two blocks
            block =  arg2[0:len(arg2) - 1]
            arg1 = arg1[1:len(arg1)]
            if util.is_number(arg2[len(arg1) - 1]) and util.is_number(arg1[0]):
                    block += " "
            block += arg1
        elif util.is_number(arg1) and util.is_number(arg2):
            # two integers
            block = "[" + str(arg2) + " " + str(arg1) + "]"
        elif util.is_number(arg2) and self.is_block(arg1):
            # interger (arg2) and block (arg1)
            block = "[" + str(arg2)
            arg1 = arg1[1:len(arg1)]
            if util.is_number(arg1[0]):
                    block += " "
            block += arg1
        elif self.is_block(arg2) and util.is_number(arg1):
            # block (arg2) and integer (arg1)
            block =  arg2[0:len(arg2) - 1]
            if util.is_number(arg2[len(arg2) - 1]) and util.is_number(arg1[0]):
                    block += " "
            block += str(arg1) + "]"
        else:
            raise ValueError("todo")
        self.data.push(block)
        
    def div(self, a, b):
        '''operator "/"'''
        return self.op_div(operator.div, a, b)
  
    def mod(self, a, b):
        '''operator "%"'''
        return self.op_div(operator.mod, a, b)
    
    def op_div(self, op_func, a, b):
        '''Check that b is not zero; then apply op_func to a and b '''
        util.expectNonZero(b)
        return op_func(a, b)
    
    def land(self, a, b):
        ''' operator "&" '''
        return self.boolean_op(operator.and_, a, b)
    
    def lor(self, a, b):
        '''operator "|"'''
        return self.boolean_op(operator.or_, a, b)
    
    def boolean_op(self, op_func, a, b):
        '''Check that a and b are boolean values; then apply op_func to a and b'''
        util.expectBool(a)
        util.expectBool(b)
        return op_func(a, b)
  
    def eq(self, a, b):
        ''' operator "="
        
        If "=" is applied to two equal blocks or two equal integers, 
        then the result equals 1, otherwise 0'''
        if not (util.is_number(a) and util.is_number(b)) and not(self.is_block(a) and self.is_block(b)):
            raise ValueError("Operator '=' can only be applied to two blocks or two integers!")
        return a == b 
        
    def is_block(self, b):
        '''return true if b is a block'''
        b = str(b)
        return b.startswith("[") and b.endswith("]")
    
    def print_state(self):
        '''Print the state of the calculator (same format as in the assignment specification) '''
        print self.data.toString(True) + " ^ " + self.code.toString()