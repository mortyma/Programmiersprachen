#! /usr/bin/env python
import calculator
import stream
import sys

class Ui():

    def __init__(self, calc = calculator.Calculator(stream.Stream(), stream.Stream())):

	    self.calc = calc
	    #mock input stream:
    #	iS.write(55);
    #	iS.write(80);
	    ####


    def headerString(self):
        # lade hello world
        hello = "104w101w108w108w111w032w119w111w114w108w100w033w010w010w"
        enter = "101w110w116w101w114w032w099w111w109w109w097w110w100w058w032w"

    def checkInputString(self):

        condition ="{0}{1}{2}[3c4d1+da]a" #put 0/1 as cond, a block as true and one as false

        convertAsciiToDig = "[48-0-]"
        convertDigToAscii = "[[48+]2c3dbg]"

        # try to convert operation with block. if its a w, add a +48 
        convertOp = "[" + condition.format("1c119=","[b]",convertDigToAscii) + "]";

        # check if number (digit..)
        checkNumLt = "[" + condition.format("1c58>", convertOp, convertAsciiToDig) + "]"; #is 58>num
        checkNumGt = "[" + condition.format("1c47<", convertOp, checkNumLt) + "]"; #is 57<num

        #check if block
        checkStartBlock = "[" + condition.format("1c91=", checkNumGt, "[[]]") + "]" #an empty block
        checkEndBlock = "[" + condition.format("1c93=", checkStartBlock, "[bg]") + "]" #first block the block, then join it the previous block

	#check spaces
        checkEndBlock = condition.format("1c32=", checkEndBlock, "[1d[]]") #if it's a space char, just put an empty block instead, so it can merge..

        #test for num, start with an empty block... we will see if needed:
        read = "[r1c10=[{0}g2ca][1d2da][3c4d1+da]a][]2ca" #reads characters till a '10' (=\n) is found, puts all together in a block and executes it with 'a'
        num = read.format(checkEndBlock)

        return num

    def runSilent(self):
        '''Doesn't show the ui, but processes all inputs and writes the result to the output stream'''
        code = self.checkInputString()
        self.calc.push_code(code)
        self.calc.execute()

    def show(self):
        '''Glue together all parts for our ui and load it into the calculator.

        Still a lot of work neede here.. i.e. how to output data without waiting till the calculator ends!? maybe start a new execution after print, but without x? maybe we can just simulate a stop using the x and just restart the ui and wait for new input, till the user inserts a x - BUT then he ui isn't written in r2d2 anymore.. sigh'''
        #uiCode = hello + enter + num
        uiCode = self.checkInputString()

        # execute the code
        print uiCode
        self.calc.push_code(uiCode)
        self.calc.execute()

        # TODO: doesn't work well this way..
        # show some output
        print self.calc.oS
        print ''.join([chr(a) for a in self.calc.oS])

