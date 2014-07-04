#! /usr/bin/env python
import calculator
import stdinstream
import stdoutstream
import sys

class Ui():
    ''''This class provides a simple UI to directly interfere with the calculator'''

    def __init__(self, calc = calculator.Calculator(stdinstream.StdinStream(), stdoutstream.StdoutStream())):
        self.calc = calc

    def headerString(self):
        # lade hello world
        hello = "104w101w108w108w111w032w119w111w114w108w100w033w010w010w"
        return hello

    def checkInputString(self):
        enter = "101w110w116w101w114w032w099w111w109w109w097w110w100w058w032w"
        bye = "10w98w121w101w98w121w101w033w10w"
        #data stack layout: [*..saved blocks..][linput loop][code block][buffer 1][buffer2][*working space]^
        #working space can consist of more than one object, but before a new input its usually empty
        
        #saved blocks are the last ones, usually 6-n
        condition ="{0}{1}{2}[3c4d1+da]a" #put 0/1 as cond, a block as true and one as false

        #move result digit down and all other numbers up and calculate the next number (ie 4 4567-4000 -> 4 567) then add a w to the digit
        nTAInnerFinish = "3d 3c4d 2c 4c5d ga 2c3d - 2c3d 48+[w]g 3c4d 2c3d g 2c3d 3ca"
        #push 10, switch top, divide by 10, push [10*] bring up other [10^x]-block from position 3, merge the two blocks, restart loop by
        nTAInnerRunning = "10 2c3d/[10*]3c4d g 2c3d 3ca"
        nTAInnerLoop = "[1c10<[{0}][{1}][3c4d1+da]a] []3c3ca".format(nTAInnerFinish, nTAInnerRunning)
        nTAOuterLoop = "[[1c10<[48+[w]gg2d][{0}] [3c4d1+da]a] [] [3ca]]".format(nTAInnerLoop)

        #subtract 48 from the ascii code , remove the unneeded buffer and join all remaining blocks with the code block, finally set up the special number buffer
        convertAsciiToDig = "[48-0-2d[+]ggg [10*][]]"
        convertDigToAscii = "[b[]g3dgg[0][]]"
        #first remove the W and the two number control bloks (10 and empty), then activate the last block.
        #afterwards put loop, split block, move number (pos4) to top, move loop init (pos2) to top, activate
        convertNumToAscii = "[1d1d1da{0} a 4c5d 2c3d a [0][]]".format(nTAOuterLoop) 
        
        # try to convert operation with block. if its a W, split digits and convert all of them
        convertOpNum = "[" + condition.format("1c87=","[b3dgg [0][]]", convertNumToAscii) + "]";
        # try to convert operation with block. if its a w then put it on the os if it was an ascii code
        convertOpDig = "[" + condition.format("1c119=", convertOpNum, convertDigToAscii) + "]";
        # try to convert operation with block. if its a w then put it on the os if it was an ascii code
        checkExit = "[" + condition.format("1c120=", convertOpDig, "[1d[{0}x]3dgg [0][]]".format(bye)) + "]";

        # check if digit
        checkNumLt = "[" + condition.format("1c58>", checkExit, convertAsciiToDig) + "]"; #is 58>num
        checkNumGt = "[" + condition.format("1c47<", checkExit, checkNumLt) + "]"; #is 57<num

        #check if block starts: remove [ and buffers, move actual code block from position 1 down to position 2 (means move 2 to top), add empty code block and buffers
        checkStartBlock = "[" + condition.format("1c91=", checkNumGt, "[1d1d1d 2c3d [][0][]]") + "]"
        #check if block ended: remove ] and buffers, move the first saved block back to top and append the actual code block to it (means move 3, 2 to top , block and group)
        checkEndBlock = "[" + condition.format("1c93=", checkStartBlock, "[1d1d1d 3c4d 2c3d bg [0][]]") + "]"

        #check spaces
        checkEndBlock = condition.format("1c32=", checkEndBlock, "[1d[]3dgg [0][]]") #if it's a space char, just put an empty block instead, so it can merge..


        #read = "[r1c10=[{0}4ca][1d1d1d2da][3c4d1+da]a][0][][]4ca" #reads characters till a '10' (=\n) is found, puts all together in a block and executes it with 'a'
        #reads characters till a '10' (=\n) is found, puts all together in a block and executes it with 'a'. after execution, prints a newline and starts over reading input
        readEndless = "[r1c10=[{0}4ca][1d1d1da 10w {1}[0][][]4ca][3c4d1+da]a]{1}[0][][]4ca" 
        return readEndless.format(checkEndBlock,  enter)

    def runSilent(self):
        '''Doesn't show the UI, but processes all inputs and writes the result to the output stream'''
        code = self.checkInputString()
        self._run(code)

    def run(self):
        '''Does show a UI and processes all inputs and writes the result to the output stream'''
        code = self.headerString() + self.checkInputString()
        self._run(code)

    def _run(self,  uiCode):
        '''Executed the code on a calculator.'''
        #TODO: remove debug
#        print uiCode
        self.calc.push_code(uiCode)
        self.calc.execute()

