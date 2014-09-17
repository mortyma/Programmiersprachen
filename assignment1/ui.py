#! /usr/bin/env python
import calculator
import stdinstream
import stdoutstream
import sys

class Ui():
    ''''This class provides a simple UI to directly interfere with the calculator'''

    def __init__(self, calc = calculator.Calculator(stdinstream.StdinStream(), stdoutstream.StdoutStream())):
        self.calc = calc
#        self.hello = "104w101w108w108w111w032w119w111w114w108w100w033w010w010w"
        self.hello = "72w101w114w101w39w115w32w74w111w104w110w110w121w33w32w59w41w10w10w"
        self.enter = "10w101w110w116w101w114w032w099w111w109w109w097w110w100w058w032w"
        self.bye = "10w98w121w101w98w121w101w033w10w"

#    def checkInputString(self):
#        enter = "101w110w116w101w114w032w099w111w109w109w097w110w100w058w032w"
#        bye = "10w98w121w101w98w121w101w033w10w"
#        #data stack layout: [*5+: ..saved blocks..][*4: input loop][3: code block][2: buffer 1][1: buffer2][*0: working space]^
#        #working space can consist of more than one object, but before a new input its usually empty
#        #there is one main input loop but there could be in use some additional loops i.e. to calculate a number -> all loops stay on pos 5 or 4 after a clean working space
#        
#        #saved blocks are the last ones, usually 6-n
#        condition ="{0}{1}{2}[3c4d1+da]a" #put 0/1 as cond, a block as true and one as false
#
#        #move result digit down and all other numbers up and calculate the next number (ie 4 4567-4000 -> 4 567) then add a w to the digit
#        nTAInnerFinish = "3d 3c4d 2c 4c5d ga 2c3d - 2c3d 48+[w]g 3c4d 2c3d g 2c3d 3ca"
#        #push 10, switch top, divide by 10, push [10*] bring up other [10^x]-block from position 3, merge the two blocks, restart loop by
#        nTAInnerRunning = "10 2c3d/[10*]3c4d g 2c3d 3ca"
#        nTAInnerLoop = "[1c10<[{0}][{1}][3c4d1+da]a] []3c3ca".format(nTAInnerFinish, nTAInnerRunning)
#        nTAOuterLoop = "[[1c10<[48+[w]gg2d][{0}] [3c4d1+da]a] [] [3ca]]".format(nTAInnerLoop)
#
#        #subtract 48 from the ascii code , remove the unneeded buffer and join all remaining blocks with the code block, finally set up the special number buffer
#        convertAsciiToDig = "[48-0-2d[+]ggg [10*][]]"
#        convertDigToAscii = "[b[]g3dgg[0][]]"
#        #first remove the W and the two number control bloks (10 and empty), then activate the last block.
#        #afterwards put loop, split block, move number (pos4) to top, move loop init (pos2) to top, activate
#        convertNumToAscii = "[1d1d1da{0} a 4c5d 2c3d a [0][]]".format(nTAOuterLoop) 
#        
#        # try to convert operation with block. if its a W, split digits and convert all of them
#        convertOpNum = "[" + condition.format("1c87=","[b3dgg [0][]]", convertNumToAscii) + "]";
#        # try to convert operation with block. if its a w then put it on the os if it was an ascii code
#        convertOpDig = "[" + condition.format("1c119=", convertOpNum, convertDigToAscii) + "]";
#        # try to convert operation with block. if its a w then put it on the os if it was an ascii code
#        checkExit = "[" + condition.format("1c120=", convertOpDig, "[1d[{0}x]3dgg [0][]]".format(bye)) + "]";
#
#        # check if digit
#        checkNumLt = "[" + condition.format("1c58>", checkExit, convertAsciiToDig) + "]"; #is 58>num
#        checkNumGt = "[" + condition.format("1c47<", checkExit, checkNumLt) + "]"; #is 57<num
#
#        #check if block starts: remove [ and buffers, move actual code block from position 1 down to position 2 (means move 2 to top), add empty code block and buffers
#        checkStartBlock = "[" + condition.format("1c91=", checkNumGt, "[1d1d1d 2c3d [][0][]]") + "]"
#        #check if block ended: remove ] and buffers, move the first saved block back to top and append the actual code block to it (means move 3, 2 to top , block and group)
#        checkEndBlock = "[" + condition.format("1c93=", checkStartBlock, "[1d1d1d 3c4d 2c3d bg [0][]]") + "]"
#
#        #check spaces
#        checkEndBlock = condition.format("1c32=", checkEndBlock, "[1d[]3dgg [0][]]") #if it's a space char, just put an empty block instead, so it can merge..
#
#
#        #read = "[r1c10=[{0}4ca][1d1d1d2da][3c4d1+da]a][0][][]4ca" #reads characters till a '10' (=\n) is found, puts all together in a block and executes it with 'a'
#        #reads characters till a '10' (=\n) is found, puts all together in a block and executes it with 'a'. after execution, prints a newline and starts over reading input
#        readEndless = "[r1c10=[{0}4ca][1d1d1da 10w {1}[][0][]4ca][3c4d1+da]a]{1}[][0][]4ca" 
#        return readEndless.format(checkEndBlock,  enter)


    def checkInputString2(self,  enter="", bye =""):
        uiString = ""

        #data stack layout for W-loop (postprocessing): [loop1][number][loop2][mod number][buffer][working space]
        #W: copy the digit, add 48 and push o outputstream with w, execute the 10-block and subtract the multiplication from the original number
        printNumDigitNestedAddW = "2c 48+w a -~ "
        
        #W:  another loop that gathers the first digit of a number and the value of this digit (ie 356->3 and 100): divide by 10, add a *10 buffer, copy the original number (and continue dividing..)
        printNumDigitNested = self.buildWhile("2c9<", "10 3c4d/2c3d[10*] g", 3) + printNumDigitNestedAddW
        
        #W: copy the number put a empty block after the copy to store the *10*10 (see loops workspace structure).. 
        #start a nested loop to get  the digit: when finished, add 48 to the last digit and push to outputstream with w
        printNumDigit = self.buildWhile("1c9<", "1c[]"+ printNumDigitNested) + "48+w"

        #W: when W, remove W and switch buffer with numcontinue with splitting in a loop, else block the ascii code to convert it to a symbol and replace the buffer
        printNum = self.buildIf("1c87=", "1d1d [[" + printNumDigit + "]a] g [0]", "2d bg [0]")

        #data stack layout: [*5+: ..saved blocks..][*4: input loop][3: code block][2: buffer 1][1: buffer2][*0: working space]^
        #w: remove puffer, create block, join blocks and set new buffer
        printAscii = self.buildIf("1c119=", "2d bg [0]", printNum)

        #R: read a number (only digits, the rest till \n gets dismissed.. no better approach atm)
        #loop: while gt 47 ant lt 58,
        readNum = self.buildIf("1c82=", "1d1d [[][0]" + self.buildWhile("r1c 47< 2c 58> &",  "48-~[+]ggg [10*]", 3, "1d") + "1d a] g [0]", printAscii) #TODO: write function to remove rest of the line!!

        #x: remove workingspace and buffer insert output string and x in a block and join the block with the code
        exit = self.buildIf("1c120=", "1d1d [{0}x]g [0]".format(bye), readNum)

        #if < 58 convert to digit and use buffer: subtract from 48 and negate, finally group with +, with the number buffer and with the code
        digitLt = self.buildIf("1c58>", "48-~[+]ggg [10*]" ,  exit)

        #if > 47 check upper bound, else check exit
        digitGt = self.buildIf("1c47<",  digitLt,  exit)

        #[: remove [ and buffer, move actual code block from position 1 down to position 2 (means move 2 to top), add empty code block and buffer
        blockStart =  self.buildIf("1c91=",  "1d1d 2c3d [][0]",  digitGt)

        #]: remove workingspace and buffer, take the last saved block back and join the actual working block finally add new buffer
        blockEnd = self.buildIf("1c93=",  "1d1d 3c4d 2c3d bg [0]",  blockStart)

        # : remove workingspace and buffer set new buffer and go on
        space = self.buildIf("1c32=", "1d1d [0]",  blockEnd)
        
        #read input till 10 (=\n) shows up
        uiString += "[" + self.buildResetWhileNot("r1c10=", space, "1d1d [][0] 3c4d a 10w" + enter) + "]"
        uiString += enter + "[][0] 3c4d a "
#        uiString += "[][0] 3c4d a  1d1d a "
        #uiString += enter
        return uiString

    def buildIf(self,  c,  t , f ):
        condition ="{0}[{1}][{2}][3c4d1+da]a" #put 0/1 as cond, a block as false and one as true, the last part is if-logic
        return condition.format(c, f, t)

    def buildWhile(self, c, code,  loopPos = 2,  post = ""):
        mov = "".join(["{0}c{1}d ".format(loopPos,  loopPos+1) for a in range(1, loopPos)])
        #loop = "[{0}[2d][{1}3ca][3c4d1+da]a]2c3d 2ca" # move loop down to pos 3, if false, delete loop, if true, copy loop and execute
        loop = "[{0}[{5}{2}d][{1}{2}ca][3c4d1+da]a]{4} {2}ca" # move loop down to pos 3, if false, delete loop, if true, copy loop and execute
        return loop.format(c, code,  loopPos,  loopPos+1, mov,  post)

    def buildResetWhileNot(self, c, code,  reset): #little strange loop, does not remove the loop where it was places in the beginning (im to lazy now to fix the other code...)
        loop = "[{0}[{1}3ca][{2} 3ca][3c4d1+da]a]3c4d 3c4d 3ca" # move loop down to pos 3, if false, delete loop, if true, copy loop and execute
        return loop.format(c, code, reset)
#    def buildWhileNot(self, c, code): #little strange loop, does not remove the loop where it was places in the beginning (im to lazy now to fix the other code...)
#        loop = "[{0}[{1}3ca][4d][3c4d1+da]a]3c4d 3c4d 3ca" # move loop down to pos 3, if false, delete loop, if true, copy loop and execute
#        return loop.format(c, code)

    def runSilent(self):
        '''Doesn't show the UI, but processes all inputs and writes the result to the output stream'''
        code = self.checkInputString2()
        self._run(code)

    def run(self):
        '''Does show a UI and processes all inputs and writes the result to the output stream'''
        code = self.hello + self.checkInputString2(self.enter,  self.bye)
        self._run(code)

    def _run(self,  uiCode):
        '''Executed the code on a calculator.'''
        #TODO: remove debug
#        print uiCode
        self.calc.push_code(uiCode)
        self.calc.execute()

