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

        convertAsciiToDig = "[[48-0-]" #notice: half open [
#        convertDigToAscii = "[[[48+]2c3dbg]" #notice: half open [
        convertDigToAscii = "[[b[]g]"#"[[2c3dbg]" #notice: half open [
        #first remove the W and the two number control bloks (10 and empty), then activate the last block.
        #afterwards put loop, split block, move number (pos4) to top, move loop init (pos2) to top, activate
        convertNumToAscii = "[1d1d1da{0} a 4c5d 2c3d a".format(nTAOuterLoop) #notice: half open ]
        numberSufix = "a2d[+]ggg[10*][]]" #notice: half open ] executes prev block, groups it to the prev one, then deletes the number check. pushes a +. groups and executes it to add the number (or fake 0 ) from before
        #active a block, remove the 3. element = [0] or [10*]and join the other remaining blocks
        noNumberSufix = "a3dgg[0][]]" #notice: half open ]
        noNumberSufixW = "[0][]]" #notice: half open ]

        # try to convert operation with block. if its a W, split digits and convert all of them
        convertOpNum = "[" + condition.format("1c87=","[[b]" + noNumberSufix, convertNumToAscii + noNumberSufixW) + "]";
        # try to convert operation with block. if its a w then put it on the os if it was an ascii code
        convertOpDig = "[" + condition.format("1c119=", convertOpNum, convertDigToAscii + noNumberSufix) + "]";

        # check if digit
        checkNumLt = "[" + condition.format("1c58>", convertOpDig, convertAsciiToDig + numberSufix) + "]"; #is 58>num
        checkNumGt = "[" + condition.format("1c47<", convertOpDig, checkNumLt) + "]"; #is 57<num

        #check if block starts: remove [ and buffers, move actual code block from position 1 down to position 2 (means move 2 to top), add empty code block and buffers
        checkStartBlock = "[" + condition.format("1c91=", checkNumGt, "[1d1d1d 2c3d [][0][]]") + "]"
        #check if block ended: remove ] and buffers, move the first saved block back to top and append the actual code block to it (means move 3, 2 to top , block and group)
        checkEndBlock = "[" + condition.format("1c93=", checkStartBlock, "[1d1d1d 3c4d 2c3d bg [0][]]") + "]"

	#check spaces
        checkEndBlock = condition.format("1c32=", checkEndBlock, "[[1d[]]" + noNumberSufix) #if it's a space char, just put an empty block instead, so it can merge..

        #test for num, start with an empty block... we will see if needed:
        read = "[r1c10=[{0}4ca][1d1d1d2da][3c4d1+da]a][0][][]4ca" #reads characters till a '10' (=\n) is found, puts all together in a block and executes it with 'a'
        num = read.format(checkEndBlock)

	#TODO:remove debug
	print num

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

