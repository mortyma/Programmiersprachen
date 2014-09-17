#! /usr/bin/env python
import stream
import sys

class StdinStream(stream.Stream):
    '''
    Kind of wrapper around stdin
    '''
        
    def read(self):
        '''read the next ascii code from the stream'''
        if not self :
            [self.write(ord(v)) for v in sys.stdin.readline()] 
        return self.pop(0)
  
  
