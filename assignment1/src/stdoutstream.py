#! /usr/bin/env python
import stream
import sys

class StdoutStream(stream.Stream):
    '''
    Kind of wrapper around stdout
    '''
        
    def write(self, val):
        '''write the ascii code into the stream'''
        sys.stdout.write(chr(val))
  
  
