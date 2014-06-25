#! /usr/bin/env python
class Stream(list):
    '''
    A simple stream (implemented as fifo queue)
    '''
    
    def write(self, val):
        '''write the ascii code into the stream'''
        super(Stream, self).append(val)
        
    def read(self):
        '''read the next ascii code from the stream'''
        return self.pop(0)
  
  