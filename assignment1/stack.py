#! /usr/bin/env python
class Stack(list):
    """A simple stack. Top-most element is first element in the list."""
 
    def push(self, item):
        self.insert(0, item)
    
    def isEmpty(self):
        return not self
    
    def size(self):
        return len(self)
    
    def peek(self):
        '''return the item on top of the stack, without popping it'''
        return self[0]
    
    def remove(self, idx):
        '''remove element at position idx from the stack.'''
        return super(Stack, self).pop(idx)
        
    def pop(self):
        return super(Stack, self).pop(0)
    
    def toString(self, reverse = False, count = 0):
        '''Contents of the stack as a string. 
        
        Top-most element is first element in the string if reverse is False 
        (default); otherwise, top-most element is the last in the string. 
        Elements are separated by space- '''
        if(reverse):
            l = reversed(self)
        else:
            l = self

	if(count>0):
	        return ' '.join('[..]' if len(str(p))>count else str(p) for p in l)	
        return ' '.join(str(p) for p in l)
 
