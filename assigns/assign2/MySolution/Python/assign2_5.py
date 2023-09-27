###########################################################################

# datatype 'a list =
# nil | cons of ('a * 'a list)

class fnlist:
    ctag = -1
    def get_ctag(self):
        return self.ctag
    def __iter__(self):
        return fnlist_iter(self)
    def __reversed__(self):
        return fnlist_reverse(self)
# end-of-class(fnlist)

class fnlist_iter:
    def __iter__(self):
        return self
    def __init__(self, itms):
        self.itms = itms
    def __next__(self):
        if (self.itms.ctag==0):
            raise StopIteration
        else:
            itm1 = self.itms.cons1
            self.itms = self.itms.cons2
            return itm1
    # end-of-[__next__]

###########################################################################

class fnlist_nil(fnlist):
    def __init__(self):
        self.ctag = 0
        # return None
# end-of-class(fnlist_nil)

class fnlist_cons(fnlist):
    def __init__(self, cons1, cons2):
        self.ctag = 1
        self.cons1 = cons1
        self.cons2 = cons2
        # return None
    def get_cons1(self):
        return self.cons1
    def get_cons2(self):
        return self.cons2
# end-of-class(fnlist_cons)

####################################################
def fnlist_sing(x0):
    res = fnlist_nil()
    res = fnlist_cons(x0, res)
    return res
####################################################
def fnlist_print(xs):
    nx = 0
    sep = "; "
    print("fnlist[",end='')
    while(xs.ctag > 0):
        if (nx > 0):
            print(sep,end='')        
        print(xs.cons1,end='')
        nx = nx + 1; xs = xs.cons2
    print("]", end='')
####################################################
def fnlist_reverse(xs):
    res = fnlist_nil()
    for x1 in xs:
        res = fnlist_cons(x1, res)
    return res
####################################################







########################################################################
# Assign2:
# //
# DUE: the 27th of September, 2023
# //
# Total: 80 point
# (OCaml: 40 points)(Python: 40 points)
########################################################################
# Higher-order programming in Python
########################################################################
#
# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml

import sys
####################################################

def fnlist_make_fwork (fopr_func):  
    list = []
    def work (x):
        list.append(x)
    fopr_func(work)
    fnlist = fnlist_nil()
    for i in reversed (list):
        fnlist=fnlist_cons(i, fnlist)
    return fnlist