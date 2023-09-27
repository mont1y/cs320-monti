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
