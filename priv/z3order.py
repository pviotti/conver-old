##!/usr/bin/env python2

from z3 import *

s = Solver()
s.set("timeout", 3000)

if len(sys.argv) > 1 and sys.argv[1] == "int":
    sort = IntSort()
else:
    sort = DeclareSort('T')
to = Function('>', sort, sort, BoolSort())

x,y,z = Consts('x y z', sort)
s.add(ForAll([x,y], Implies(And(to(x,y),to(y,x)), x==y)))       # antisymmetry
s.add(ForAll([x,y,z], Implies(And(to(x,y),to(y,z)), to(x,z))))  # transitivity
s.add(ForAll([x,y], Or(to(x,y), to(y,x))))                      # totality

a,b,c,d = Consts('a b c d', sort)
s.add(Distinct(a,b,c,d))

s.add(to(a,b))
s.add(to(b,d))
#s.add(to(d,a)) # add cycle to make it unsat

print s.check()
print s.model()

"""
# find all possible total orders given a partial order
ct = 0
while s.check() == sat:
    print s.model()[a], s.model()[b], s.model()[c], s.model()[d]
    #print s.model()
    #print s.assertions()
    ct += 1
    s.add(c != s.model()[c]) 
            #a != s.model()[a],
            #b != s.model()[b],
            #d != s.model()[d]))
print "Models: ", ct"""
# http://stackoverflow.com/questions/11867611/z3py-checking-all-solutions-for-equation

"""
if len(sys.argv) > 1 and sys.argv[1] == "int":
    for e in [a,b,c,d]:
        s.add(And(e<5,e>0))
"""

#s.add(ForAll([x,y], Implies(x!=y, Xor(to(x,y), to(y,x))))) 

#print model()[a]

#s.reset()

#s.add(to(a,b))
#s.add(to(a,c))
#s.add(to(a,d))
#s.add(to(b,d))
#s.add(to(b,c))
#s.add(to(c,d))

#s.add(ForAll([x,y], Implies(to(x,y), Not(to(y,x))))) 
#s.add(ForAll(x, Not(to(x,x))))                                  # antiriflexivity
#s.add(ForAll([x,y], Implies(x!=y, Xor(to(x,y), to(y,x)))))      # totality

