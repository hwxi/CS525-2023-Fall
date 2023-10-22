####################################################
#!/usr/bin/env python3
####################################################
import sys
####################################################
sys.path.append('../../../06')
sys.path.append('./../../../../mypylib')
####################################################
from dictwords import *
from mypylib_cls import *
from assign05_02 import *
####################################################
"""
HX-2023-03-24: 30 points
Solving the doublet puzzle
"""
####################################################
"""
Please revisit assign05_02.py.
######
Given a word w1 and another word w2, w1 and w2 are a
1-step doublet if w1 and w2 differ at exactly one position.
For instance, 'water' and 'later' are a 1-step doublet.
The doublet relation is the reflexive and transitive closure
of the 1-step doublet relation. In other words, w1 and w2 are
a doublet if w1 and w2 are the first and last of a sequence of
words where every two consecutive words form a 1-step doublet.
Here is a little website where you can use to check if two words
for a doublet or not:
http://ats-lang.github.io/EXAMPLE/BUCS320/Doublets/Doublets.html
######
Given a word, the function [doublet_stream_from] returns a stream
enumerating *all* the tuples such that the first element of the tuple
is the given word and every two consecutive words in the tuple form a
1-step doublet. The enumeration of tuples should be done so that shorter
tuples are always enumerated ahead of longer ones.
######
"""
####################################################
def word_neighbors_legal(word):
    return fnlist_filter_pylist\
        (word_neighbors(word), word_is_legal)
####################################################
def wpath_neighbors_legal(wpath):
    word1 = wpath[-1]
    words = word_neighbors_legal(word1)
    return [wpath + (word2,) for word2 in words]
####################################################
def doublet_stream_from(word):
    nx0 = (word,)
    return gtree_bfs([nx0], wpath_neighbors_legal)
####################################################
stream_iforall\
    (doublet_stream_from('water'), lambda i, x: i < 100 and not(print(x)))
####################################################
