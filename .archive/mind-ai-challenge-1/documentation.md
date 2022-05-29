## Solution

### Key data structure: `-union` and `-find`

The key data structure to use is called "union-find" in computer
science, which is best explained through graphics or even videos.
There are decent videos explaining

  + What is union-find:
    https://www.youtube.com/watch?v=ibjEGG7ylHk
  + Basic operations "find" and "union":
    https://www.youtube.com/watch?v=0jNmHPfA_yE

  But I can also explain briefly here. Basically, we have a set
to be partited into disjoint groups. What we need is two
functions, `-union` and `-find`, respectively dictates if two
elements should be in a same group and finds the group that an
element belongs to.

  The tricky part occurs when the two elements to be `-union`ed
belong to two different groups already. We need an efficient way
to "inform" each member in one group to "update" their group to
the other group. A naive approach is to traverse through all
members and update one by one, but this is inefficient. Instead,
we group elements by putting them into a tree, with the root of
the tree being the identity of the group. To merge two elements x
and y, simply

  1. Point the root of y to the root of x.
  2. Point y to the root of x.

The first action automatically updates for all elements in the
group of y (O(1)), while the second action makes future actions
more efficient by minimizing the depth of the tree. To `-find`
(the root), simply traverse the tree to its root inductively.

### Main function: `solve`

  In the source code, I implement both functions efficiently. The
last unexplained function is `solve`, whose usage can be found in
`test.lisp`. For example

``` test.lisp
(defvar +example-5+
  (make-array '(2 3)
              :initial-contents
              '((2 0 7)
                (0 5 1))))

(assert (equal (solve 2 3 +example-5+) '(2 13)))
```

Its implementation goes roughly as follows

  1. Map the positions into an array of nonnegative numbers.
  2. As we traverse through the positions, `-union` the two
     neighbors whenever both of them are positive.
  3. Finally, for each group, sum up all volumes of the positions
     in the same group.
