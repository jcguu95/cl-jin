## Description of problem.

The following two dimensional matrix represents a map of
islands in an ocean. Every zero represents sea level. Every
positive integer represents the volume of land above sea level
at that location. An island is represented by a contiguous
block of positive integers. Here are 3 examples:

In this map, there are four islands, all 3x3 squares:

  0 5 5 7 0 0 0 0 0 0
  0 1 8 8 0 0 0 0 0 0
  0 3 2 2 0 0 2 8 4 0
  0 0 0 0 0 0 8 8 3 0
  0 0 0 0 0 0 8 7 8 0
  8 5 2 0 0 0 0 0 0 0
  2 7 3 0 0 0 0 0 0 0
  3 5 1 0 0 7 5 8 0 0
  0 0 0 0 0 1 1 7 0 0
  0 0 0 0 0 8 3 5 0 0

In this map, there are two oddly shaped islands:

  0 1 1 1 0 0 0 0 0 0
  0 0 0 1 0 0 0 0 0 0
  0 0 1 1 0 0 2 2 2 0
  0 0 1 0 0 0 2 0 2 0
  0 1 1 1 1 0 2 2 2 0
  1 1 0 0 0 0 0 2 0 0
  0 1 0 2 2 2 0 2 0 0
  1 1 0 0 0 2 2 2 0 0
  0 1 1 1 0 0 2 0 0 0
  0 0 0 0 0 2 2 2 0 0


The following has four total islands, because of how we define
"contiguous" (connecting) land:

  0 1 1 1 0 0 0 0 0 0
  0 1 1 1 0 0 0 0 0 0
  0 1 1 1 1 1 1 1 1 0   <-- horizontal and vertical neighbors are considered
  0 0 0 0 0 0 1 1 1 0       contiguous, so the bridge forms one big island.
  0 0 0 0 0 0 1 1 1 0
  2 2 2 0 0 0 0 0 0 0
  2 2 2 0 0 0 0 0 0 0
  2 2 2 0 0 4 4 4 0 0
  0 0 0 3 0 4 4 4 0 0
  0 0 0 0 4 4 4 4 0 0

   land touching at corners is NOT contiguous, so this is
   treated as THREE land islands, the single '3' pointed at
   being the third island.


The challenge is to write whatever code you need to:

1. Represent a map like the above.
     -- The map is a rectangle of arbitrary size.
     -- The zero or more islands can be of any shape and heights.

2. Find all the islands, using the above definition of "contiguous".

3. Return a list of the total volume of each island, in any order you like.
   The volume of an island is the sum of all its values, each of which you
   can assume to be positive.  For example, in the very first map, the top
   left island is:

       5 5 7
       1 8 8
       3 2 2

   and its volume is 41.


Use whatever representation you think is best, and explain
why.

Use whatever method you think is best, and explain why you use
it.

Take the time to make the code and commenting the way *you*
like to see it.

This is tricky, so don't worry if your first approach isn't
right -- that's why it's called a puzzle.
