# Calculating Water

From the [restless programmer blog post](http://www.restlessprogrammer.com/2014/04/calculating-water.html).

## Input Description

You're given an array of non-negative numbers, which describe the shape
of two-dimensional water container. A container is a set of vertical 1-unit-wide
columns, standing closely next to each other. Each number in the array denotes
the height of an appropriate column.

E.g. the array `[1,2,1,3,4,4,5,1,2,0,3]` describes this container:

```
      X
    XXX
   XXXX   X
 X XXXX X X
XXXXXXXXX X
12134451203
```

It's considered that even if container's column has height 0, it still has a
bottom.

## Output Description

You must calculate how many units of water would a container hold.

E.g. for the example above the solution is **7**:

```
      X
    XXX
   XXXX...X
 X.XXXX.X.X
XXXXXXXXX.X

. (dot) = water
```

# Implementation

I've written my solution in _C_ compiled with _gcc 5.4_.

Use `gcc main.c && ./a.out` to build and run the program.
