# Spaghetti Wiring

## Description

<p>Eric the Electrician has a problem. He has been told to connect a set of ports on a flat surface using some cables, but there's a problem: the cables are carrying signals that interfere with each other. They must not cross. Since the locations of the ports are all over the place, this poses a significant challenge.</p>

<p>We can help Eric, but we need to boil the problem down a little. We will represent the usable space as a simple rectangular grid. The objective will be to connect some pairs of ports at some given coordinates using continuous, non-intersecting paths on the grid.</p>

## Formal input

<p>The first line of our input will be a line containing two numbers representing a width-by-height measurement of our available grid. Next, there will be a series of lines with two coordinate pairs (X, Y) per line, representing pairs of ports that need to be connected.</p>

<p><strong>Sample Input:</strong></p>

<pre><code>6 4
5 0 4 2
1 1 5 3
0 3 4 3
</code></pre>

<p>This would correspond to a grid that looks like this (assigning some arbitrary letters to the three port pairs):</p>

<pre><code>.....A
.B....
....A.
C...CB
</code></pre>

## Formal output

<p>Our output will simply be the grid itself, with the proper paths filled in.</p>

<p><strong>Sample output:</strong></p>

<pre><code>AAAAAA
ABBBBB
AAAAAB
CCCCCB
</code></pre>

## Challenge Inputs

### Challenge Input 1

<pre><code>13 5
1 1 7 4
11 1 5 3
8 1 10 2
0 4 1 2
</code></pre>

<p>Visually, this grid is:</p>

<pre><code>.............
.A......C..B.
.D........C..
.....B.......
D......A.....
</code></pre>

### Challenge Output 1

<pre><code>.....BBBBBBB.
.AA..B..C..B.
DDA..B..CCC..
D.A..B.......
D.AAAAAA.....
</code></pre>

### Challenge Input 2

<pre><code>12 12
1 10 8 6
9 2 1 8
5 5 9 9
2 5 6 6
6 5 3 7
7 5 10 9
1 7 10 1
</code></pre>

<p>Visually, this grid is:</p>

<pre><code>............
..........G.
.........B..
............
............
..D..CEF....
......D.A...
.G.E........
.B..........
.........CF.
.A..........
............
</code></pre>

## Notes

<ul>
<li>As may be evident, the grids are 0-indexed.</li>
<li>Some inputs may have multiple solutions. Others may have no solutions. If there are no possible solutions, print "No solutions" or something similar.</li>
<li>The paths must be continuous and unbroken. They may not "double back" on themselves.</li>
<li>Letters were chosen as arbitrary characters for convenience. Feel free to use numbers, symbols, or emoji👌👌 (though a monospace font is useful for the output being readable).</li>
</ul>

## Bonus points

<p>Make your program come up with solutions that use all the available space on the grid. For example, for the <strong>Challenge Input 1</strong> above, such an output would be:</p>

<pre><code>BBBBBBBBBBCCC
BAAAAAAACBCBC
BDDDDDDACBCBC
BBBBBBDACBBBC
DDDDDDDACCCCC
</code></pre>

## Finally

<p><em>This challenge was inspired by the <a href="https://play.google.com/store/apps/details?id=com.bigduckgames.flow&amp;hl=en">Flow Free</a><span class="noCtrlF keyNavAnnotation" data-text="[2]" title="press 2 to open link"></span> mobile game. Credit where it's due.</em> </p>

<p>Have a good challenge idea? Consider submitting it to <a href="/r/dailyprogrammer_ideas">/r/dailyprogrammer_ideas</a><span class="noCtrlF keyNavAnnotation" data-text="[3]" title="press 3 to open link"></span>.</p>
</div>

# Implementation

Written in _Haskell GHC 7.16_.

You can the program with _stack_:

```
$ stack build
$ stack exec spaghetti-wiring-exe input1.txt output1.txt
$ stack exec spaghetti-wiring-exe input2.txt output2.txt
```

__Be careful__ with second sample: it takes up a lot of memory and may hang your
computer.

I did not implement the bonus task though.
