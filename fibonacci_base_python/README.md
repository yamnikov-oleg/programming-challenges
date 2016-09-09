# Fibonacci base

From [/r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/).
[Source link](https://www.reddit.com/r/dailyprogrammer/comments/5196fi/20160905_challenge_282_easy_unusual_bases/).

## Description

<p>Binary numbers (base 2) are written using <code>1</code>s and <code>0</code>s to represent which powers of 2 sum together to create the decimal number.</p>

<table><thead>
<tr>
<th>16</th>
<th>8</th>
<th>4</th>
<th>2</th>
<th>1</th>
</tr>
</thead><tbody>
<tr>
<td>1</td>
<td>0</td>
<td>0</td>
<td>1</td>
<td>1</td>
</tr>
</tbody></table>

<p>A <code>1</code> represents using that power of 2 and a <code>0</code> means not using it. In the above example there is a one in the <code>16</code>s, <code>2</code>s and the <code>1</code>s so we do:</p>

<pre><code>10011 = 16 + 2 + 1 = 19
</code></pre>

<p>meaning that <code>10011</code> is binary for <code>19</code></p>

<p>The Fibonacci Sequence has a similar property that any positive integer can be written in the form of Fibonacci numbers (with no repeats). For example:</p>

<pre><code>25 = 21 + 3 + 1
</code></pre>

<p>If we use the same form as for writing binary, with the Fibonacci sequence instead of powers of 2, we can represent which Fibonacci numbers we use with a 1, and the ones we don't with a 0.</p>

<table><thead>
<tr>
<th>13</th>
<th>8</th>
<th>5</th>
<th>3</th>
<th>2</th>
<th>1</th>
<th>1</th>
</tr>
</thead><tbody>
<tr>
<td>1</td>
<td>0</td>
<td>1</td>
<td>0</td>
<td>0</td>
<td>1</td>
<td>0</td>
</tr>
</tbody></table>

<pre><code>1010010 = 13 + 5 + 1 = 19
</code></pre>

<p>meaning that <code>101001</code> is 'Base Fib' for <code>19</code></p>

<p>The task is to create a converter to convert to and from decimal to 'Base Fib'
Due to the nature of the Fibonacci Sequence, many numbers have multiple representations in 'Base Fib', for the moment these are to be ignored - any form is acceptable.</p>

### Input description

<p>You will be given a line of input for each conversion, stating the base it is currently in, and the number to convert seperated by space</p>

<pre><code>10 16
10 32
10 9024720
F 10
F 1
F 111111
F 100000
F 10110110100111001
</code></pre>

### Output description

<p>The program should output the converted number, in it's expected base, e.g.</p>

<pre><code>1001000
10101000
1010100101010100000010001000010010
1
1
20
8
2868
</code></pre>

## Notes/Hints

<ul>
<li><a href="http://planetmath.org/listoffibonaccinumbers">List of Fibonacci Numbers</a><span class="noCtrlF keyNavAnnotation" data-text="[1]" title="press 1 to open link"></span>, though you can generate these yourself quite easily.</li>
</ul>

<p>Your language probably already has a list of primes, although for the bonus you may need to create you own list of Fibonacci Numbers</p>

## Bonus

<p>Now, a specific form is required for the 'Base Fib' answers.</p>

<p>Because each term of the sequence is the sum of the previous two, the 'Base Fib' form of a decimal number in the Fibonacci sequence can either be the term itself, or the previous two, e.g.</p>

<pre><code>8             = 100000
8 = 5 + 3     = 11000
8 = 5 + 2 + 1 = 10101
</code></pre>

<p>For the bonus challenge, give the output with the least <code>1</code>'s.</p>

### Bonus input

<pre><code>10 8
10 16
10 32
10 9024720
</code></pre>

## Bonus 2

<p>As <a href="/u/thorwing" class="userTagged">/u/thorwing</a><span class="RESUserTag"><a class="userTagLink RESUserTagImage" username="thorwing" title="set a tag" href="javascript:void 0"></a></span> <a href="#" class="voteWeight" style="display: none;">[vw]</a><span class="noCtrlF keyNavAnnotation" data-text="[2]" title="press 2 to open link"></span> suggested, it would be a greater challenge to write the base fib with the most <code>1</code>'s instead of the least</p>

## Finally

<p>Have a good challenge idea like <a href="/u/SovietKetchup" class="userTagged">/u/SovietKetchup</a><span class="RESUserTag"><a class="userTagLink RESUserTagImage" username="sovietketchup" title="set a tag" href="javascript:void 0"></a></span> <a href="#" class="voteWeight" style="display: none;">[vw]</a><span class="noCtrlF keyNavAnnotation" data-text="[3]" title="press 3 to open link"></span>?</p>

<p>Consider submitting it to <a href="/r/dailyprogrammer_ideas">/r/dailyprogrammer_ideas</a><span class="noCtrlF keyNavAnnotation" data-text="[4]" title="press 4 to open link"></span></p>

## Edit

<p>As some of you have pointed out, my solution had a small bug in it. </p>

<pre><code>9024720 -&gt; 1010100101010100000010001000010010
</code></pre>

# Implementation

Written in _Python 3.5_.

I made the converter into a webapp, because besides problem solving I wanted
to try out Docker containers.

```
pip install -r ./requirements.txt
python main.py
```

Runnable with _Docker 1.12_ and _Docker Compose 1.8_:

```
docker-compose up
```
