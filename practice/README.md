
# A story of chunks and slices

Here's a short resume/explnation for the practice round of
the 2018 edition of the google hashcode. So, I'll suppose you
know the problem.

The way the solution works is by cutting the pizza into areas
of reasonnable size that we'll call chunks (since slice is
already taken), and then try and find the exact best solution for
each chunk. Then we concatenate the solutions.

## Chunks and Dynamic programming

Let's consider a chunk, or equivalently, a pizza of reasonable size,
how can we find the optimal slice partitioning in reasonable time ?
Dynamic programming is the easy answer and works quite well seing the
results we got.

Let's consider a rectangle pizza, preferably with a small number of rows,
let's say 4. We will consider a dynamic algorithm with keys / indexed by
an array of columns, let's say `[c0,c1,c2,c3]`. This index represents
4 points of the pizza, each on a different row, as follows `(0,c0)`,
`(1,c1)`, `(2,c2)` and `(3,c3)`. We will try and compute the optimal solution
for the sub-pizza that is strictly to the left of these points, that is
the sub-pizza constituted of the points on row `0` with a column index
strictly smaller than `c0`, and so on...
Now the question is how to express a relation between the optimal solution
for a given index and other indexes. Rather than give a recursive equation
which express the optimum solution as a function of solutions on smaller indexes,
we'll do the inverse and given an optimal solution for an index, compute
the best solution it provides for other greater indexes. To do so, let's say
we have the following index:

```
  0123456789
0 ...|______
1 ..|_______
2 .|________
3 .....|____
```

The vertical bars `|` represent the cells in the index, the dots on the left are "used"
cell, i.e cells we consider filled and unusable to extend the current solution,
and finally underscores are unused cells.

We will try and "fill" the leftmost, topmost non-used cell, in this case the
cell at `(1, 3)`, to do so we can either:
- insert a rectangle with top-left corner at exactly `(1,3)`
- do nothing, consider the `(1,3)` cell used, and contiue to the next index

In the first case, we will compute all legal rectangles that satisfy the
mushroom/tomato requirements (and with top-left corner the `(1,3)` cell),
and together with the solution not using the cell, that gives us a list of
new potential solutions (with new indexes), that we add to a hashtable.
The new indexes are then added to a queue to be treated in next iterations of the loop.

With that we have an algorithm for finding optimal solutions of chunks. Interestingly,
it is enough to automatically find an optimal solution for the small input (although the
optimal solution is not hard to find by hand, so it's more of a nice fact rather than useful),
however it obviously cannot scale to the medium and big inputs.

## Cutting into chunks

Since the optimal solution can't run reasonably on the whole pizza of the medium and big outputs,
a first solution is to cut it into chunks, and find the optimal solution of each chunk.
This is easy to do by cutting the pizza into horizontal chunks that span all columns of the
pizza, but only a few rows. That's what the `split_and_conquer` function in `dyn.ml' does.
Luckily, with only that, and chuks of size 4, we got solutions that were actually quite close
to the theoretical maximums. These were our first solutions:
- for the medium output, we got 49 970 out of a total of `200 \* 250 = 50 000` points, which means
  that only `30` cells were not part of any slice in the solution
- for the big output, we got 951 223 out of the total of `1000 \* 1000 = 100 000` points, which means
  that `only` about 50 000 cells weren't used by our slices.

So that brought us to the 1 001 250 points mentionned in my google post. However, we weren't actually
quite finished.

## Dynamically sizing chunks

Since we already had one dynamic algorithm why not use another ! The idea was to compute the optimal solution
of all chunks of size 1, 2, up to a given `n`, and the use another dynamic agorithm to choose the best arrangement
of chunks.
- Using `n=3` that brought us a solution with 49982 points for the medium input. Using `n=4`, which takes significantly
  longer, we got a solution with 49996 points, only 4 cells unused !
- On the big input, we directly tried with `n=4` and that gave us our final solution worth 963236 points, which is
  quite satisfying (it took about 15 minutes to run on a 20-core machine using 30 processes (and sadly not 40 because
  that caused an Out of memory error in at least one chunk)).

## Grinding for 3 more points

But we weren't finished ! Instead of focusing on the big input, which gives a lot more points, we really, really wanted
to find an optimal and complete solution for the medium input, because we felt we were sooo close. So on a hunch,
we tried to run our solution on the transpose of the pizza, or rather we could say we choose to use vertical chunks
instead of horizontal ones. With that we got a 49 999 point solution for the medium output ! unfortunately, on the
big output, it actually returned a worse solution than the previous one, so this trick only won us 3 points, but still
it was clearly worth it.

So that was what we did before calling it a night (we had worked on the problem for about 4 hours I'd say). But we
clearly intend to find that missing point on the medium input, because it would be really nice to eat all that delicous
pizza.

