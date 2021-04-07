# Computing the Rank of Braids

[Read the paper](paper/thesis.pdf).
[View the slides](https://justinmeiners.github.io/braid-rank-thesis/)

## Working with the Source Code

If you want to build the code you will
need to install [sbcl][1] and [quicklisp][2].
Once those are installed just run

    ./install.sh

You can start an interactive session with

    ./run.sh

## Examples

Estimating the rank of a braid:

    (rank:upper-bound '(1 2 -1 3 4))
    (rank:lower-bound '(1 2 -1 3 4))


[1]: http://www.sbcl.org
[2]: https://www.quicklisp.org/beta/ 
