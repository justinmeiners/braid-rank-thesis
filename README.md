# Computing the Rank of Braids

[Read the paper][3].
[View the slides summary][4]
(also available as a PDF in this repo.)

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

## Citing

    @mastersthesis{computing-braid-rank,
      author = {Justin Meiners}, 
      title = {Computing the Rank of Braids},
      school = {Brigham Young University},
      year = 2021,
      month = 4
    }


[1]: http://www.sbcl.org
[2]: https://www.quicklisp.org/beta/ 
[3]: https://github.com/justinmeiners/braid-rank-thesis/raw/master/paper/thesis.pdf
[4]: https://justinmeiners.github.io/braid-rank-thesis/
