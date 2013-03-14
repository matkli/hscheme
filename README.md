hscheme
=======

This is a simple interpreter for (a subset of) Scheme written in Haskell, with
inspiration from Haskell tutorial ["Write Yourself a Scheme in 48 Hours"][1]
by Jonathan Tang.

Copyright 2008, 2013 Mats Klingberg

This project is licensed under the GNU GPL, see the file LICENSE in this
directory for the full license text.

[1]: http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html


Installation
------------

Download source and then build using cabal:

    cabal configure
    cabal build

and, optionally:

    cabal install


Usage
-----

Start the interpreter by running `hscheme` (it should be in the directory
`dist/build/hscheme` if you didn't run the install command above).

Example:

    >>> (+ 1 1)
    2
    >>> (define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))
    #undefined
    >>> (fact 5)
    120
