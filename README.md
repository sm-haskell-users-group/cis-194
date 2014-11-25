# cis194

Homework solutions for UPenn's CIS 194:

[http://www.seas.upenn.edu/~cis194/](http://www.seas.upenn.edu/~cis194/)

## Installation

On a mac:

    # Add Cabal path to PATH
    touch ~/.bashrc
    echo 'export PATH=$HOME/Library/Haskell/bin:$PATH' >> ~/.bashrc

    # Install haskell-platform
    brew install ghc cabal-install

    # Clone repo
    git clone https://github.com/sm-haskell-users-group/cis-194 cis194
    cd cis194

On Linux:

    # Add Cabal path to PATH
    touch ~/.bashrc
    echo 'export PATH=$PATH:$HOME/.cabal/bin' >> ~/.bashrc

    # Clone repo
    git clone https://github.com/sm-haskell-users-group/cis-194 cis194
    cd cis194

Then...

    cabal update
    cabal install hspec
    cabal configure --enable-test
    cabal build

## How to run a single week's tests

```
make week1
```

## How to run ALL the tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Where to Put Your Work

Branch `master` will contain tests and stubbed homework assignments only; please create a topic branch for yourself to which you'll commit your homework solutions.

Example:

```sh
~/dev/sm-cis194 $ git checkout -b erin
Switched to a new branch 'erin'

~/dev/sm-cis194 $ touch .foo
~/dev/sm-cis194 $ git add .
~/dev/sm-cis194 $ git commit -m "adding foo"
~/dev/sm-cis194 $ git push origin erin
```

## Miscellaneous Links

1. [Basic GHCI commands](https://gist.github.com/laser/789b4416a0aa55bafd72)
1. [Imperative debugging of Haskell code](https://gist.github.com/laser/3a37ffbfd73ec21ba399)

--------

## CIS 194 Schedule ##

date    | which homework due? | who is presenting? | what's the topic?    
------- | ------------------- | ------------------ | -----------------
WED, Nov. 26 | [Markets / monoids](http://www.seas.upenn.edu/~cis194/hw/06-monoid-io.pdf) | Erin | Lazy Evaluation
THU, Dec. 4  | [Streams](http://www.seas.upenn.edu/~cis194/hw/07-laziness.pdf) | ??? | Monads or Functor + Applicative
THU, Dec. 11 | N/A | N/A | N/A
THU, Dec. 18 | ??? | ??? | ???
THU, Dec. 25 | N/A | N/A | N/A
FRI, Jan. 2  | ??? | ??? | ???

## Remaining Lectures ##

1. [Functor](http://www.seas.upenn.edu/~cis194/spring13/lectures/09-functors.html)
2. Applicative Functor ([here](http://www.seas.upenn.edu/~cis194/spring13/lectures/10-applicative.html) and [here](http://www.seas.upenn.edu/~cis194/spring13/lectures/11-applicative2.html))
3. Monads ()[here](http://www.seas.upenn.edu/~cis194/spring13/lectures/12-monads.html) and [here](http://www.seas.upenn.edu/~cis194/lectures/08-monads.html))
4. [Quickcheck](http://www.seas.upenn.edu/~cis194/lectures/09-testing.html)
5. [Template Haskell](http://www.seas.upenn.edu/~cis194/lectures/11-template-haskell.html)

## Fun Topics ##

1. Reader, Writer monads
2. Concurrency Primitives
3. Lens
4. Foldable
5. Traversable
6. Phantom Types
7. GADTs
