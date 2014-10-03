# cis194

Homework solutions for UPenn's CIS 194:

[http://www.seas.upenn.edu/~cis194/](http://www.seas.upenn.edu/~cis194/)

## Installation

On a mac:

    # Add Cabal path to PATH
    touch ~/.bashrc
    echo 'export PATH=$HOME/Library/Haskell/bin:$PATH' >> ~/.bashrc

    # Install haskell-platform
    brew install haskell-platform

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

## Branching Strategy

Branch `master` will contain tests and stubbed homework assignments only; please create a topic branch for yourself to which you'll commit your homework solutions.

## Miscellaneous Links

1. [Basic GHCI commands](https://gist.github.com/laser/789b4416a0aa55bafd72)
1. [Imperative debugging of Haskell code](https://gist.github.com/laser/3a37ffbfd73ec21ba399)
