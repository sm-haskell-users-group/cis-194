# cis194

Homework solutions for UPenn's CIS 194:

http://www.seas.upenn.edu/~cis194/spring13/index.html

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
