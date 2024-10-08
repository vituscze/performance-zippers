# Performance Analysis of Zippers

This repository contains the source code used for comparing the performance of zippers and mutable pointers.

* Tree insertion code in [C++](build.cpp), [C#](build.cs) and [Haskell](build.hs).
* Tree traversal code in [C++](traverse.cpp), [C#](traverse.cs) and [Haskell](traverse.hs).
* Generate script for tree traversal in [Haskell](generate.hs).
* Raw results in [CSV format](data.csv).

## Usage

C++ and C# programs accept one command line argument: time limit in milliseconds.
Haskell programs use the [criterion](http://hackage.haskell.org/package/criterion)
package where the time limit is given by `-L <time in seconds>`.

The tree traversal programs read the input array from standard input. This input can
be generated by using the generate script mentioned above. The script can be used in the
following way:

    runhaskell generate.hs <node count> <tree depth> <left-right bias> <top-bottom bias>

The result is written to stdout.

## Compilation

The source files were compiled with:

* C#

      csc /optimize /t:exe /out:<name>_cs.exe <name>.cs

* C++

      g++ -O3 -std=c++17 -o <name>_cpp.exe <name>.cpp

* Haskell

      ghc -O2 -fllvm -o <name>_hs.exe <name>.hs

## Dependencies

C++ and C# programs depend on their respective standard libraries. Haskell depends on the following packages:

* `build.hs`: base, criterion
* `traverse.hs`: base, bytestring, criterion, mtl, unboxed-ref, vector
* `generate.hs`: base, mtl, random

Haskell source files contain cabal metadata blocks that can be used with
`cabal run` to handle the dependencies automatically. Make sure to pass
the correct GHC options with `--ghc-options=`.

## Input Files

The input files for testing no bias, bottom bias, right bias, and bottom-right bias were
generated as follows:

    runhaskell generate.hs 1000000 20 0 2 > balanced.txt
    runhaskell generate.hs 1000000 20 0 5 > bottom.txt
    runhaskell generate.hs 1000000 20 1 2 > right.txt
    runhaskell generate.hs 1000000 20 1 5 > bottom_right.txt

The script isn't really optimized, you might wish to compile the generate script to
speed up this process.
