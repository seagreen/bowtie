# Overview

There are actually a number of reasons NOT to contribute to Bowtie (mainly due to it being an educational language designed to be used in a book):

## No compensation

While the code is open source, I'm compensated for it indirectly via sales of the book. I don't expect anyone to work to help make me money, and so am fine doing all the coding myself.

## Understanding restriction

Because it's meant to support a book, I need a deep understanding of all parts of the code. To understand things I usually need to do them myself, so I'm not likely to accept PRs for big features or changes.

## Particular style

The codebase is meant to be understandable by new Haskellers. One of the tools to do that is a distinct style (e.g. note the preference for `fmap` over `<$>` and parentheses over `$`) and consistency of style is more important to it than most other projects.

# Conclusion

Opening issues or forking Bowtie are likely to be more fruitful than making PRs (forks are especially satisfying, as Bowtie is meant to be a reasonable starting point for language experiments).

If want to make a PR anyway, probably ask first, and then in the PR add your name to [CONTRIBUTORS.md](CONTRIBUTORS.md).
