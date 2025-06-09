# Cabal Package Collections

At the moment, multiple package repositories in a cabal project mostly work.

If you try to depend a package present in a single repository, things work. But if a package is present in *n* repositories,
the behaviour of the cabal solver is not intuitive or very predictable.
Two packages with the same name and version but with different interfaces will give annoying behaviour for the end user.

So, the solution is to have a declarative language that describes which packages are allowed to come from where.
