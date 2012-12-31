# Delimited, answer-type-polymorphic continuations for Haskell.

This package provides delimited continuations that are [answer-type
polymorphic][atp]; the original formulation was [done by Oleg
Kiselyov][oleg].

[atp]: http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf
[oleg]: http://okmij.org/ftp/continuations/implementations.html#genuine-shift

The name of the package is in homage to Kenichi Asai.

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install asai
```

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-asai.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-asai.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-asai/master/AUTHORS.txt).

# License

MIT. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/hs-asai/master/LICENSE.txt)
for terms of copyright and redistribution.

[main page]: http://thoughtpolice.github.com/hs-asai
[contribute]: https://github.com/thoughtpolice/hs-asai/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/hs-asai/issues
[gh]: http://github.com/thoughtpolice/hs-asai
[bb]: http://bitbucket.org/thoughtpolice/hs-asai
[Hackage]: http://hackage.haskell.org/package/asai
