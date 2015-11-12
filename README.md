# Lemonade, the sparkling monad library

The **Lemonade** project aims at providing a comprehensive standard
monad library for OCaml.

[![Build Status](https://travis-ci.org/michipili/lemonade.svg?branch=master)](https://travis-ci.org/michipili/lemonade?branch=master)

It implements the following monads:

- **Lemonade_Continuation** for continuation passing style programming.
- **Lemonade_Lazy** for lazy computations.
- **Lemonade_List** for computations yielding several possible results.
- **Lemonade_Maybe** for computations yielding zero or one result.
- **Lemonade_Reader** for computations explicitly depending on some environment.
- **Lemonade_Retry** for retryable computations.
- **Lemonade_State** for computations modifying a state.
- **Lemonade_Success** for computations failing with context information.
- **Lemonade_Writer** for computations writing a log book.


## Setup guide

It is easy to install **Lemonade** using **opam** and its *pinning*
feature.  In a shell visiting the repository, say

```console
% autoconf
% opam pin add lemonade .
```

It is also possible to install **Lemonade** manually.
The installation procedure is based on the portable build system
[BSD Owl Scripts][bsdowl-home] written for BSD Make.

1. Verify that prerequisites are installed:
   - BSD Make
   - [BSD OWl][bsdowl-install]
   - OCaml
   - [Broken][broken-home]
   - [Mixture][mixture-home]
   - GNU Autoconf

2. Get the source, either by cloning the repository or by exploding a
   [distribution tarball](releases).

3. Optionally run `autoconf` to produce a configuration script. This
   is only required if the script is not already present.

4. Run `./configure`, you can choose the installation prefix with
   `--prefix`.

5. Run `make build`.

6. Optionally run `make test` to test your build.

7. Finally run `make install`.

Depending on how **BSD Make** is called on your system, you may need to
replace `make` by `bsdmake` or `bmake` in steps 5, 6, and 7.
The **GNU Make** program usually give up the ghost, croaking
`*** missing separator. Stop.` when you mistakingly use it instead of
**BSD Make**.

Step 7 requires that you can `su -` if you are not already `root`.


Michael Grünewald in Aachen, on November 12, 2015

  [licence-url]:        http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html
  [licence-en]:         COPYING
  [licence-fr]:         COPYING-FR
  [bsdowl-home]:        https://github.com/michipili/bsdowl
  [bsdowl-install]:     https://github.com/michipili/bsdowl/wiki/Install
  [broken-home]:        https://github.com/michipili/broken
  [mixture-home]:       https://github.com/michipili/mixture
