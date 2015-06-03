Lusp
====

Lusp is an interpreter for a scheme-like language written in Haskell
and inspired by [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

Installing
==========

```sh
git clone https://github.com/locksten/lusp /tmp/lusp
cd /tmp/lusp/library    && cabal install
cd /tmp/lusp/executable && cabal install
```

Usage
=====

Running a file:
```
$ lusp /tmp/lusp/examples/gradient.scm 2 25 linear 25 23
xx;;;;;;:::::,,,,,.....   
xxx;;;;;::::::,,,,,.....  
xxxx;;;;;::::::,,,,,..... 
```

Running the repl:
```scheme
$ lusp
Lusp> (load "/tmp/lusp/examples/sort.scm")
Lusp> (sort < '(1 9 2 8 3 7 4 6 5))
Lusp> (1 2 3 4 5 6 7 8 9)
Lusp> exit
```

Examples
========

- [sort](examples/sort.scm)
- [stdlib](library/data/stdlib.scm)
- [gradient](examples/gradient.scm)
- [fibonacci](examples/fibonacci.scm)
