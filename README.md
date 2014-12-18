# lsci

*An LFE Wrapper Library for SciPy, NumPy, and matplotlib*

## Introduction

This project is currently more of a thought experiment than anything else,
and will require several other (infrastructure) libraries to be created
before becoming a reality. But you've got to start somewhere :-)

And it's pronounced "Elsie".


## Development

For the latest thoughts on interface, archiecture, etc., see the [project Wiki](https://github.com/oubiwann/lsci/wiki).


## Installation

For now, just run it from a git clone:

```bash
$ git clone git@github.com:oubiwann/lsci.git
$ cd lsci
$ make
```


## Usage

Activate your Python virtualenv, update your ``$PATH``, and then start up
the LFE REPL:

```bash
$ . ./python/.venv/bin/activate
$ make repl-no-deps
```

Note that the ``repl`` and ``repl-no-deps`` targets automatically start up
the lsci (and thus ErlPort) Erlang Python server. If you run the REPL without
these ``make`` targets, you'll need to manually start things:

```bash
$ lfetool repl lfe -s lsci
```

```cl
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:4:4] [async-threads:10] ...

LFE Shell V6.2 (abort with ^G)
>
```

First things first: let's make sure that you have the appropriate versions
of things -- in particular, let's confirm that you're running Python 3:

```cl
> (lsci-util:get-versions)
(#(erlang "17")
 #(emulator "6.2")
 #(driver-version "3.1")
 #(lfe "0.9.0")
 #(lsci "0.0.1")
 #(python
   ("3.4.2 (v3.4.2:ab2c023a9432, Oct  5 2014, 20:42:22)"
    "[GCC 4.2.1 (Apple Inc. build 5666) (dot 3)]")))
>
```

Now we can start making calls:

```cl
> (lsci-math:pi)
3.141592653589793
> (lsci-math:e)
2.718281828459045
> (lsci-math:phi 6)
0.9999999990134123
> (lsci-math:ceil 5.1)
6
> (lsci-math:floor 5.1)
5
> (lsci-math:round 5.67)
6
> (lsci-math:round 5.6666667 1)
5.7
> (lsci-math:round 5.6666667 3)
5.667
```

Complex numbers are suppored; in fact, there's a whole module dedicated
to them:

```cl
> (lsci-cmath:phase (lsci-cmath:complex -1.0 0.0))
3.141592653589793
> (lsci-cmath:phase (lsci-cmath:complex -1.0 -0.0))
-3.141592653589793
```
