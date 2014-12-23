# py

*Python from Erlang/LFE: A Wrapper for ErlPort with Convenience Functions*

<img src="resources/images/Python-logo-notext.png"/>


## Introduction

This project provides an easier interface to Python, wrapping
[ErlPort](http://erlport.org/docs/python.html) calls. It was originally part of
the [lsci project](git@github.com:lfex/lsci.git), but was split out due to it
being generally useful.


## Requirements

To use py, you need the following:

* [lfetool](http://docs.lfe.io/quick-start/1.html)
* [Python 3](https://www.python.org/downloads/)


## Setup

For now, just run it from a git clone:

```bash
$ git clone git@github.com:lfex/py.git
$ cd py
$ make
```

Activate your Python virtualenv and then start up the LFE REPL:

```bash
$ . ./python/.venv/bin/activate
$ make repl-no-deps
```

Note that the ``repl`` and ``repl-no-deps`` targets automatically start up
the py (and thus ErlPort) Erlang Python server. If you run the REPL without
these ``make`` targets, you'll need to manually start things:

```bash
$ lfetool repl lfe -s py
```


## Usage

Below we show some basic usage of py from both LFE and Erlang. In a
separate section a list of docs are linked showing detailed usage of wrapped
libraries.


### Meta Data

First things first: let's make sure that you have the appropriate versions
of things -- in particular, let's confirm that you're running Python 3:

```cl
> (py-util:get-versions)
(#(erlang "17")
 #(emulator "6.3")
 #(driver-version "3.1")
 #(lfe "0.9.0")
 #(py "0.0.1")
 #(python
   ("3.4.2 (v3.4.2:ab2c023a9432, Oct  5 2014, 20:42:22)"
    "[GCC 4.2.1 (Apple Inc. build 5666) (dot 3)]")))
>
```


### Module Level


#### Calling Functions

```cl
> (py:call 'os 'getcwd)
"/Users/yourname/lab/erlang/py"
```


#### Module Constants

### Objects
#### Instantiation
#### Calling Methods
#### Attribute Values


#### Erlang

We can, of course, do all of this from Erlang:

```bash
$ make shell-no-deps
```

```erlang
1> 'py-util':'get-versions'().
[{erlang,"17"},
 {emulator,"6.3"},
 {'driver-version',"3.1"},
 {lfe,"0.9.0"},
 {py,"0.0.1"},
 {python,["3.4.2 (v3.4.2:ab2c023a9432, Oct  5 2014, 20:42:22)",
          "[GCC 4.2.1 (Apple Inc. build 5666) (dot 3)]"]},
 {numpy,"1.9.1"},
 {scipy,"0.14.0"}]
2> py:py(os, getcwd).
"/Users/yourname/lab/erlang/py"
```

