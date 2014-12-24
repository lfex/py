# py

*Python from Erlang/LFE: A Wrapper around ErlPort with Convenience Functions*

<img src="resources/images/Python-logo-notext-small.png"/>


## Introduction

This project provides an easier interface to Python, wrapping
[ErlPort](http://erlport.org/docs/python.html) calls. It was originally part of
the [lsci project](git@github.com:lfex/lsci.git), but was split out due to it
being generally useful.

This project lets you do the following very easily:

* Make module-level calls
* Get module-level constants
* Instantiate objects
* Call object methods
* Get object attributes

It also provides easy access to Python builtins and operators.


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
 #(lfe-py "0.0.1")
 #(python
   ("3.4.2 (v3.4.2:ab2c023a9432, Oct  5 2014, 20:42:22)"
    "[GCC 4.2.1 (Apple Inc. build 5666) (dot 3)]")))
>
```


### Module Level

The following sections describe module-level operations.


#### Calling Functions

```cl
> (py:func 'os 'getcwd)
"/Users/yourname/lab/erlang/py"
> (py:func 'datetime.datetime 'now)
#("datetime" #(2014 12 23 16 57 11 693773 undefined))
```


#### Module Constants

```cl
> (py:const 'math 'pi)
3.141592653589793
```

Optionaly, you may provide a type:

```cl
> (py:const 'math 'pi 'float)
3.141592653589793
> (py:const 'math 'pi 'int)
3
> (py:const 'math 'pi 'str)
"3.141592653589793"
```


### Objects

The following sections describe how to work with Python objects.


#### Instantiation

With no arguments passed to the constructor:

```cl
> (py:init 'builtins 'dict)
#("dict" ())
> (py:init 'collections 'UserDict)
#("UserDict" ())
```

With args:

```cl
> (py:init 'datetime 'date '(1923 4 2))
#("date" #(1923 4 1))
```

Note that strings need to be converted to binary:

```cl
> (py:func 'os.path 'isfile '(#b("/tmp")))
false
> (py:func 'os.path 'isdir '(#b("/tmp")))
true
```

Keyword arguments are passed as proplists, e.g.,
``'(#(key1 val1) #(key2 val2))``. In the next example we'll pass a string (as
a binary) which represents a binary number. We'll give ``int`` the keyword of
``base``, since we're not going to use the default decimal base (10):

```cl
(py:func 'builtins 'int '(#b("101010")) '(#(base 2)))
42
```

#### Calling Methods

To call a method, we need an object. Let's return to the date example
above:

```cl
> (set now (py:func 'datetime.datetime 'now))
#("datetime" #(2014 12 23 23 14 37 677463 undefined))
```

The tuple representing a date time object has been saved as the ``now``
variable in the REPL. Let's call some methods:

```cl
> (py:method now 'strftime '(#b("%Y.%m.d %H:%M:%S")))
"2014.12.d 23:14:37"
```

#### Attribute Values

Continuing with that same object:

```cl
> (py:attr now 'year)
2014
> (py:attr now 'microsecond)
677463
```


#### Operations on Objects

Let's get another time ... and give our other variable a better name:

```cl
> (set later (py:func 'datetime.datetime 'now))
#("datetime" #(2014 12 23 23 21 25 714474 undefined))
> (set earlier now)
#("datetime" #(2014 12 23 23 14 37 677463 undefined))
```

Let's use the two objects in a calculation:

```cl
> (set diff (py:func 'operator 'sub `(,later ,earlier)))
#("timedelta" 0 408 37011)
> (py:attr diff 'seconds)
408
```

Let's get that in minutes:

```cl
> (/ (py:attr diff 'seconds) 60)
6.8
```


### Builtins

In several of the examples above, we made calls to the ``builtins`` module.
LFE py actually provides wrappers for these, making such calls much easier.
For example:

```cl

```


### Erlang

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

