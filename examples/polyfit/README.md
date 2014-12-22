# Examples: Polynomial Curve Fitting with ``lsci-np``


## Introdcution

This example demonstrates how to use lsci to perform a polynomial fit for a
given data set. The "observed data" we're going to use is the same data set as
that used in the
[Incanter linear regression tutorial]](http://data-sorcery.org/2009/06/04/linear-regression-with-higher-order-terms/),
the
[NIST Filip.dat](http://www.itl.nist.gov/div898/strd/lls/data/LINKS/DATA/Filip.dat)
file. The data file we will use is a conversion of the original NIST file to
CSV.


## Loading Data

Let's load our experimental data:

```cl
> (set data (lsci-np:genfromtxt
              "examples/polyfit/filip.csv"
              `(#(delimiter ,(list_to_binary ","))
                #(names true))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
```

The data returned by this function is an ErlPort binary wrapping a Python
pickle of a NumPy array. We can take a look at the data by converting it to
a list:

```cl
> (lsci-np:->list data)
(#(0.8116 -6.860120914)
 #(0.9072 -4.324130045)
 #(0.9052 -4.358625055)
 #(0.9039 -4.358426747)
 #(0.8053 -6.955852379)
 #(0.8377 -6.661145254)
 #(0.8667 -6.355462942)
 #(0.8809 -6.118102026)
 #(0.7975 -7.115148017)
 #(0.8162 -6.815308569)
 #(0.8515 -6.519993057)
 #(0.8766 -6.204119983)
 #(0.8885 -5.853871964)
 #(0.8859 -6.109523091)
 #(0.8959 -5.79832982)
 #(0.8913 -5.482672118)
 #(0.8959 -5.171791386)
 #(0.8971 -4.851705903)
 #(0.9021 -4.517126416)
 #(0.909 -4.143573228)
 #(0.9139 -3.709075441)
 #(0.9199 -3.499489089)
 #(0.8692 -6.300769497)
 #(0.8872 -5.953504836)
 #(0.89 -5.642065153)
 #(0.891 -5.031376979)
 #(0.8977 -4.680685696)
 #(0.9035 -4.329846955)
 #(0.9078 ...)
 #(...) ...)
```


## Plotting Our Data

Let's look at the *x* and *y* values separately:

```cl
> (set xs (lsci-np:get data 'x))
...
> (set ys (lsci-np:get data 'y))
...
> (lsci-np:->list xs)
(-6.860120914 -4.324130045 -4.358625055 -4.358426747 -6.955852379
 -6.661145254 -6.355462942 -6.118102026 -7.115148017 -6.815308569
 -6.519993057 -6.204119983 -5.853871964 -6.109523091 -5.79832982
 -5.482672118 -5.171791386 -4.851705903 -4.517126416 -4.143573228
 -3.709075441 -3.499489089 -6.300769497 -5.953504836 -5.642065153
 -5.031376979 -4.680685696 -4.329846955 -3.928486195 -8.56735134 ...)
> (lsci-np:->list ys)
(0.8116 0.9072 0.9052 0.9039 0.8053 0.8377 0.8667 0.8809 0.7975
 0.8162 0.8515 0.8766 0.8885 0.8859 0.8959 0.8913 0.8959 0.8971
 0.9021 0.909 0.9139 0.9199 0.8692 0.8872 0.89 0.891 0.8977 0.9035
 0.9078 0.7675 ...)
```

Now let's plot our data in the terminal:

```cl
(lsci-asciiplot:scatter xs ys)
```

Which will give tou something like this:

```
                                                                            o  oo  o           o
                                                                   o   o  o
                                                                    o   o
                                                                   oo o
                                                                oo
                                                                  o
                                                              o
                                                            ooo
                                                            o


                                                         oo
                                                        o

                                                     o

                                                    o o

                                                    o

                                                 o
                                                o o

                                             o  o

                                         o    o
                                            o
                                          o

                                       o
                           o o o   o o
o               o      o
 o  o o  o  o o    o o
     o
  o

ok
```


## Curve Fitting

TBD
