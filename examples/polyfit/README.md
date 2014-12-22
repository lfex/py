# Examples: Polynomial Curve Fitting


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
> (lsci-asciiplot:scatter xs ys)
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

The [NIST data set](http://www.itl.nist.gov/div898/strd/lls/data/LINKS/DATA/Filip.dat)
provided a polynomial describing this data:

```
y = B0 + B1*x + B2*(x**2) + ... + B9*(x**9) + B10*(x**10) + e
```

Or, if you prefer LaTeX:

\begin{equation}
y = B_0 + B_1{x} + B_2{x^2} + ... + B_9{x^9} + B_{10}{x^{10}} + e
\end{equation}

Using NumPy, we can easily fit an 10th-degree polynomial curve to this data.
We will use ``numpy.polyfit`` for finding a least squares polynomial fit,
passing it the $x$ and $y$ values for the data to fit as well as the degree
of our polynomial:


```cl
(set coeffs (lsci-np:polyfit xs ys 10))
#($erlport.opaque python ...)
```

Let's peek at the data:

```cl
> (lsci-np:->list coeffs)
(-4.029625205186532e-5 -0.0024678107546401437 -0.06701911469215643
 -1.0622149736864719 -10.875317910262362 -75.12420087227511
 -354.47822960532113 -1127.973927925715 -2316.371054759451
 -2772.1795597594755 -1467.4895971641686)
```

``np.polyplot`` can return more data, if you are so inclined:

```cl
> (set `#(,coeffs
          ,residuals
          ,rank
          ,singular-values
          ,rcond) (lsci-np:polyfit xs ys 10 '(#(full true))))
#($erlport.opaque python ...)
> (lsci-np:->list coeffs)
(-4.029625205186532e-5 -0.0024678107546401437 -0.06701911469215643
 -1.0622149736864719 -10.875317910262362 -75.12420087227511
 -354.47822960532113 -1127.973927925715 -2316.371054759451
 -2772.1795597594755 -1467.4895971641686)
> (lsci-np:->list residuals)
(7.958513839371895e-4)
> rank
11
> (lsci-np:->list singular-values)
(3.128894711145785 1.064548669029962 0.27180324022363517
 0.05296821542551952 0.008387108325776571 0.0010157565988992792
 9.583030547029836e-5 7.605115790256685e-6 4.6491044714423815e-7
 1.9871421381342612e-8 6.009222284310632e-10)
> (lsci-np:->list rcond)
1.8207657603852567e-14
```

There is a conveience class in NumPy ``numpy.poly1d`` that, once instantiated
with our fit data, we can use to evaluate at any given point. Let's try it
out:

```cl
> (set model (lsci-np:poly1d coeffs))
#($erlport.opaque python ...)
```

Let's call this function against several values as a sanity check:

```cl
> (lsci-np:->list (lsci-py:func-call model '(-9)))
0.7766886098502255
> (lsci-np:->list (lsci-py:func-call model '(-7)))
0.7990591787051926
> (lsci-np:->list (lsci-py:func-call model '(-6)))
0.8860483219018533
> (lsci-np:->list (lsci-py:func-call model '(-5)))
0.8926343904781788
> (lsci-np:->list (lsci-py:func-call model '(-4)))
0.9094348679923314
> (lsci-np:->list (lsci-py:func-call model '(-3)))
0.8893022773313533
```

Looking back at our graph, we can see that these check out fine.

## Polynomial Linear Regression

TBD

