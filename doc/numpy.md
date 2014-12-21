# NumPy in LFE

## Array attributes

```cl
> (set array (lsci-np:array '((1 2 3) (4 5 6) (7 8 9))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:size array)
9
> (lsci-np:shape array)
#(3 3)
> (lsci-np:strides array)
#(24 8)
> (lsci-np:ndim array)
2
> (lsci-np:data array)
#($erlport.opaque python
  #B(128 2 99 95 95 98 117 105 108 116 105 110 95 95 10 109 101 109 ...))
> (lsci-np:size array)
9
> (lsci-np:itemsize array)
8
> (lsci-np:nbytes array)
72
```

## Array methods

```cl
> (include-lib "lutil/include/compose.lfe")
loaded
> (lsci-np:tolist array)
((1 2 3) (4 5 6) (7 8 9))
(->> array
     (lsci-np:transpose)
     (lsci-np:tolist))
((1 4 7) (2 5 8) (3 6 9))
```

```cl
> (lsci-np:->string array)
#B(1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 4 0 0 0 0 0 ...)
> (lsci-np:->string array "C")
#B(1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 4 0 0 0 0 0 ...)
> (lsci-np:->string array "F")
#B(1 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 2 0 0 0 0 0 ...)
```

```cl
> (lsci-np:dump array "array.dat")
ok
```

## Array creation routines

### Ones and zeros

```cl
> (set array (lsci-np:ones '#(4 2)))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list array)
((1.0 1.0) (1.0 1.0) (1.0 1.0) (1.0 1.0))
```

```cl
> (set array (lsci-np:zeros '#(4 4)))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list array)
((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
```


### From existing data

```cl
> (set array (lsci-np:array '((1 2 3) (4 5 6) (7 8 9))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-py:type array)
numpy.ndarray
```

```cl
> (set matrix (lsci-np:asmatrix '((1 2 3) (4 5 6) (7 8 9))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-py:type matrix)
numpy.matrixlib.defmatrix.matrix
```

### Creating record arrays

### Creating character arrays

### Numerical ranges

```cl
> (set a (lsci-np:arange 4))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list a)
(0 1 2 3)
> (set a (lsci-np:arange 4.0))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list a)
(0.0 1.0 2.0 3.0)
> (lsci-np:->list a)
(0.0 1.0 2.0 3.0)
> (set a (lsci-np:arange 4 12))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list a)
(4 5 6 7 8 9 10 11)
> (set a (lsci-np:arange 4 24 2))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list a)
(4 6 8 10 12 14 16 18 20 22)
```

```cl
> (set a (lsci-np:linspace 2.0 3.0))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list a)
(2.0 2.020408163265306 2.0408163265306123 2.061224489795918
 2.0816326530612246 2.1020408163265305 2.122448979591837
 2.142857142857143 2.163265306122449 2.183673469387755
 2.204081632653061 2.2244897959183674 2.2448979591836733
 2.2653061224489797 2.2857142857142856 2.306122448979592
 2.326530612244898 2.3469387755102042 2.36734693877551
 2.387755102040816 2.4081632653061225 2.4285714285714284
 2.4489795918367347 2.4693877551020407 2.489795918367347
 2.510204081632653 2.5306122448979593 2.5510204081632653
 2.571428571428571 2.5918367346938775 ...)
> (length (lsci-np:tolist a))
50
> (set a (lsci-np:linspace 2.0 3.0 `(#(num 5))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list a)
(2.0 2.25 2.5 2.75 3.0)
> (set a (lsci-np:linspace 2.0 3.0 `(#(num 5) #(endpoint false))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108...))
> (lsci-np:->list a)
(2.0 2.2 2.4 2.6 2.8)
> (set `#(,a ,spacing) (lsci-np:linspace 2.0 3.0 `(#(num 5) #(retstep true))))
#(#($erlport.opaque python
    #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
  0.25)
> (lsci-np:->list a)
(2.0 2.25 2.5 2.75 3.0)
> spacing
0.25
```

```cl
> (set `(,nx ,ny) '(3 2))
(3 2)
> (set x (lsci-np:linspace 0 1 `(#(num ,nx))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (set y (lsci-np:linspace 0 1 `(#(num ,ny))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
```

```cl
> (set `(,xv ,yv) (lsci-np:meshgrid `(,x ,y)))
(#($erlport.opaque python
   #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
 #($erlport.opaque python
   #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...)))
> (lsci-np:->list xv)
((0.0 0.5 1.0) (0.0 0.5 1.0))
> (lsci-np:->list yv)
((0.0 0.0 0.0) (1.0 1.0 1.0))
```

```cl
> (set `(,xv ,yv) (lsci-np:meshgrid `(,x ,y) `(#(sparse true))))
(#($erlport.opaque python
   #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
 #($erlport.opaque python
   #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...)))
> (lsci-np:->list xv)
((0.0 0.5 1.0))
> (lsci-np:->list yv)
((0.0) (1.0))
```

### Building matrices

### The Matrix class

## Array manipulation routines

## Binary operations

## String operations

## C-Types Foreign Function Interface

## Datetime Support Functions

## Data type routines

## Optionally Scipy-accelerated routines (numpy.dual)

## Mathematical functions with automatic domain (numpy.emath)

## Floating point error handling

## Discrete Fourier Transform (numpy.fft)

## Financial functions

## Functional programming

## Numpy-specific help functions

## Indexing routines

## Input and output

## Linear algebra (numpy.linalg)

## Logic functions

## Masked array operations

## Mathematical functions

## Matrix library (numpy.matlib)

## Miscellaneous routines

## Padding Arrays

## Polynomials

## Random sampling (numpy.random)

## Set routines

## Sorting, searching, and counting

## Statistics
