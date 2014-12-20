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
> (lsci-np:tostring array)
#B(1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 4 0 0 0 0 0 ...)
> (lsci-np:tostring array "C")
#B(1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 4 0 0 0 0 0 ...)
> (lsci-np:tostring array "F")
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
> (lsci-np:tolist array)
((1.0 1.0) (1.0 1.0) (1.0 1.0) (1.0 1.0))
```

```cl
> (set array (lsci-np:zeros '#(4 4)))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:tolist array)
((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
```


### From existing data

```cl
> (set array (lsci-np:array '((1 2 3) (4 5 6) (7 8 9))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci:py-type array)
numpy.ndarray
```

```cl
> (set matrix (lsci-np:asmatrix '((1 2 3) (4 5 6) (7 8 9))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci:py-type matrix)
numpy.matrixlib.defmatrix.matrix
```

### Creating record arrays

### Creating character arrays

### Numerical ranges

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
