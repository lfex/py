# NumPy in LFE

## Array attributes

## Array methods

```cl
> (include-lib "lutil/include/compose.lfe")
loaded
> (set array (lsci-np:array '((1 2 3) (4 5 6) (7 8 9))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci:py-type array)
numpy.ndarray
> (lsci-np:tolist array)
((1 2 3) (4 5 6) (7 8 9))
(->> array
     (lsci-np:transpose)
     (lsci-np:tolist))
((1 4 7) (2 5 8) (3 6 9))
```

```cl
> (lsci-np:tostring array "C")
#B(1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 4 0 0 0 0 0 ...)
> (lsci-np:tostring array)
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
> (lsci-np:size array)
8
> (lsci-np:shape array)
#(4 2)
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
