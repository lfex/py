# NumPy in LFE

## Array creation routines

```cl
> (set array (lsci-np:ones '#(4 2)))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...
> (lsci:py-attr array "size")
8
> (lsci:py-attr array "shape")
#(4 2)
> (lsci:py-call array "tolist")
((1.0 1.0) (1.0 1.0) (1.0 1.0) (1.0 1.0))
```

### Ones and zeros

### From existing data

### Creating record arrays

### Creating character arrays

### Numerical ranges

### Building matrices

### The Matrix class
