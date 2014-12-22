# SciPy in LFE

## Clustering (scipy.cluster)

## Constants (scipy.constants)

## Discrete Fourier transforms (scipy.fftpack)

## Integration and ODEs (scipy.integrate)

## Interpolation (scipy.interpolate)

```cl
> (set x (lsci-np:arange 0 10))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list x)
(0 1 2 3 4 5 6 7 8 9)
> (set y (lsci-np:exp (lsci-math:div (lsci-math:mul -1 x) 3.0)))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list y)
(1.0 0.7165313105737893 0.513417119032592 0.36787944117144233
 0.26359713811572677 0.18887560283756183 0.1353352832366127
 0.09697196786440505 0.06948345122280154 0.049787068367863944)
> (set interp (lsci-sp:interpolate.interp1d x y))
#(#B(105 110 116 101 114 112 49 100) ...)
```

```cl
> (set x-smaller (lsci-np:arange 0 1 0.1))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
> (lsci-np:->list x-smaller)
(0.0 0.1 0.2 0.30000000000000004 0.4 0.5 0.6000000000000001
 0.7000000000000001 0.8 0.9)
> (set y-smaller (lsci-py:func interp `(,x-smaller)))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108  ...))
> (lsci-np:->list y-smaller)
(1.0 0.9716531310573789 0.9433062621147579 0.9149593931721368
 0.8866125242295158 0.8582656552868946 0.8299187863442735
 0.8015719174016525 0.7732250484590314 0.7448781795164103)
```

## Input and output (scipy.io)

## Linear algebra (scipy.linalg)

## Miscellaneous routines (scipy.misc)

## Multi-dimensional image processing (scipy.ndimage)

## Orthogonal distance regression (scipy.odr)

## Optimization and root finding (scipy.optimize)

## Signal processing (scipy.signal)

## Sparse matrices (scipy.sparse)

## Sparse linear algebra (scipy.sparse.linalg)

## Compressed Sparse Graph Routines (scipy.sparse.csgraph)

## Spatial algorithms and data structures (scipy.spatial)

## Special functions (scipy.special)

## Statistical functions (scipy.stats)

## Statistical functions for masked arrays (scipy.stats.mstats)

## C/C++ integration (scipy.weave)
