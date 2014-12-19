# Python Standard Library

## ``math``

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

Of couse, this is just as usable from Erlang:

```erlang
1> 'lsci-math':pi().
3.141592653589793
2> 'lsci-math':e().
2.718281828459045
3> 'lsci-math':phi(6).
0.9999999990134123
4> 'lsci-math':ceil(5.1).
6
5> 'lsci-math':floor(5.1).
5
6> 'lsci-math':round(5.67).
6
7> 'lsci-math':round(5.6666667, 1).
5.7
8> 'lsci-math':round(5.6666667, 3).
5.667
```

## ``cmath``

Complex numbers are suppored; in fact, there's a whole module dedicated
to them:

```cl
> (lsci-cmath:phase (lsci-cmath:complex -1.0 0.0))
3.141592653589793
> (lsci-cmath:phase (lsci-cmath:complex -1.0 -0.0))
-3.141592653589793
```

The second function below isn't working correctly in Erlang, 'cause I'm not
sure how to make a negative zero float in Erlang (LFE just does the right
thing).

For instance, ``-0.0.`` returns ``0.0`` instead of ``-0.0``. I'll
need the input from an Erlang master to show the working example of that:

```erlang
9> 'lsci-cmath':phase('lsci-cmath':complex(-1.0, 0.0)).
3.141592653589793
10> 'lsci-cmath':phase('lsci-cmath':complex(-1.0, -0.0)).
3.141592653589793
```
