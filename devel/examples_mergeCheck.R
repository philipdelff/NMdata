
Say we want to add a covariate from a
`dt.cov`. Imagine we just assume that all subjects are represented with one row
each in `dt.cov`, and so we expect the number of rows to be unchanged from `pk`. But we see that only when remembering `all.x=T`, we get the dimensions we had expected (we do not checked that we get the _right_ rows, only that we get the _right number_ of rows):


```{r}
dim(pk)
## the number of unique values of ID in pk
pk[,uniqueN(ID)]
pk[,range(ID)]
## dt.cov has a covariate for some of the subjects
dt.cov
pk2 <- merge(pk,dt.cov,by="ID")
dim(pk2)
## now as expected
pk3 <- merge(pk,dt.cov,by="ID",all.x=TRUE)
dim(pk3)
```

So when creating `pk2` we lost a lot of rows because we forgot the
`all.x` option. And also as we shall now see that checking the
resulting dimensions is not enough. In the following example, `pk4` has the same number of rows as `pk`, but one row is repeated, another must have disappeared.

```{r}
pk4 <- merge(pk,dt.cov2,by="ID")
dim(pk4)
## we now have twice as many rows for this subject
pk[ID==31,.N]
pk4[ID==31,.N]
```


As illustrated by these simple examples, even the simplest merges have
to be rigorously checked, no exceptions. The function call `mergeCheck(x1,x2)`
ensures that the result has exactly one of each rows from `x1` and
nothing else. We can use `...` to pass additional arguments to merge. Here, we need `all.x` to include rows from `x1` where `ID` is not matched by a row in `dt.cov`.

```{r}
pk2 <- try(mergeCheck(pk,dt.cov,by="ID"))
## now as expected
pk3 <- mergeCheck(pk,dt.cov,by="ID",all.x=TRUE)
dim(pk3)
```
