# Changepoints

Is automatic detection of changepoints in timeseries good enough to focus human
attention when reviewing large government reports that are released frequently?

![changepoints on a growing timeseries](https://github.com/nacnudus/changepoints/blob/master/animation.gif)

This uses London Underground [performance data
almanac](https://tfl.gov.uk/corporate/publications-and-reports/underground-services-performance),
which contains dozens of performance metrics with monthly resolution over more
than ten years.  It's available in tidy form in the R package
[underground](https://github.com/nacnudus/underground).

Background changepoints blog
[post](https://www.r-bloggers.com/a-simple-intro-to-bayesian-change-point-analysis/)
with links to other materials

Method:

1. Apply various R changepoint detection packages
   ([changepoint](https://github.com/rkillick/changepoint/blob/master/NEWS),
   [ecp](https://cran.r-project.org/package=ecp),
   [bcp](https://cran.r-project.org/package=bcp) (Bayesian), Twitter's
   [BreakoutDetection](https://github.com/twitter/BreakoutDetection)) just to
   see what happens.
2. Evaluate the changepoints on shorter subsets of the series to see how stable
   they are from month to month.
3. Build a simple Twitter bot to tweet newly-detected changepoints as new data
   becomes available (proof of concept only, don't over-engineer this).

# `changepoint` package

A related Arxiv [paper](https://arxiv.org/pdf/1101.1438.pdf).

> As increasingly longer data sets are being collected, more and more
> applications require the detection of changes in the distributional properties
> of such data.

> ...

> In particular we focus on applications where we expect the number of
> changepoints to increase as we collect more data.

> ...

> At the time of writing Binary Segmentation proposed by Scott and Knott (1974)
> is arguably the most widely used changepoint search method. It is approximate
> in nature with an O(n log n) computational cost, where n is the number of data
> points.  While exact search algorithms exist for the most common forms of
> changepoint models, these have a much greater computational cost.

> ...

> We present a new approach to search for changepoints, which is exact and under
> mild conditions has a computational cost that is linear in the number of data
> points: the Pruned Exact Linear Time (PELT) method.

A [demo](https://rpubs.com/richkt/269908) on Rpubs.
