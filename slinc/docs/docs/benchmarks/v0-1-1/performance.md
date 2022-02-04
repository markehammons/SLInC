---
layout: benchmark-page
title: v0.1.1 Performance
benchmarks:
   - sprintf,cstd,0-1-1,ops/µs,scalaFormatString sprintf sprintfNonAllocating,linear
   - sscanf,cstd,0-1-1,ops/µs,scalaParseString sscanf sscanfNonAllocating,linear
   - abs,cstd,0-1-1,ops/µs,abs scalaAbs,linear
   - atol,cstd,0-1-1,ops/µs,atol scalaAtol,linear
   - div,cstd,0-1-1,ops/µs,div scalaDiv,linear
   - getEnv,cstd,0-1-1,ops/µs,getEnv scalaGetEnv,linear
   - qsort,cstd,0-1-1,ops/ms,qsort scalaSort,linear
   - rand,cstd,0-1-1,ops/µs,rand scalaRand,linear
   - cdotu,cblas,0-1-1,ops/µs,jblasCDotU openblasCDotUArrayStyle openblasCDotUStructStyle,linear
   - ddot,cblas,0-1-1,ops/µs,jblasDDot slincblasDDot,linear
---

The following benches compare Slinc to plain scala and jblas. Unlike the v0.1.0 benches, these graphs are linear, as performance has improved enough that in most cases the graphs are readable without logarithmic scale.

[CStd benchmark data](/resources/cstd-bench-v0-1-1.json)

[Cblas benchmark data](/resources/cblas-bench-v0-1-1.json)
