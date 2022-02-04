---
layout: benchmark-page
title: v0.1.0 Performance
benchmarks:
   - sprintf,cstd,0-1-0,ops/µs,scalaFormatString sprintf sprintfNonAllocating,logarithmic
   - sscanf,cstd,0-1-0,ops/µs,scalaParseString sscanf sscanfNonAllocating,logarithmic
   - abs,cstd,0-1-0,ops/µs,abs scalaAbs,logarithmic
   - atol,cstd,0-1-0,ops/µs,atol scalaAtol,logarithmic
   - div,cstd,0-1-0,ops/µs,div scalaDiv,logarithmic
   - getEnv,cstd,0-1-0,ops/µs,getEnv scalaGetEnv,logarithmic
   - qsort,cstd,0-1-0,ops/ms,qsort scalaSort,logarithmic
   - rand,cstd,0-1-0,ops/µs,rand scalaRand,logarithmic
   - cdotu,cblas,0-1-0,ops/µs,jblasCDotU openblasCDotUArrayStyle openblasCDotUStructStyle,logarithmic
   - ddot,cblas,0-1-0,ops/µs,jblasDDot slincblasDDot,logarithmic
---

The following benches compare Slinc to plain scala and jblas. Please note these benches are logarithmic scale because v0.1.0 is that much slower in most situations!!

[CStd benchmark data](/resources/cstd-bench-v0-1-0.json)

[Cblas benchmark data](/resources/cblas-bench-v0-1-0.json)