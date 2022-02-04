---
layout: doc-page
title: v0.1.1 vs v0.1.0
extraJS:
    - scripts/benchmarkGraph.js
---

<canvas id="cstdBenches"></canvas>
<canvas id="cblasBenches"></canvas>

<script src=/scripts/benchmarkGraph.js />

<script>
   comparisonGraph("cstd", "0-1-0", "0-1-1", ["sprintf", "sprintfNonAllocating", "sscanf", "sscanfNonAllocating", "abs", "atol", "div", "getEnv", "qsort", "rand"], "logarithmic", document.getElementById('cstdBenches'));
   comparisonGraph("cblas", "0-1-0", "0-1-1", ["openblasCDotUArrayStyle", "openblasCDotUStructStyle", "slincblasDDot"], "logarithmic", document.getElementById('cblasBenches'));

</script>