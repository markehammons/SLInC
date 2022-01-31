---
layout: doc-page
title: v0.1.1 vs v0.1.0
extraJS:
    - scripts/benchmarkGraph.js
---

<canvas id="myBenchmarks"></canvas>

<script src=/scripts/benchmarkGraph.js />

<script>
   comparisonGraph("cstd", "0-1-0", "0-1-1", ["abs", "atol", "getEnv", "qsort", "rand"], "logarithmic", document.getElementById('myBenchmarks'));
   // var ls = ["abs", "atol", "getEnv", "qsort", "rand"];

   // const formatGroup = ["abs", "atol", "getEnv", "qsort", "rand"];

   // var cstd_0_1_0 = [];
   // var cstd_0_1_1 = [];

   // var dataFunction = function(array) { return function(data) {

   //    for(var i = 0; i < data.length; i++) {
   //       const name = data[i].benchmark.split('.');
   //       if(formatGroup.includes(name[name.length-1])) {
   //          console.log(name[name.length-1]);
   //          array.push(data[i].primaryMetric.score);
   //       }
   //    }
   // };};
   // $.ajax({
   //    type: "GET",
   //    url: "/resources/cstd-bench-v0-1-0.json",
   //    dataType: "json",
   //    success: dataFunction(cstd_0_1_0),
   //    async: false
   // });

   // $.ajax({
   //    type: "GET",
   //    url: "/resources/cstd-bench-v0-1-1.json",
   //    dataType: "json",
   //    success: dataFunction(cstd_0_1_1),
   //    async: false
   // });

   // var cstd_0_1_0_perf = [];

   // for(var i = 0; i < cstd_0_1_0.length;  i++) {
   //    cstd_0_1_0_perf[i] = 1;
   // }

   // var cstd_0_1_1_perf = [];

   // for(var i = 0; i < cstd_0_1_0.length; i++) {
   //    console.log(cstd_0_1_1[i]);
   //    console.log(cstd_0_1_0[i]);
   //    console.log(cstd_0_1_1[i]/cstd_0_1_0[i]);
   //    cstd_0_1_1_perf[i] = cstd_0_1_1[i] / cstd_0_1_0[i];
   // }


   // console.log(cstd_0_1_0);
   // const config = {
   //    type: "bar",
   //    data: {
   //       labels: ls,
   //       datasets: [{
   //          label: 'v0.1.0',
   //          data: cstd_0_1_0_perf,
   //          backgroundColor: "blue"
   //       }, 
   //       {
   //          label: 'v0.1.1',
   //          data: cstd_0_1_1_perf,
   //          backgroundColor: "red"
   //       }
   //       ]
   //    },
   //    options: {
   //       scales: {
   //          myScale: {
   //             type: 'logarithmic',
   //             position: 'left',
   //             title: {
   //                display: true,
   //                text: "ops/microsecond"
   //             }
   //          }
   //       }
   //    }
   // };

   // const myChart = new Chart(
   //    document.getElementById('myBenchmarks'),
   //    config
   // );
</script>