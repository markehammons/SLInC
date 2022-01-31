
function dataFunction(array, labels) {
   return function (data) {
      for (var i = 0; i < data.length; i++) {
         const name = data[i].benchmark.split('.');
         if (labels.includes(name[name.length - 1])) {
            array.push(data[i].primaryMetric.score);
         }
      }
   };
};


function loadBenchData(array, labels, benchName, version) {
   $.ajax({
      type: "GET",
      url: "/resources/" + benchName + "-bench-v" + version + ".json",
      dataType: "json",
      success: dataFunction(array, labels),
      async: false
   });
}

function comparisonGraph(benchName, version1, version2, labels, scaleType, canvas) {
   var v1Data = [];
   var v2Data = [];


   loadBenchData(v1Data, labels, benchName, version1);
   loadBenchData(v2Data, labels, benchName, version2);

   var version1Perf = [];

   for (var i = 0; i < v1Data.length; i++) {
      version1Perf[i] = 1;
   };

   var version2Perf = [];

   for (var i = 0; i < v1Data.length; i++) {
      version2Perf[i] = v2Data[i] / v1Data[i]
   };

   const config = {
      type: "bar",
      data: {
         labels: labels,
         datasets: [{
            label: "v" + version1,
            data: version1Perf,
            backgroundColor: "blue"
         }, {
            label: "v" + version2,
            data: version2Perf,
            backgroundColor: "green"
         }]
      },
      options: {
         scales: {
            myScale: {
               type: scaleType,
               position: 'left',
               title: {
                  display: true,
                  text: "improvement"
               },
               ticks: {
                  callback: function (value, index, ticks) {
                     return value + 'x';
                  }
               }
            }
         }
      }
   };

   const myChart = new Chart(
      canvas,
      config
   );
};

