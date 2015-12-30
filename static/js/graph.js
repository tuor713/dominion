var dataSetColors = ["steelblue", "red", "green", "orange"];

function axes(chart,height,x,y) {
  var xAxis = d3.svg.axis().scale(x).orient("bottom");
  var yAxis = d3.svg.axis().scale(y).orient("left");

  chart.append("g")
    .attr("class","x axis")
    .attr("transform","translate(0,"+height + ")")
    .call(xAxis);

  chart.append("g")
    .attr("class","y axis")
    .call(yAxis);
}

function legend(chart, labels, height) {
  var leg = chart.selectAll(".legend")
      .data(d3.range(0,labels.length))
      .enter().append("g")
      .attr("class", "legend")
      .attr("transform", function(d) { return "translate("+ (d*150) + ","+(height+30)+")"; });

  // draw legend colored rectangles
  leg.append("rect")
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", function (d) { return dataSetColors[d]; });

  // draw legend text
  leg.append("text")
      .attr("x", 20)
      .attr("y", 9)
      .attr("dy", ".35em")
      .text(function(d) { return labels[d];});
}

function mkChart(selector,width,height) {
  return d3.select(selector)
    .attr("width", width + 60)
    .attr("height", height + 90)
    .append("g")
    .attr("transform", "translate(30,30)");
}

function barChart(selector,width,height,data) {
  var height = height, barWidth = width / data.length;

  var x = d3.scale.ordinal()
    .domain(data.map(function (d) { return d.name; }))
    .rangeRoundBands([0,width],.1);

  var y = d3.scale.linear()
    .domain([0, d3.max(data, function (d) { return d.value; })])
    .range([height,0]);

  var chart = mkChart(selector,width,height);
  axes(chart,height,x,y);

  var bar = chart.selectAll(".bar")
    .data(data)
    .enter().append("rect")
    .attr("class","bar")
    .attr("x", function (d) { return x(d.name); })
    .attr("y", function (d) { return y(d.value); })
    .attr("height", function (d) { return height - y(d.value); })
    .attr("width", x.rangeBand());
}

function linePlot(selector, width, height) {
  var labels = [];
  var args = [];
  for (var i = 3; i < arguments.length; i+=2) {
    labels.push(arguments[i])
    args.push(arguments[i+1]);
  }

  var maxdata = args[0];
  for (var i=0; i<args.length; i++) {
    if (args[i].length > maxdata.length) {
      maxdata = args[i];
    }
  }

  var x = d3.scale.ordinal()
    .domain(maxdata.map(function (d) { return d.name; }))
    .rangeRoundBands([0,width],.1);

  var y = d3.scale.linear()
    .domain([0, d3.max(args, function (d) { return d3.max(d, function (di) { return di.value; }); })])
    .range([height,0]);

  var chart = mkChart(selector,width,height);
  axes(chart,height,x,y);

  for (var i=0; i<args.length; i++) {
    data = args[i];

    var lineGen = d3.svg.line()
      .x(function (d) { return x(d.name) + x.rangeBand()/2; })
      .y(function (d) { return y(d.value); });

    chart.append("svg:path")
      .attr("d", lineGen(data))
      .attr("stroke", dataSetColors[i])
      .attr("stroke-width",2)
      .attr("fill","none");
  }

  legend(chart, labels, height);
}

function scatterPlot(selector, width, height) {
  var labels = [];
  var args = [];
  for (var i = 3; i < arguments.length; i+=2) {
    labels.push(arguments[i])
    args.push(arguments[i+1]);
  }

  var maxdata = args[0];
  for (var i=0; i<args.length; i++) {
    if (args[i].length > maxdata.length) {
      maxdata = args[i];
    }
  }

  var x = d3.scale.ordinal()
    .domain(maxdata.map(function (d) { return d.name; }))
    .rangeRoundBands([0,width],.1);

  var y = d3.scale.linear()
    .domain([0, d3.max(args, function (d) { return d3.max(d, function (di) { return di.value; }); })])
    .range([height,0]);

  var chart = mkChart(selector,width,height);
  axes(chart,height,x,y);

  for (var i=0; i<args.length; i++) {
    data = args[i];

    var circle = chart.selectAll("circle"+i)
      .data(data)
      .enter().append("circle")
      .attr("cx", function (d) { return x(d.name) + x.rangeBand()/2; })
      .attr("cy", function (d) { return y(d.value); })
      .attr("fill",dataSetColors[i])
      .attr("r", 3);
  }

  legend(chart, labels, height);
}

function percentageData(data) {
  var total = data.reduce(function (accu,d) { return accu + d.value; }, 0);
  return data.map(function (d) { return {name:d.name, value:100*d.value/total}; });
}
