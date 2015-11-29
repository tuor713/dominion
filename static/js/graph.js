function barChart(selector,width,height,data) {
  var height = height, barWidth = width / data.length;

  var x = d3.scale.ordinal()
    .domain(data.map(function (d) { return d.name; }))
    .rangeRoundBands([0,width],.1);

  var y = d3.scale.linear()
    .domain([0, d3.max(data, function (d) { return d.value; })])
    .range([height,0]);

  var xAxis = d3.svg.axis().scale(x).orient("bottom");
  var yAxis = d3.svg.axis().scale(y).orient("left");

  var chart = d3.select(selector)
    .attr("width", width + 60)
    .attr("height", height + 60)
    .append("g")
    .attr("transform", "translate(30,30)");

  chart.append("g")
    .attr("class","x axis")
    .attr("transform","translate(0,"+height + ")")
    .call(xAxis);

  chart.append("g")
    .attr("class","y axis")
    .call(yAxis);

  var bar = chart.selectAll(".bar")
    .data(data)
    .enter().append("rect")
    .attr("class","bar")
    .attr("x", function (d) { return x(d.name); })
    .attr("y", function (d) { return y(d.value); })
    .attr("height", function (d) { return height - y(d.value); })
    .attr("width", x.rangeBand());
}

function percentageData(data) {
  var total = data.reduce(function (accu,d) { return accu + d.value; }, 0);
  return data.map(function (d) { return {name:d.name, value:100*d.value/total}; });
}
