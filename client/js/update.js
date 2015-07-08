function update_circles(data,gridScale,swarmSize) {
 // console.log(data);
  //var anim_time = Math.abs(0.3*swarmSize.value);
    var svg = d3.select("svg");
    svg.selectAll("circle")
    .data(data, function(d) { return d.id; })
    .transition()
    .attr("cx", function(d) { return d.x*gridScale; })
    .attr("cy", function(d) { return d.y*gridScale; })
    .duration(300);
   
   svg.selectAll("circle")
      .each(function(d,i) {
        //Find corresponding pid in data list
        for (var j=0;j<data.length;j++) {
          if (data[j].id === d.id){
            break;
          } else {
            if(j == (data.length -1)) {
              d3.select(this).remove()
            }
          }
        }
      })
}
