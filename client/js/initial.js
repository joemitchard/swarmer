function start() {
  var startjson = JSON.stringify({"type":"startzombies"});
  socket.send(startjson);
  socket.onmessage = function (x) {;}
  inspector.value ="";
  var gridScale = document.getElementById('gridScale').value;
  var dummy_json = JSON.stringify({"type":"report"});
  
  //This sends the "update" message to the socket every 1000ms
  // and updates the circles with the recived data
  setInterval(function() {doUpdate()},300);
  function doUpdate() {
    socket.send(dummy_json);
    socket.onmessage = function(evt) {
    var json = JSON.parse(evt.data);
    update_circles(json,gridScale,swarmSize);
    for (var i = 0; i<inspectList.length; i++) {
      var element = inspectList[i];
      inspector.value = "id: "+element.__data__.id+", Pos: "+element.__data__.x+","+element.__data__.y+","+element.__data__.viewer+"\n" + inspector.value;
    }
    };
  };
};


function setup_grid(arrity,tileSize,gridScale) {
  arrity = parseInt(arrity);
  tileSize = parseInt(tileSize);
  gridScale = parseInt(gridScale);
  var list =[];
  for (var i = 0; i <= arrity; i++) {
    list.push(i*tileSize);
  };
  draw_hlines(arrity,tileSize,gridScale,list);
  draw_vlines(arrity,tileSize,gridScale,list);
}


function draw_hlines(arrity,tileSize,gridScale,list) {
   var svg = d3.select("svg")
    .attr("height",(arrity*tileSize)*gridScale)
    .attr("width",(arrity*tileSize)*gridScale)
    .selectAll("hline")
    .data(list)
    .enter().append("line")
    .attr("class","xline")
    .attr("x1",0)
    .attr("x2",(arrity*tileSize)*gridScale)
    .attr("y1",function(d) { return d*gridScale; })
    .attr("y2",function(d) { return d*gridScale; })
    .attr("stroke","cadetblue")
    .style("stroke-dasharray","10 5");
}


function draw_vlines(arrity,tileSize,gridScale,list) {
    var svg = d3.select("svg")
    .selectAll(".vline")
    .data(list)
    .enter().append("line")
    .attr("class","yline")
    .attr("y1",0)
    .attr("y2",(arrity*tileSize)*gridScale)
    .attr("x1",function(d) { return d*gridScale; })
    .attr("x2",function(d) { return d*gridScale; })
    .attr("stroke","cadetblue")
    .style("stroke-dasharray","10 5");
};


function updateArrity(arrity) {
   ; //dummy
};


function draw_circles(data,gridScale) {
  var svg = d3.select("svg");
      svg.selectAll("circle")
        .data(data)
        .enter().append("circle")
        .on("click", function() {changeColour(this);})
        .attr("class", function(d) {return d.type; })
        .attr("r", gridScale)
        .attr("cx", function(d) { return d.x*gridScale; })
        .attr("cy", function(d) { return d.y*gridScale; })
        .style("fill",function(d) { return setColour(d);})
        .style("stroke", function(d) { return strokeColour(d);})
        .style("stroke-width",0.5*gridScale);
        
      //svg.selectAll(".zombie")
        //.style("fill", "darkseagreen" )
        //.style("stroke","seagreen");
      //svg.selectAll(".human")
        //.style("fill", "steelblue" )
        //.style("stroke","darkblue");
}
function setColour(ob) {
  if (ob.type==="human"){
    return "steelblue";
  }
  return "darkseagreen";
}
  
function strokeColour(ob) {
  if (ob.type==="human"){
    return "blue";
  }
  return "seagreen";
}

function changeColour(object) {
  d3.select(object)
  .style("fill","tomato")
  .style("stroke","indianred");
  console.log(object);
  inspectList.push(object);
  
}
  
