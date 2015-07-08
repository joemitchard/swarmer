//
//	Lines
//
function setup_grid(arrity,tileSize,gridScale,obArray) {
  arrity = parseInt(arrity);
  tileSize = parseInt(tileSize);
  gridScale = parseInt(gridScale);
     var svg=d3.select("svg")
      .attr("height",arrity*tileSize*gridScale)
      .attr("width",arrity*tileSize*gridScale)
        .append("rect")
        .attr("class","bg")
        .attr("height",arrity*tileSize*gridScale-1)
        .attr("width",arrity*tileSize*gridScale-1)
        .attr("style","fill:url(#grass)");
  var list =[];
  for (var i = 0; i <= arrity; i++) {
    list.push(i*tileSize)
  };
  draw_background(arrity,tileSize,gridScale,obArray);
  draw_hlines(arrity,tileSize,gridScale,list);
  draw_vlines(arrity,tileSize,gridScale,list);
}

function draw_background(arrity,tileSize,gridScale,obArray){
  //console.log(obArray);
   var svg = d3.select("svg")
   .selectAll(".ob")
   .data(obArray)
   .enter().append("rect")
   .attr("class","ob")
   .attr("height",gridScale*5)
   .attr("width",gridScale*5)
   //.attr("y",function(d) { var y=tileSize; return (Math.floor(d[2]/y)*gridScale*5); })
   //.attr("x",function(d) { var x=tileSize; return ((d[1]%x)*gridScale*5); })
    .attr("y",function(d) { return d[2]*gridScale*5; })
    .attr("x",function(d) { return d[1]*gridScale*5; })
    .attr("style","fill:url(#brick)")
    ;
    }

function obColour(b) {
  if (b==="t") {
    return "#000000";}
    return "#FFFFFF";
  };


function draw_hlines(arrity,tileSize,gridScale,list) {
   var svg = d3.select("svg").selectAll("hline")
    .data(list)
    .enter().append("line")
    .attr("class","xline")
    .attr("x1",0)
    .attr("x2",(arrity*tileSize)*gridScale)
    .attr("y1",function(d) { return d*gridScale; })
    .attr("y2",function(d) { return d*gridScale; })
    .attr("stroke","black")
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
    .attr("stroke","black")
    .style("stroke-dasharray","10 5");
};

/////////////
// Circles //
/////////////

function setColour(ob) {
  if (ob.type==="human"){
    return "img/hume.png";
  }else if (ob.type==="food"){
    return "img/food.png";
  };
  return "img/zomb.png";
}
  
function strokeColour(ob) {
  if (ob.type==="human"){
    return "blue";
  }
  return "seagreen";
}

function changeColour(object,d,$scope) {
  var inv;
  if (d.type==="human"){
    inv = "img/inv_hume.png";
  }else{ inv = "img/inv_zomb.png";}
  d3.select(object)
  .attr("xlink:href",inv);
  $scope.inspectList.push(d);
  
}

function update_circles(data,gridScale,swarmSize,$scope) {
    
    var svg = d3.select("svg");
    var circles = svg.selectAll(".ent_image").data(data, function(d) {return d.id});
    
    // change the xy of the selection
    circles.transition()
    .attr("x_vel",function(d) { return d.x_velocity;})
    .attr("y_vel",function(d) {return d.y_velocity;})
    .attr("x", function(d) { return d.x*gridScale; })
    .attr("y", function(d) { return d.y*gridScale; })
    .duration(500);
    
    // add any new elements in .enter
    circles.enter()
        .append("image")
        .on("click", function(d) {changeColour(this,d,$scope);})
        .attr("class", "ent_image")
                .attr("id", function(d) { return d.id; })
                .attr("x",function(d) { return d.x*gridScale; })
        .attr("y", function(d) { return d.y*gridScale; })
        .attr("width",gridScale)
        .attr("height",gridScale)
        .attr("x_vel",function(d) {return d.x_velocity;})
        .attr("y_vel",function(d) {return d.y_velocity;})
        .attr("xlink:href", function(d) {return setColour(d);});
      
      // remove any leftover elements
      circles.exit().remove();
};


function draw_circles(data,gridScale,$scope) {
  var svg = d3.select("svg");
      svg.selectAll(".ent_image")
        .data(data)
        .enter().append("image")
        .on("click", function(d) {changeColour(this,d,$scope);})
        //.attr("class", function(d) {return d.type; })
        .attr("class", "ent_image")
        .attr("id", function(d) { return d.id; })
        //.style("stroke", function(d) { return strokeColour(d);})
        .attr("x",function(d) { return d.x*gridScale; })
        .attr("y", function(d) { return d.y*gridScale; })
        .attr("width",gridScale)
        .attr("height",gridScale)
        .attr("x_vel",function(d) { return d.x_velocity;})
        .attr("y_vel",function(d) {return d.y_velocity;})
        .attr("xlink:href", function(d) {return setColour(d);})
        ;
	};

function update_web(data,gridScale,$scope) {
  //the incoming data contains too many nested structures to easily deal with
  // first flatten it out
  
  //lines to zombies
    var zmodData=[];
    for (var i=0;i<data.length;i++){
        if (data[i] === undefined) {continue};
      if (data[i].z_list.length>0) {
        for (j=0;j<data[i].z_list.length;j++){
          zmodData.push([data[i].id+data[i].z_list[j].id,
          data[i].x,data[i].y,data[i].z_list[j].x,data[i].z_list[j].y]);
          }
      }
    }
    
    //lines to humans
    var hmodData = [];
      for (var i=0;i<data.length;i++){
        if (data[i] === undefined) {continue};
        if (data.length >0 && data[i].h_list.length>0) {
          for (j=0;j<data[i].h_list.length;j++){
            hmodData.push([data[i].id+data[i].h_list[j].id,
            data[i].x,data[i].y,data[i].h_list[j].x,data[i].h_list[j].y]);
          }
        }
    }
  draw_zlines(zmodData,gridScale);
  draw_humlines(hmodData,gridScale);
  }
    

function draw_zlines(data,gridScale){

    var weblines = d3.select("svg").selectAll(".zline"); 
 
    weblines = d3.select("svg").selectAll(".zline").data(data, function(d){return d[0];});
    //weblines.remove();
    weblines.transition()
    .attr("x1",function(d) {return (d[1]*gridScale)+gridScale/2;})
    .attr("x2",function(d,i) {return (d[3]*gridScale)+gridScale/2;})
    .attr("y1",function(d) {return d[2]*gridScale+gridScale/2;})
    .attr("y2",function(d,i) { return d[4]*gridScale+gridScale/2; })
    .duration(500);
    
    weblines.enter().append("line")
    .attr("class","zline")
    .attr("id",function(d){return d[0];})
    .attr("x1",function(d) {return d[1]*gridScale+gridScale/2})
    .attr("x2",function(d) { return d[3]*gridScale+gridScale/2})
    .attr("y1",function(d) {return d[2]*gridScale+gridScale/2})
    .attr("y2",function(d) { return d[4]*gridScale+gridScale/2; })
    .attr("stroke","red")
    .attr("opacity","0.5");
          weblines.exit().remove();
    
};

function draw_humlines(data,gridScale){

    var weblines = d3.select("svg").selectAll(".humline"); 
 
    weblines = d3.select("svg").selectAll(".humline").data(data, function(d){return d[0];});
    //weblines.remove();
   weblines.transition()
    .attr("x1",function(d) {return (d[1]*gridScale)+gridScale/2;})
    .attr("x2",function(d,i) {return (d[3]*gridScale)+gridScale/2;})
    .attr("y1",function(d) {return d[2]*gridScale+gridScale/2;})
    .attr("y2",function(d,i) { return d[4]*gridScale+gridScale/2; })
    .duration(500);
    weblines.enter().append("line")
    .attr("class","humline")
    .attr("id",function(d){return d[0];})
    .attr("x1",function(d) {return d[1]*gridScale+gridScale/2})
    .attr("x2",function(d) { return d[3]*gridScale+gridScale/2})
    .attr("y1",function(d) {return d[2]*gridScale+gridScale/2})
    .attr("y2",function(d) { return d[4]*gridScale+gridScale/2; })
    .attr("stroke","blue")
        .attr("opacity","0.5");
    weblines.exit().remove();
    
};
