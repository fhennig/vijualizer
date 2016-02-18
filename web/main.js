function createLineElement(x, y, length, angle) {
    var line = document.createElement("div");
    var styles = 'border: 1px solid black; '
               + 'width: ' + length + 'px; '
               + 'height: 0px; '
               + '-moz-transform: rotate(' + angle + 'rad); '
               + '-webkit-transform: rotate(' + angle + 'rad); '
               + '-o-transform: rotate(' + angle + 'rad); '  
               + '-ms-transform: rotate(' + angle + 'rad); '  
               + 'position: absolute; '
               + 'top: ' + y + 'px; '
               + 'left: ' + x + 'px; ';
    line.setAttribute('style', styles);  
    return line;
}

function createLine(x1, y1, x2, y2) {
    var a = x1 - x2,
        b = y1 - y2,
        c = Math.sqrt(a * a + b * b);

    var sx = (x1 + x2) / 2,
        sy = (y1 + y2) / 2;

    var x = sx - c / 2,
        y = sy;

    var alpha = Math.PI - Math.atan2(-b, a);

    return createLineElement(x, y, c, alpha);
}

$(function() {

   $(".flex-container").mousewheel(function(event, delta) {

      this.scrollLeft -= (delta * 30);
    
      event.preventDefault();

   });

});


function populateComponentSelector() {
  for (var i = 0; i < 10; i++) {
    var div = document.createElement("div");
    var a = document.createElement("a");
    var img = document.createElement("img");
    div.appendChild(a);
    a.appendChild(img);
    div.setAttribute("class", "flex-item");
    img.setAttribute("src", "temp_component.png");
    document.getElementById("filter-container").appendChild(div);
  }
    for (var i = 0; i < 7; i++) {
    var div = document.createElement("div");
    var a = document.createElement("a");
    var img = document.createElement("img");
    div.appendChild(a);
    a.appendChild(img);
    div.setAttribute("class", "flex-item");
    img.setAttribute("src", "temp_component.png");
    document.getElementById("gates-container").appendChild(div);
  }
    for (var i = 0; i < 3; i++) {
    var div = document.createElement("div");
    var a = document.createElement("a");
    var img = document.createElement("img");
    div.appendChild(a);
    a.appendChild(img);
    div.setAttribute("class", "flex-item");
    img.setAttribute("src", "temp_component.png");
    document.getElementById("visualizer-container").appendChild(div);
  }
    for (var i = 0; i < 5; i++) {
    var div = document.createElement("div");
    var a = document.createElement("a");
    var img = document.createElement("img");
    div.appendChild(a);
    a.appendChild(img);
    div.setAttribute("class", "flex-item");
    img.setAttribute("src", "temp_component.png");
    document.getElementById("other-container").appendChild(div);
  }
}

populateComponentSelector();