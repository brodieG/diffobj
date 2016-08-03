
function post_size() {
  var iw = document.body.clientWidth;
  var w = document.body.clientWidth;
  /*
  var w = window.innerWidth
  || document.documentElement.clientWidth
  || document.body.clientWidth;

  var h = window.innerHeight
  || document.documentElement.clientHeight
  || document.body.clientHeight;
  */

  var t = document.getElementById("measure").scrollWidth;

  document.getElementById("iw").innerHTML = "inner width:" + iw;
  document.getElementById("xsize").innerHTML = "width: " + w;
  document.getElementById("tsize").innerHTML = "text size: " + t;

  var pad = 0;
  if(t + pad > w) {
    var fs = ((w - pad) / t) + "em";
    document.getElementById("ratio").innerHTML = fs;
    document.getElementById("docont").style.fontSize = fs;
    console.log("Font size changed to " + fs);
  } else {
    document.getElementById("ratio").innerHTML = 1;
    document.getElementById("docont").style.fontSize = "1em";
    console.log("Is smaller");
  }
};
window.addEventListener('resize', post_size, true);
post_size();
