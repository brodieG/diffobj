// diffobj - Compare R Objects with a Diff
// Copyright (C) 2016  Brodie Gaslam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Go to <https://www.r-project.org/Licenses/GPL-3> for a copy of the license.

/*
 * Resizes diff by changing font-size using a hidden row of sample output as
 * a reference
 */
function resize_diff_out() {
  var w = document.body.clientWidth;
  var meta = document.getElementById("diffobj_size_meta");
  var content = document.getElementById("diffobj_content");
  var lines = meta.getElementsByClassName("line");

  if(lines.length != 1 && lines.length != 2)
    throw new Error("Unexpected lines in meta block; contact maintainer.")

  // meta.style.display = "block";

  var t = 0;
  for(i = 0; i < lines.length; i++) t = t + lines[i].scrollWidth;

  // meta.style.display = "none";
  console.log(w);
  console.log(t);
  if(!w || !t) throw new Error("Unable to get dimensions for resizing.")

  var pad = 5;
  if(t + pad > w) {
    var fs = ((w - pad) / t) + "em";
    content.style.fontSize = fs;
    console.log("Font size changed to " + fs);
  } else {
    content.style.fontSize = "1em";
    console.log("Is smaller");
  }
};
window.addEventListener('resize', resize_diff_out, true);
