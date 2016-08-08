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
function resize_diff_out(scale) {
  var w = document.body.clientWidth;
  var meta = document.getElementById("diffobj_size_meta");
  var content = document.getElementById("diffobj_content");
  if(meta == null || content == null)
    throw new Error("Unable to find meta and content; contact maintainer.");
  var row = meta.getElementsByClassName("row");

  if(row.length != 1)
    throw new Error("Unexpected row struct in meta block; contact maintainer.");

  var lines = meta.getElementsByClassName("line");

  if(lines.length != 1 && lines.length != 2)
    throw new Error("Unexpected lines in meta block; contact maintainer.");

  meta.style.display = "block";

  var t = 0;
  var pad = 1;  // looks like scrollWidth returns floats truncated to ints

  for(i = 0; i < lines.length; i++) t = t + lines[i].scrollWidth + pad;

  meta.style.display = "none";
  content.style.width = t + "px";  // prevent wrapping outside of native width

  if(scale) {
    var scale = w / t;

    if(scale < 1) {
      content.style.width = t + "px";
      content.style.transform = "scale(" + scale + ")";
      content.style.transformOrigin = "top left";
      content.style.webkitTransform = "scale(" + scale + ")";
      content.style.webkitTransformOrigin = "top left";
      content.style.msTransform = "scale(" + scale + ")";
      content.style.msTransformOrigin = "top left";
      // console.log("Scaled to: " + scale);
    } else {
      content.style.width = "auto";
      content.style.transform = "none";
      content.style.webkitTransform = "none";
      content.style.msTransform = "none";
      // console.log("Unscaled");
    }
  }
};
function resize_diff_out_scale() {resize_diff_out(true);}
function resize_diff_out_no_scale() {resize_diff_out(false);}

