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
 * 
 * NOTE: this code is intended to be loaded after the HTML has been rendered
 */

var meta = document.getElementById("diffobj_meta");
var meta_cont = document.getElementById("diffobj_content_meta");
var meta_banner = document.getElementById("diffobj_banner_meta");
var content = document.getElementById("diffobj_content");

if(meta == null || content == null)
  throw new Error("Unable to find meta and content; contact maintainer.");

var row = meta_cont.getElementsByClassName("row");

if(row.length != 1)
  throw new Error("Unexpected row struct in meta block; contact maintainer.");

var lines = meta_cont.getElementsByClassName("line");

if(lines.length != 1 && lines.length != 2)
  throw new Error("Unexpected lines in meta block; contact maintainer.");

var meta_bnr_gutter =
  document.querySelector("#diffobj_banner_meta .line .gutter");
var meta_bnr_delete =
  document.querySelector("#diffobj_banner_meta .line .text>.delete");
var meta_bnr_text =
  document.querySelector("#diffobj_banner_meta .line .text");

var bnr_gutters =
  document.querySelectorAll("#diffobj_content .line.banner .gutter");
var bnr_text_div =
  document.querySelectorAll("#diffobj_content .line.banner .text>DIV");

if(
    meta_bnr_gutter == null || meta_bnr_delete == null ||
    bnr_gutters.length != 2 || bnr_text_div.length != 2
  )
  throw new Error("Unable to get meta banner objects")

function resize_diff_out(scale) {

  // - Get object refs ---------------------------------------------------------

  var w = document.body.clientWidth;
  // - Get Sizes ---------------------------------------------------------------

  meta.style.display = "block";

  // Get target content widths

  var t = 0;
  var pad = 1;  // looks like scrollWidth returns floats truncated to ints

  for(i = 0; i < lines.length; i++) t = t + lines[i].scrollWidth + pad;

  // The getComputedStyle business won't work on IE9 or lower; need to detect
  // and implement work-around

  var b_t, b_d_w, b_d_o, b_g;
  b_g = parseFloat(window.getComputedStyle(meta_bnr_gutter).width);
  b_d_o = meta_bnr_delete.offsetWidth;
  b_d_w = parseFloat(window.getComputedStyle(meta_bnr_delete).width);
  b_t = parseFloat(window.getComputedStyle(meta_bnr_text).width);

  meta.style.display = "none";

  // - Set Sizes ---------------------------------------------------------------

  content.style.width = t + "px";  // prevent wrapping outside of native width

  for(i = 0; i < 2; i++) {
    bnr_gutters[i].style.width = b_g + "px";
    // for some reason table fixed width computation doesn't properly account
    // for padding and lines
    bnr_text_div[i].style.width = b_t - b_d_o + b_d_w + "px";
  }
  var scale_size = w / t;

  if(scale_size < 1) {
    content.style.width = t + "px";
    if(scale) {
      content.style.transform = "scale(" + scale_size + ")";
      content.style.transformOrigin = "top left";
      content.style.webkitTransform = "scale(" + scale_size + ")";
      content.style.webkitTransformOrigin = "top left";
      content.style.msTransform = "scale(" + scale_size + ")";
      content.style.msTransformOrigin = "top left";
    }
  } else {
    content.style.width = "auto";
    content.style.transform = "none";
    content.style.webkitTransform = "none";
    content.style.msTransform = "none";
  }
};
/*
 * Manage resize timeout based on how large the object is
 */

var out_rows = content.getElementsByClassName("row").length;
var timeout_time;
if(out_rows < 100) {
  timeout_time = 50;
} else if(out_rows < 500) {
  timeout_time = 250;
} else timeout_time = out_rows / 2;

var timeout;
function resize_window(f, scale) {
  clearTimeout(timeout);
  timeout = setTimeout(f, timeout_time, scale);
}

function resize_diff_out_scale() {resize_window(resize_diff_out, true);}
function resize_diff_out_no_scale() {resize_window(resize_diff_out, false);}
