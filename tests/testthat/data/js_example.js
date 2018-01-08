$(document).keydown(function(event) {
  if ($("#rmd_knit").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#rmd_knit").click();
  }
});
