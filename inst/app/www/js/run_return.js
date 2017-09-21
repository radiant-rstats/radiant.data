// based on http://stackoverflow.com/a/32340906/1974918
// and http://stackoverflow.com/a/8774101/1974918
$(document).keydown(function(event) {
  if ($("#combine_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#combine_report").click();
  } else if ($("#explore_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#explore_report").click();
  } else if ($("#pivotr_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#pivotr_report").click();
  } else if ($("#transform_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#transform_report").click();
  } else if ($("#view_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#view_report").click();
  } else if ($("#visualize_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#visualize_report").click();
  }
});

// $(document).keydown(function(event) {
  // if ((event.metaKey || event.ctrlKey) && event.keyCode == 83) {
  // if ((event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    // $("#saveStateNav").click();
    // document.getElementById('saveStateNav').click();
    // $("#saveStateNav").trigger("click");
  // }
// });