// based on http://stackoverflow.com/a/32340906/1974918
// and http://stackoverflow.com/a/8774101/1974918
// check out https://www.youtube.com/watch?v=tM0q3u220mI for debugging
// https://stackoverflow.com/questions/35831811/register-repeated-keyboard-presses-in-shiny
// https://github.com/rstudio/shiny/issues/928
// https://stackoverflow.com/questions/32002170/r-shiny-enabling-keyboard-shortcuts
// https://stackoverflow.com/questions/47569992/home-button-in-header-in-r-shiny-dashboard
$(document).keydown(function(event) {

  if ($(".btn-success:visible" || ".shiny-bound-input:visible").is(":visible") &&
       (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
     $(".btn-success:visible" || ".shiny-bound-input:visible").click();
  } else if ($(".fa-edit:visible" || ".shiny-bound-input:visible").is(":visible") &&
       event.altKey && event.keyCode == 13) {
     $(".fa-edit:visible" || ".shiny-bound-input:visible").click();
  } else if ($("#rmd_read_files").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 79) {
    $("#rmd_read_files").click();
    event.preventDefault();
  } else if ($("#r_read_files").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 79) {
    $("#r_read_files").click();
    event.preventDefault();
  } else if ($("#rmd_save").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 83) {
    // different because rmd_save is a link see https://stackoverflow.com/a/3738603/1974918
    document.getElementById("rmd_save").click();
    event.preventDefault();
  } else if ($("#r_save").is(":visible") && (event.metaKey || event.ctrlKey) && event.shiftKey === false && event.keyCode == 83) {
    // different because r_save is a link see https://stackoverflow.com/a/3738603/1974918
    document.getElementById("r_save").click();
    event.preventDefault();
  } else if ((event.metaKey || event.ctrlKey) && event.shiftKey && event.keyCode == 83) {
    document.getElementById("state_save").click();
    event.preventDefault();
  } else if ((event.metaKey || event.ctrlKey) && event.shiftKey && event.keyCode == 79) {
    // not working yet
    document.getElementById("state_load").click();
    event.preventDefault();
  }

  // focusing in text (area) inputs
  if ($("#data_rename").is(":focus") && event.keyCode == 13) {
    $("#renameButton").click();
  } else if ($("#view_dat").is(":focus") && event.keyCode == 13) {
    $("#view_store").click();
  } else if ($("#pvt_dat").is(":focus") && event.keyCode == 13) {
    $("#pvt_store").click();
  } else if ($("#expl_dat").is(":focus") && event.keyCode == 13) {
    $("#expl_store").click();
  } else if ($("#tr_dataset").is(":focus") && event.keyCode == 13) {
    $("#tr_store").click();
  } else if ($("#cmb_name").is(":focus") && event.keyCode == 13) {
    $("#cmb_store").click();
  } else if ($("#man_rename_data").is(":focus") &&
    document.getElementById('man_rename_data').checked === true) {
    $("#data_rename").focus();
  } else if ($("#man_add_descr").is(":focus") &&
    document.getElementById('man_add_descr').checked === true) {
    $("#man_data_descr").focus();
  } else if ($("#show_filter").is(":focus") && $("#show_filter")[0].checked) {
    $("#data_filter").focus();
  } else if ($("#tr_change_type").next(".selectize-control").find(".focus").length > 0) {
    // can set focus for selectize input
    // https://stackoverflow.com/questions/48104027/determine-if-selectize-input-has-focus
    if ($('#tr_change_type').selectize()[0].selectize.getValue() === "recode") {
      $("#tr_recode").focus();
    } else if ($('#tr_change_type').selectize()[0].selectize.getValue() === "clip") {
      $("#tr_paste").focus();
    } else if ($('#tr_change_type').selectize()[0].selectize.getValue() === "create") {
      $("#tr_create").focus();

    }
  }
});

// } else if ($("#show_filter").is(":focus") &&
// document.getElementById('show_filter').checked === true) {
// not sure if either option is 'better'

// needs 1 or more tr_vars selected first
// } else if ($('#tr_change_type').selectize()[0].selectize.getValue() === "rename") {
// $("#tr_rename").focus();
// needs 1 or more tr_vars selected first
// } else if ($('#tr_change_type').selectize()[0].selectize.getValue() === "replace") {
// $("#tr_replace").focus();

// first character type is not shown as input grabs full focus
// $("#tr_create").val("");
// $("#tr_create").trigger({type: 'keypress', which: 13, keyCode: 13});
// var press = jQuery.Event("keypress");
// press.ctrlKey = false;
// press.which = 40;
// $("tr_create").trigger(press);

// } else if ($("#rmd_knit").is(":visible")) {
// // still may not show newly generated code
//   editor__rmdedit.focus()
// } else if ($("#r_knit").is(":visible")) {
//   editor__redit.focus()
// }

// change class not working yet
// $('#pvt_run').removeClass('btn-sucsses').addClass('btn-warning');
// $('#viz_run').removeClass('btn-sucsses').addClass('btn-warning');
// $('#viz_run').text()
// $('#view_store').text()

// $(document).on("shiny:connected", function(e) {
// $(document).ready(function() {
  // if ($("#rmd_edit").is(":visible")) {
    // var line_nr = editor__rmdreport.getSelectionRange().start.row;
    // var line_content = editor__rmdreport.session.getLine(line_nr);
    // Shiny.onInputChange("rmd_current_line", line_nr);
    // Shiny.onInputChange("rmd_current_line_content", line_content);
  // }
// });

// currline = editor__rmdreport.getSelectionRange().start.row;
// wholelinetxt = editor__rmdreport.session.getLine(currline);

// from https://stackoverflow.com/a/33251536/1974918 by Dean Attali
// $(document).on("shiny:connected", function(e) {
//   var jsWidth = screen.width;
//   Shiny.onInputChange("get_screen_width", jsWidth);
// });

// currline = editor__rmdreport.getSelectionRange().start.row;
// wholelinetxt = editor__rmdreport.session.getLine(currline);

// $(document).keydown(function(event) {
  // if ((event.metaKey || event.ctrlKey) && event.keyCode == 83) {
  // if ((event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    // $("#saveStateNav").click();
    // document.getElementById('saveStateNav').click();
    // $("#saveStateNav").trigger("click");
  // }
// });

// } else if ($(".fa-upload:visible" || ".shiny-bound-input:visible").is(":visible") &&
// } else if ($(".input-group-btn" || ".shiny-bound-input:visible").is(":visible") &&
