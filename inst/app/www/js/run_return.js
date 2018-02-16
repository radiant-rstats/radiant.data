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
  } else if($("#rmd_knit").is(":visible") &&
            // focus on rmd_knitted doesn't seem to work
            // $("#rmd_knitted").is(":focus") === false,
            $(".ace_text-input").is(":focus") === false &&
            $(".ace_search_form").is(":visible") === false &&
            event.metaKey === false &&
            event.ctrlKey === false &&
            event.shiftKey === false) {
    // don't change focus if meta, ctrl, of shift are pressed
    // allows selecting and copying output from #rmd_knitted
    $(".ace_text-input").focus();
  } else if($("#r_knit").is(":visible") &&
            // focus on r_knitted doesn't seem to work
            // $("#r_knitted").is(":focus") === false,
            $(".ace_text-input").is(":focus") === false &&
            $(".ace_search_form").is(":visible") === false &&
            event.metaKey === false &&
            event.ctrlKey === false &&
            event.shiftKey === false) {
    // don't change focus if meta, ctrl, of shift are pressed
    // allows selecting and copying output from #r_knitted
    $(".ace_text-input").focus();
 }

  // needed to address https://github.com/rstudio/shiny/issues/1916
  $("input:text").attr("spellcheck", "false");
});

// from https://stackoverflow.com/a/33251536/1974918 by Dean Attali
$(document).on("shiny:connected", function() {
// $(document).on("shiny:value", function(e) {
  // var jsWidth = screen.width;
  // var jsWidth = $(window).width();
  // Shiny.onInputChange("get_screen_width", jsWidth);
  Shiny.onInputChange("get_screen_width", $(window).width());
});

// based on https://stackoverflow.com/a/3150139/1974918
// $(document).ready(function () {
//   var addEvent = function(object, type, callback) {
//     if (object === null || typeof(object) == "undefined") return;
//     if (object.addEventListener) {
//       object.addEventListener(type, callback, false);
//     } else if (object.attachEvent) {
//       object.attachEvent("on" + type, callback);
//     } else {
//       object["on" + type] = callback;
//     }
//   };

//   var org_width = $(window).width();

//   addEvent(window, "resize", function(event) {
//     var jsWidth = $(window).width();
//     // if ($(window).width() != jsWidth) {
//     console.log(jsWidth);
//     console.log(org_width);
//     if (org_width != jsWidth) {
//       Shiny.onInputChange("get_screen_width", jsWidth);
//     }
//   });
// });

// $(document).ready(function () {
// $(document).on("shiny:connected", function() {
//   var width = $(window).width();
//   Shiny.onInputChange("get_screen_width", width);
//   function resizeIt() {
//     $(window).resize(function () {
//       if ($(window).width() != width) {
//         Shiny.onInputChange("get_screen_width", width);
//       }            
//     });
//   }
//   resizeIt();
// });

//// Things to still try
// https://stackoverflow.com/questions/20247945/bootstrap-3-navbar-dynamic-collapse
// https://stackoverflow.com/questions/18192082/bootstrap-3-navbar-collapse
// https://stackoverflow.com/questions/19827605/change-bootstrap-navbar-collapse-breakpoint-without-using-less

// function autocolllapse() {
//   var width_full = $('.navbar-collapse').innerWidth();
//   var width_left = $('.navbar-nav').outerWidth(true);
//   var width_right = $('.navbar-right').outerWidth(true);
//   if (width_full - (width_left + width_right) < 0) {
//       // trigger collapse, you have to figure out what css changes
//       // exactly are needed, for example:
//       $('.navbar-toggle').css('display', 'inline');
//   }
// };

// function autocollapse() {
//     var navbar = $('.navbar');
//     navbar.removeClass('collapsed');  // set standart view
//     if(navbar.innerHeight() > 75) {   // check if we've got 2 lines
//       navbar.addClass('collapsed');   // force collapse mode
//     }
// }

// change class not working ... yet
// $('#pvt_run').removeClass('btn-sucsses').addClass('btn-warning');
// $('#viz_run').removeClass('btn-sucsses').addClass('btn-warning');
// $('#viz_run').text()
// $('#view_store').text()
