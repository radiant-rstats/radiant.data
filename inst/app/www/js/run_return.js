// based on http://stackoverflow.com/a/32340906/1974918
// and http://stackoverflow.com/a/8774101/1974918
// check out https://www.youtube.com/watch?v=tM0q3u220mI for debugging
// https://stackoverflow.com/questions/35831811/register-repeated-keyboard-presses-in-shiny
// https://github.com/rstudio/shiny/issues/928
// https://stackoverflow.com/questions/32002170/r-shiny-enabling-keyboard-shortcuts
// https://stackoverflow.com/questions/47569992/home-button-in-header-in-r-shiny-dashboard
$(document).keydown(function (event) {

  // console.log(document.activeElement)
  if ($(".btn-success:visible" || ".shiny-bound-input:visible").is(":visible") &&
    (event.metaKey || event.ctrlKey || event.shiftKey) && event.keyCode == 13) {
    $(".btn-success:visible" || ".shiny-bound-input:visible").click();
  } else if ($(".fa-edit:visible" || ".shiny-bound-input:visible").is(":visible") &&
    event.altKey && event.keyCode == 13) {
    $(".fa-edit:visible" || ".shiny-bound-input:visible").click();
  } else if ($("#updateDescr").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#updateDescr").click();
    event.preventDefault();
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
    document.getElementById("state_load").click();
    event.preventDefault();
  } else if ($("#uploadfile").is(":visible") && (event.metaKey || event.ctrlKey) &&
    event.shiftKey === false && event.keyCode == 79) {
    $("#uploadfile").click();
    event.preventDefault();
  } else if ($("#man_save_data").is(":visible") && (event.metaKey || event.ctrlKey) &&
    event.shiftKey === false && event.keyCode == 83) {
    $("#man_save_data").click();
    event.preventDefault();
  }

  // focusing in text (area) inputs
  if ($("#data_rename").is(":focus") && event.keyCode == 13) {
    $("#renameButton").click();
  } else if ($("#url_csv").is(":focus") && event.keyCode == 13) {
    $("#url_csv_load").click();
  } else if ($("#url_rds").is(":focus") && event.keyCode == 13) {
    $("#url_rds_load").click();
  } else if ($("#view_name").is(":focus") && event.keyCode == 13) {
    $("#view_store").click();
  } else if ($("#pvt_name").is(":focus") && event.keyCode == 13) {
    $("#pvt_store").click();
  } else if ($("#expl_name").is(":focus") && event.keyCode == 13) {
    $("#expl_store").click();
  } else if ($("#tr_name").is(":focus") && event.keyCode == 13) {
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
  } else if ($("#rmd_knit").is(":visible") && document.activeElement === document.body) {
    $(".ace_text-input").focus();
  } else if ($("#r_knit").is(":visible") && document.activeElement === document.body) {
    $(".ace_text-input").focus();
  }

  // needed to address https://github.com/rstudio/shiny/issues/1916
  $("input:text").attr("spellcheck", "false");
});

$(function () {
  $("#state_save_link").on('click', function (e) {
    e.preventDefault();
    $("#state_save").trigger('click');
  });
  $("#state_load_link").on('click', function (e) {
    e.preventDefault();
    $("#state_load").trigger('click');
  });
  $("#state_upload_link").on('click', function (e) {
    e.preventDefault();
    $("#state_upload").trigger('click');
  });
});

// from https://stackoverflow.com/a/33251536/1974918 by Dean Attali
$(document).on("shiny:connected", function () {
  Shiny.onInputChange("get_screen_width", $(window).width());
});

// from https://github.com/rstudio/shiny/issues/2033#issuecomment-386438821
$(document).on('shiny:disconnected', function () {
  window.parent.postMessage('disconnected', '*');
});

// based on https://stackoverflow.com/questions/61690502/shiny-setinputvalue-only-works-on-the-2nd-try
function get_img_src() {
  var img_src = $("#screenshot_preview img").attr("src");
  Shiny.setInputValue("img_src", img_src);
}

function generate_screenshot() {
  html2canvas($("body")[0],{ignoreElements:function (el) {return el.className === 'dropdown-menu';}}).then(canvas=>{
    var img = document.createElement("img");
    img.src = canvas.toDataURL("png");
    img.width = parseInt(canvas.style.width);
    img.height = parseInt(canvas.style.height);
    $("#screenshot_preview").empty();
    $("#screenshot_preview").append(img);
  });
}
