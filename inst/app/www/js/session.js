Shiny.addCustomMessageHandler("session_start", function(data) {
  var search = location.search;
  var reSSUID = /([?&])SSUID=[^&]*&?/g;

  if (search.length > 0) {
    if (reSSUID.test(search))
      search = search.replace(reSSUID, "$1");
    if (!/[?&]$/.test(search))
      search += "&";
    search += "SSUID=" + encodeURIComponent(data);
  } else {
    search = "?SSUID=" + encodeURIComponent(data);
  }

  // Joe Cheng: "Work around ShinyApps.io/SSP/RSC base href silliness"
  var path = location.pathname.replace(/\/_w_(\w+)/, "");
  history.replaceState(null, null, path + search);
})
