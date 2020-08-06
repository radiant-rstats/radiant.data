// This recieves messages of type "testmessage" from the server.
// See https://shiny.rstudio.com/gallery/server-to-client-custom-messages.html
// for details
// Code copied from https://github.com/rstudio/shiny-examples/tree/master/088-action-pattern1
Shiny.addCustomMessageHandler("message",
  function(message) {
    alert(JSON.stringify(message));
  }
);
