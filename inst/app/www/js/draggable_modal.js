// From http://stackoverflow.com/a/27122472/1974918
// $(function(){
//   $('.modal').modal({ keyboard: false,
//                      show: true
//   });
//   // Jquery draggable
//   $('.modal-dialog').draggable({
//       handle: ".modal-header"
//   });
// });

// https://stackoverflow.com/questions/45062397/make-bootstrap-modal-draggable-and-keep-background-usable/45062993
// perhaps try again when jquery-ui.js is upgraded in the next shiny release?
// $('#manage_help').click(function() {
//   // reset modal if it isn't visible
//   if (!($('.modal.in').length)) {
//     $('.modal-dialog').css({
//       top: 0,
//       left: 0
//     });
//   }
//   $('#manage_help').modal({
//     backdrop: false,
//     show: true
//   });

//   $('.modal-dialog').draggable({
//     handle: ".modal-header"
//   });
// });