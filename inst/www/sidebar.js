// ---- JS and CSS to fully collapse sidebar ----
// Modified from: https://stackoverflow.com/a/53517933
// '#sidebarCollapsed' data-collapsed="true"
$(document).on('shiny:connected', function(event) {
  $('.sidebar-toggle').on('click', function() {
    if ($('body')[0].className === 'skin-blue sidebar-mini sidebar-open') {
      $('#sidebarCollapsed').css('display', 'block')
    } else if ($('body')[0].className != 'skin-blue sidebar-mini sidebar-collapse') {
      $('#sidebarCollapsed').css('display', 'none')
    } else {
      $('#sidebarCollapsed').css('display', 'block')
    }
  })
});
