// Add JavaScript to add an id to the <section> tag so we can overlay waiter on top of it
// Source: https://waiter.john-coene.com/#/examples?id=shinydashboard
$( document ).ready(function() {
  var section = document.getElementsByClassName('content');
  section[0].setAttribute('id', 'waiter-content');
});
