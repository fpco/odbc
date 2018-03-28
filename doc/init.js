$(document).ready(function() {
  $('pre,code').each(function(i, block) {
    hljs.highlightBlock(block);
  });
  $('.show').removeClass('show').addClass('hide');
});
