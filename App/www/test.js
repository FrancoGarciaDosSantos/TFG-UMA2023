
function changeText(buttonId, text) {
  btn = document.getElementById(buttonId);
  textBack = btn.getAttribute('data-original-title')
  $(btn).attr('title', text).tooltip('fixTitle').tooltip('show');
  
  setTimeout(function() { back(btn, textBack); }, 100);
}

function back(button, textBack){ 
  $(button).attr('title', textBack).tooltip('fixTitle');
}

