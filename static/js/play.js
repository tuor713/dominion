function choose(val) {
  $.post(location.href,
    val,
    function () { location.reload(); },
    'text'
  )
}

function pass() {
  choose('');
}

function choices(id,low,high) {
  var choices = [];

  $("#"+id+" input.checked").each(function () {
    choices.push($(this).attr('name'));
  });

  if (choices.length < low) {
    alert("Not enough cards selected. Please select at least " + low + " card(s).")
  } else if (choices.length > high) {
    alert("Too many cards selected. Please select at most " + high + " card(s).");
  } else {
    choose(choices.join(','));
  }
}

$(function () {
  $(".checkbox").click(function(){
    $(this).toggleClass('checked');
  });
});

function start() {
  var tableau = [];

  $(".item.selected").each(function () {
    tableau.push($(this).attr('id'));
  });

  if (tableau.length != 10) {
    alert("Please select exactly 10 cards.")
  } else {
    $.post("/game/start",
      JSON.stringify({"players":["Alice","Bob"], "cards":tableau}),
      function (id) { window.location.href = "/game/"+id +"/decision/Alice?format=html"; },
      'text'
    );
  }
}