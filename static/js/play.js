function showWarning (msg) {
  $("#flash").addClass("ui negative message")
    .append("<i class='close icon'></i><div class='header'>"+msg+"</div>");

  $("#flash .close").click(function () {
    // revert the changes
    $("#flash").empty();
    $("#flash").removeClass("ui negative message");
  });
}

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

  // semantic ui checkboxes put checked on the div
  $("#"+id+" .checked input").each(function () {
    choices.push($(this).attr('name'));
  });


  if (choices.length < low) {
    showWarning("Not enough cards selected. Please select at least " + low + " card(s).");
  } else if (choices.length > high) {
    showWarning("Too many cards selected. Please select at most " + high + " card(s).");
  } else {
    choose(choices.join(','));
  }
}

$(function () {
  $(".checkbox").click(function(){
    $(this).toggleClass('checked');
  });
});

function startGame(cards) {
  $.post("/game/start",
      JSON.stringify({"players":["Alice","Bob"], "cards":cards}),
      function (id) { window.location.href = "/game/"+id +"/decision/Alice?format=html"; },
      'text'
    );
}

function start() {
  var tableau = [];

  $(".item.selected").each(function () {
    tableau.push($(this).attr('id'));
  });

  if (tableau.length != 10) {
    showWarning("Please select exactly 10 cards.");
  } else {
    startGame(tableau);
  }
}

function shuffle(o){
    for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
}

function randomStart(cards) {
  var selected = shuffle(cards).slice(0,10);
  startGame(selected);
}