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

var cardSelection = [];

function choices(id,low,high) {
  var choices = [];

  for (var i=0; i<cardSelection.length; i++) {
    choices.push($(cardSelection[i]).attr('name'));
  }

  $("#"+id+" input:checked").each(function () {
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
  $(".cardbox").click(function() {
    var idx = cardSelection.indexOf(this);
    var it = this;
    if (idx != -1) {
      cardSelection = cardSelection.filter(function (e) { return e != it;});
    } else {
      cardSelection.push(this);
    }
    $(this).toggleClass('checked');
  });
});

function startGame(type, cards) {
  $.post("/game/start",
      JSON.stringify({ "players":["Alice","Bob"], "cards":cards, "type":type }),
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
    startGame($("input[name='gametype']:checked").val(), tableau);
  }
}

function shuffle(o){
    for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
}

function randomStart(cards) {
  var tableau = [];

  $(".item.selected").each(function () {
    tableau.push($(this).attr('id'));
  });

  if (tableau.length > 10) {
    showWarning("You already selected more than 10 cards.");
  } else {
    var remaining = cards.filter(function (card) { return tableau.indexOf(card) == -1; });
    var selected = shuffle(remaining).slice(0,10 - tableau.length);

    startGame($("input[name='gametype']:checked").val(), tableau.concat(selected));
  }
}