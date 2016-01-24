import 'dart:js';
import 'dart:html';
import 'dart:convert';

// HTML manipulation

showWarning(msg) {
  var flash = querySelector('#flash');
  flash.classes.addAll(['ui', 'negative', 'message']);
  flash.appendHtml("<i class='close icon'></i><div class='header'>${msg}</div>");

  querySelector('#flash .close').onClick.listen((e) {
    flash.setInnerHtml('');
    flash.classes.removeAll(['ui', 'negative', 'message']);
  });
}

logToConsole(val) {
  context['console'].callMethod('log',[val]);
}

// User choices

choose(val) {
  HttpRequest.request(window.location.href,
      method: "POST",
      sendData: val)
    .then((resp) { window.location.reload(); });
}

chooseSelected(id,min,max) {
  var choices = querySelectorAll('#${id} input.checked').map((e) => e.getAttribute('name')).toList();
  choices.addAll(querySelectorAll('#${id} input[checked]').map((e) => e.getAttribute('name')));

  if (choices.length < min) {
    showWarning("Not enough choices selected. Please select at least ${min} item(s).");
  } else if (choices.length > max) {
    showWarning("Too many choices selected. Please select at most ${max} items(s).");
  } else {
    choose(choices.join(','));
  }
}

// Game functions

startGame(type, players, cards) {
  HttpRequest.request('/game/start',
      method: 'POST',
      sendData: JSON.encode({'players':players, 'cards':cards, 'type':type}))
  .then((req) {
    var id = req.responseText;
    logToConsole("Started game: ${id}");
    window.location.href = '/game/${id}/decision/Alice?format=html';
  });
}

getStartGamePlayer() {
  var players = [];
  [1,2,3,4].forEach((i) {
    var typ = (querySelector('#playerType${i}') as SelectElement).selectedOptions[0].value;

    if (typ != 'none') {
      var name = querySelector('#playerName${i}').getAttribute('value');
      players.add({'name':name, 'type':typ});
    }
  });

  return players;
}

List<String> getSelectedCards() {
  return querySelectorAll('.cardselection.selected').map((e) => e.getAttribute('id')).toList();
}

getGameType() {
  return querySelector('input[name="gametype"][checked]').getAttribute('value');
}

start() {
  var tableau = getSelectedCards();

  if (tableau.length == 10) {
    startGame(getGameType(), getStartGamePlayer(), tableau);
  } else {
    showWarning('Please select exactly 10 cards.');
  }
}

randomStart(List<String> choices) {
  var tableau = getSelectedCards();
  var protoSet = tableau.toSet();

  var filteredChoices = choices.where((e) => !protoSet.contains(e)).toList();
  filteredChoices.shuffle();
  tableau.addAll(filteredChoices.take(10 - tableau.length));

  startGame(getGameType(), getStartGamePlayer(), tableau);
}

// HTML interaction, scaffolding

initHtml() {
  // semantic ui setup

  context.callMethod(r'$', ['.ui.dropdown'])
      .callMethod('dropdown');

  // selection

  querySelectorAll('.cardselection').forEach((elem) {
    elem.onClick.listen((e) => elem.classes.toggle('selected'));
  });

  querySelectorAll('.cardbox').forEach((elem) {
    elem.onClick.listen ((e) => elem.classes.toggle('checked'));
  });

  // choices

  querySelectorAll('[data-choice]').forEach((elem) {
    elem.onClick.listen((e) => choose(elem.getAttribute('data-choice')));
  });

  querySelectorAll('[data-choice-id]').forEach((elem) {
    elem.onClick.listen((e) => chooseSelected(elem.getAttribute('data-choice-id'),
                                              int.parse(elem.getAttribute('data-choice-min')),
                                              int.parse(elem.getAttribute('data-choice-max'))));
  });

  querySelector('#startGame')?.onClick?.listen((e) => start());

  var rElem = querySelector('#randomStart');
  if (rElem != null) {
    var choices = JSON.decode(rElem.getAttribute('data'));
    rElem.onClick.listen((e) => randomStart(choices));
  }

  querySelectorAll('[data-tableau]').forEach((elem) {
    var cards = elem.getAttribute('data-tableau').split(',');
    elem.onClick.listen((e) => startGame('standard',[{'name':'Alice','typ':'human'}, {'name':'Bob','typ':'bot'}],cards));
  });
}

main () {
  window.onLoad.listen((e) => initHtml());
}