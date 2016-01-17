# Dominion Engine

A small collection of code for playing around with Haskell
and trying to build an engine for simulating Dominion games.
Potentially at some point to test run bots, as well as
fledging domain design muscles.

See also [Dominion Wiki](http://wiki.dominionstrategy.com/index.php/Main_Page).

## Prior Art

Two excellent simulators are out there that implement much more cards,
much more reliably and have loads of strategies defined.

- [Geronimoo's simulator](https://github.com/Geronimoo/Geronimoo-s-Dominion-Simulator)
- [Dominiate](https://github.com/rspeer/dominiate)

## Building

- Get Dart (1.13.2), GHC (7.10.2) and Cabal (1.22.4.0)
- External dependencies, not included
  - Get SemanticUI and install into `static/lib/semanticui`
  - Get Dominion Wiki images and store in `static/images/cards`
- Build Dart sources `dart2js src/dart/main.dart -o static/js/app.js`
- Start the server `cabal run web`
- Navigate to [http://localhost:8080](http://localhost:8080)