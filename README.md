# Elevator game Haskell bindings

There's a cool game where you get to program your own elevators: 
https://play.elevatorsaga.com/
I wanted to play it, but I didn't want to use JavaScript.

This project uses GHCJS to compile Haskell code into JavaScript that can be
pasted into the game window.

## Building

```
cd elevator-game
nix-shell
# inside nix-shell
./build
# copy the contents of bin/out.js or
cat bin/out.js | xsel -b
# paste it in the browser
```
