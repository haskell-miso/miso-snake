miso-snake
==========

Snake, using [miso](https://github.com/haskell-miso/miso).

Adapted from [elm-snake](https://github.com/theburningmonk/elm-snake), using
miso's SVG api instead of elm's `Graphics.Collage`.

Play it [here](http://snake.haskell-miso.org/)


## Build and run (Web Assembly)

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop
make
make serve
```

