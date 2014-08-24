## Introduction

This is the Emacs configuration that [Alex Payne](http://al3x.net/) ([@al3x](https://github.com/al3x)) uses. It's not a "starter kit" or "configuration framework" or anything like that.

## Overview

* Packages are managed with [Cask](https://github.com/cask/cask) and [Pallet](https://github.com/rdallasgray/pallet).
* Works on Mac OS X. I install Emacs on OS X via [Homebrew](http://brew.sh/).
* Previously worked on Linux (Emacs 23 GTK on Ubuntu), but _not actively maintained for Linux_.
* Has some useful accommodations for Dvorak typists, such as `C-t` being mapped to `C-x`, and being able to `C-x c` (actually `C-t c`) to do a `M-x`.
  * Note that all of the above are way more comfortable if you set caps lock to act as ctrl.
* Assumes that you want to use Emacs for editing text and code and not much more. No email, IRC, web browsing, Org mode craziness, etc.

## Supported Syntaxes

Beyond what Emacs supports out of the box:

* Clojure
* Go
* Scala
* Haskell
* CoffeeScript
* Markdown
* YAML

...and some others. See the `Caskfile` for the whole gamut.

## Additional Modes and Tweaks

* Magit – integration with the Git VCS
* ag – a nice interface to The Silver Searcher, which supercedes grep and ack
* defaults to the [Solarized](http://ethanschoonover.com/solarized) dark theme

## Inspiration

* [technomancy](http://github.com/technomancy/emacs-starter-kit)
* [whilp](https://github.com/whilp/dotfiles/tree/master/.emacs.d)
