#!/bin/sh

ln -s ~/code/dotfiles/gitignore ~/.gitignore
ln -sn ~/code/dotfiles/emacs.d ~/.emacs.d

#######
# OSX #
######

# emacs markdown-mode
brew install multimarkdown

# emacs github flavored markdown preview
brew install node
npm install -g vmd
