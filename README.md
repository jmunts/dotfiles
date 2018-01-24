# jmunts/dotfiles
  My personal dotfiles


# Prerequisites
### Ag
```
brew install the_silver_searcher
```

### fzf
On OS X, using Homebrew
```
brew install fzf

# Install shell extensions
/usr/local/opt/fzf/install
```

## OSX
This converts screencapture file type to jpg. For lesser file size.
```
defaults write com.apple.screencapture type jpg
```

This allows the keys to repeat when held
```
defaults write -g ApplePressAndHoldEnabled -bool false
```
For Visual Studio Code
```
defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false
```
## zerodark-theme for emacs
For the icons to show, please install the fonts in [fonts/zerodark-theme](https://github.com/jmunts/dotfiles/tree/master/fonts/zerodark-theme).

### OSX
```
mv <dotfiles_dir>/fonts/zerodark-theme/* ~/Library/Fonts
```

