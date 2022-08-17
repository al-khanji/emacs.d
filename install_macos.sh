#!/bin/bash -eux

if ! which brew
then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

brew tap homebrew/cask-fonts
brew install --cask font-iosevka

brew tap d12frosted/emacs-plus
brew install emacs-plus --with-native-comp --with-imagemagick --with-xwidgets

ln -sf /usr/local/opt/emacs-plus/Emacs.app /Applications
