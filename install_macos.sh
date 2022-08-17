#!/bin/bash -eux

if [ ! which brew ]
do
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

brew tap d12frosted/emacs-plus
brew install font-iosevka
brew install emacs-plus --with-native-comp --with-imagemagick --with-xwidgets
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications
