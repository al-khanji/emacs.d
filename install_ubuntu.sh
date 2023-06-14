#!/bin/bash -eux

sudo apt update
sudo apt install -y libtool libtool-bin cmake

sudo snap install emacs --classic

TTF_IOSEVKA_TERM_ZIP="ttf-iosevka-term-24.1.3.zip"
DOWNLOADS_DIR="$HOME/Downloads"
FONTS_DIR="$HOME/.local/share/fonts"
install -d "$DOWNLOADS_DIR"
cd "$DOWNLOADS_DIR"
curl -sLO "https://github.com/be5invis/Iosevka/releases/download/v24.1.3/$TTF_IOSEVKA_TERM_ZIP"
cd -
install -d "$FONTS_DIR"
cd "$FONTS_DIR"
unzip -o "$DOWNLOADS_DIR/$TTF_IOSEVKA_TERM_ZIP"
fc-cache -f
cd -

~/.emacs.d/setup_bash_for_vterm.sh
