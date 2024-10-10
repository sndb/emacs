#!/bin/sh -x

mkdir -p $HOME/.config/emacs
ln -sfr early-init.el init.el sndb-modules $HOME/.config/emacs
