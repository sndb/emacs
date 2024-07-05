#!/bin/sh -x

mkdir -p $HOME/.config/emacs
ln -sr early-init.el init.el sndb-modules $HOME/.config/emacs
