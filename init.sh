#!/bin/sh

files=".bashrc .emacs .gitconfig .mailrc .signature .zilerc .zshrc"

for file in $files; do
    if ! test -f "$HOME/$file"; then
	absfile="$(pwd)/$file"
	printf "%s: linking %s -> %s\n" "$0" "$HOME/$file" "$absfile"

	if ! ln -s "$absfile" "$HOME/$file"; then
	    printf "%s: linking %s failed\n" "$0" "$HOME/$file"
	    continue
	fi
    fi
done

