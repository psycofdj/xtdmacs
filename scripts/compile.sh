#!/bin/bash

# l_basePath=$(dirname $(readlink -f $0))

find . -name '*.elc' | xargs rm -f
# echo "(load \"$l_basePath/dot.emacs\")" > /tmp/todo.el
find $l_basePath -name '*.el'  | sed -r 's/(.*)/(byte-compile-file "\1")/g' > /tmp/todo.el
emacs -nw -batch -l /tmp/todo.el -kill
rm -f /tmp/todo.el
