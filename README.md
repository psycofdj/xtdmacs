Xtdmacs provides a bunch a development tools and ready-to-use configuration.

Install
=======


```bash
wget https://github.com/psycofdj/xtdmacs/archive/0.0.1.tar.gz -O xtdmacs-0.0.1.tar.gz
tar xvzf xtdmacs-0.0.1.tar.gz
cd xtdmacs-0.0.1
make install
```

Features
========

Bindings
--------

  [home]       . beginning-of-line)
  [select]     . end-of-line)
  [M-up]       . beginning-of-buffer)
  [A-up]       . beginning-of-buffer)
  [M-down]     . end-of-buffer)
  [A-down]     . end-of-buffer)
  [C-right]    . forward-word)
  [C-left]     . backward-word)
  "\C-c\C-g"   . goto-line)
  "\C-d"       . query-replace)
  "\C-f"       . query-replace-regexp)
  "\M-d"       . align-regexp)
  [24 down]    . windmove-down)
  [24 right]   . windmove-right)
  [24 left]    . windmove-left)
  [24 up]      . windmove-up)
  [24 C-right] . swbuff-switch-to-next-buffer)
  [24 C-left]  . swbuff-switch-to-previous-buffer)
  [24 C-down]  . iswitchb-buffer)
  "\C-xk"      . kill-buffer)
  "\C-x\C-f"   . find-file)
  "\M-+"       . enlarge-window)
  "\M--"       . shrink-window)
  [M-delete]   . kill-word)
  "\e "        . dabbrev-expand)
  "\es"        . speedbar-get-focus)
  "C"          . self-insert-command)
  "\C-xl"      . xtdmacs-insert-date)
  "\M-q"       . comment-region)
  "\M-a"       . uncomment-region)
  [f5]         . delete-trailing-whitespace)
  [C-f5]       . font-lock-fontify-buffer)
  [C-f11]      . xtdmacs-shell-toggle)
  [f11]        . tmm-menubar))

