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

xtdmacs-bindings
----------------

This optional mode setup keyboard bindings for the most commonly used
features.

It also configures iswitchb module to ignore standard system buffers like
\*Help\* \*scratch\* \*Messages\* etc..

### Install

##### manually

```
M-x xtdmacs-bindings-mode
```

##### from ~/.emacs:
```
(xtdmacs-bindings-mode)
```

##### using emacs customizaton:

```M-x customize-variable RET xtdmacs-bindings-mode```

-> change value to on, then apply and save

### Bindings

| Key                           | Effect                                | Key                           | Effect                                |
|-------------------------------|---------------------------------------|-------------------------------|---------------------------------------|
| \<home\>                      | move cursor to end of line            | \<ctrl\>+x \<ctrl\>+\<right\> | display next buffer                   |
| \<select\>                    | move cursor to end of line            | \<ctrl\>+x \<ctrl\>+\<left\>  | display previous buffer               |
| \<alt\>+\<up\>                | move cursor to beggining of buffer    | \<ctrl\>+x \<ctrl\>+\<down\>  | prompt buffer to display              |
| \<alt\>+\<down\>              | move curtor to end of buffer          | \<ctrl\>+x k                  | close current buffer                  |
| \<ctrl\>+\<right\>            | move cursor to end of word            | \<ctrl\>+x \<ctrl\>+f         | open file                             |
| \<ctrl\>+\<left\>             | move cursor to beginning of word      | \<alt\>+\<plus\>              | enlarge current window's height       |
| \<ctrl\>+c \<ctrl\>+g         | goto given line                       | \<alt\>+\<minus\>             | shink current window's height         |
| \<ctrl\>+d                    | search and replace                    | \<alt\>+\<delete\>            | delete previous word                  |
| \<ctrl\>+f                    | search and replace regexp             | \<alt\>+s                     | display speedbar                      |
| \<alt\>+d                     | align regexp                          | \<alt\>+/                     | autocomplete current word             |
| \<ctrl\>+x \<right\>          | move cursor to the right window       | \<ctrl\>+l                    | insert current date                   |
| \<ctrl\>+x \<left\>           | move cursor to the left window        | \<alt\>+q                     | comment region                        |
| \<ctrl\>+x \<up\>             | move cursor to the top window         | \<alt\>+a                     | uncomment region                      |
| \<ctrl\>+x \<down\>           | move cursor to the bottom window      | \<F5\>                        | delete buffer's trailing whitespaces  |
| \<ctrl\>+\<F5\>               | refresh buffer syntax colors          | \<ctrl\>+\<F11\>              | toggle terminal shell                 |
| \<F11\>                       | display menu                          |                               |                                       |


xtdmacs-loader
--------------

This package helps customizing which minors modes should be loaded for each
file extensions.

In order modify associations between file extensions are minor modes, the simpler
is to customize the *xtdmacs-loader-auto-minor-mode-alist* variable.

![alt text](doc/xtdmacs-loader.png "Logo Title Text 1")


xtdmacs-code-mode
-----------------

This minor mode enabled multi-language tools that help editing code.

### highlight+80+-mode

Colorize lines that are longer than a specific number of columns (default 80).
![alt text](doc/xtdmacs-code-highlight80+.png "Logo Title Text 1")

To set the number of columns :
```
M-x customize-variable RET highlight-80+-columns RET
```

You can also customize the face used to display long lines :
```
M-x customize-face RET highlight-80+ RET
M-x customize-face RET highlight-80+-first RET
M-x customize-face RET highlight-80+-line RET
```

### linum-mode

Display current line number and fix default window margin

![alt text](doc/xtdmacs-code-linum.png "Logo Title Text 1")

To customize columns number face :
```
M-x customize-face RET linum RET
```
