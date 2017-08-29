<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Introduction](#introduction)
- [Install](#install)
- [Loading modes](#loading-modes)
    - [Manually](#manually)
    - [From ~/.emacs:](#from-emacs)
    - [Using emacs customization](#using-emacs-customization)
    - [Using xtdmacs-loader](#using-xtdmacs-loader)
- [General purpose modes](#general-purpose-modes)
    - [xtdmacs-bindings](#xtdmacs-bindings)
        - [Ido Bindings](#ido-bindings)
        - [Cursor Bindings](#cursor-bindings)
        - [Other bindings](#other-bindings)
    - [xtdmacs-find package](#xtdmacs-find-package)
    - [xtdmacs-code-mode](#xtdmacs-code-mode)
        - [fill-column-indicator](#fill-column-indicator)
        - [linum-mode](#linum-mode)
        - [Aligning variables and parameters](#aligning-variables-and-parameters)
        - [Code Bindings](#code-bindings)
    - [xtdmacs-compile++-mode](#xtdmacs-compile-mode)
        - [Window management](#window-management)
        - [Behind the curtain](#behind-the-curtain)
        - [Compile API](#compile-api)
        - [Compile Configuration](#compile-configuration)
        - [Compile Bindings](#compile-bindings)
    - [xtdmacs-code-doxygen-mode](#xtdmacs-code-doxygen-mode)
        - [Templates](#templates)
        - [Keyword and faces](#keyword-and-faces)
        - [Doxymacs Bindings](#doxymacs-bindings)
    - [xtdmacs-code-line-mode](#xtdmacs-code-line-mode)
    - [xtdmacs-code-spell-mode & xtdmacs-code-spell-prog-mode](#xtdmacs-code-spell-mode--xtdmacs-code-spell-prog-mode)
        - [Spell Configuration](#spell-configuration)
        - [Spell Faces](#spell-faces)
        - [Spell API](#spell-api)
        - [Spell Bindings](#spell-bindings)
- [Language specific modes](#language-specific-modes)
    - [xtdmacs-code-cpp-mode](#xtdmacs-code-cpp-mode)
        - [C++ Configuration](#c-configuration)
        - [C++ Faces](#c-faces)
        - [C++ API](#c-api)
        - [C++ Bindings](#c-bindings)
    - [xtdmacs-code-python-mode](#xtdmacs-code-python-mode)
        - [Python Faces](#python-faces)
        - [Python API](#python-api)
        - [Python Configuration](#python-configuration)
    - [xtdmacs-code-php-mode](#xtdmacs-code-php-mode)
        - [Php Faces](#php-faces)
        - [Php Configuration](#php-configuration)
    - [xtdmacs-code-lisp-mode](#xtdmacs-code-lisp-mode)
        - [Configuration](#configuration)
    - [xtdmacs-code-json-mode](#xtdmacs-code-json-mode)
        - [Json Bindings](#json-bindings)
    - [xtdmacs-code-web-mode](#xtdmacs-code-web-mode)
    - [xtdmacs-code-makefile-mode](#xtdmacs-code-makefile-mode)
    - [xtdmacs-code-java-mode](#xtdmacs-code-java-mode)
    - [xtdmacs-code-js-mode](#xtdmacs-code-js-mode)
    - [xtdmacs-code-sphinx-mode](#xtdmacs-code-sphinx-mode)

<!-- markdown-toc end -->

# Introduction

Xtdmacs provides a bunch a development tools and ready-to-use configuration.
Each feature is bundled as a separate minor mode.


# Install

The following procedure downloads latest package version and install xtdmacs and
all its dependencies in your elpa directory, usually ```~/.emacs.d/elpa```.

 ```bash
 tag=$(curl -s https://api.github.com/repos/psycofdj/xtdmacs/tags | jq -r '[ .[] | .["name"] ] | sort | last')
 wget https://github.com/psycofdj/xtdmacs/archive/${tag}.tar.gz -O xtdmacs-${tag}.tar.gz
 tar xvzf xtdmacs-${tag}.tar.gz
 cd xtdmacs-${tag}
 make install
 ```

# Loading modes

Each mode provided by xtdmacs can be loaded like every other minor mode. However
we recommend to use the xtdmacs-loader described below.

## Manually

```M-x xtdmacs-bindings-mode RET```

## From ~/.emacs:

``` (xtdmacs-bindings-mode) ```

## Using emacs customization

```M-x customize-variable RET xtdmacs-bindings-mode RET```

## Using xtdmacs-loader

Xtdmacs' package provides is own minor-mode loading system. It is very similar
to default ```minor-mode-alist``` but allows to define the same mode list
to several file extensions.

This package helps customizing which minors modes should be loaded for each
file extensions.

In order modify associations between file extensions are minor modes, the simpler
is to customize the *xtdmacs-loader-auto-minor-mode-alist* variable.

![alt text](doc/xtdmacs-loader.png "Logo Title Text 1")


To enable Xtdmacs' loader, you need to load package at start up :
```elisp
;; in your ~/.emacs
(require 'xtdmacs-loader)
```



# General purpose modes

## xtdmacs-bindings

This optional mode setup keyboard bindings for the most commonly used
features.

### Ido Bindings

The ido (Interactively do things) mode provides an efficient way to navigate among
opened buffers. Ido display available buffer names in mini-buffer and filters the
list as you type characters.

swbuff defines functions to directly cycle among existing buffers. It also
provides a way to ignore a list of buffer names in this cycle. Typically,
users will ignore systems buffers like ```*Help*``` or ```*Message*```.

To customize list of ignore buffers :

```M-x customize-variable RET swbuff-exclude-buffer-regexps```

ido completion will also ignore patterns defined in
```swbuff-exclude-buffer-regexps``` but will suggest them if typed characters
matches nothing but filtered buffer names.


Example:
![IDO mode](doc/ido-mode.png "IDO mode")

| Key                           | Effect                                |
|-------------------------------|---------------------------------------|
| \<ctrl\>+x \<ctrl\>+\<down\>  | Run ido interactive buffer selection  |
| \<left\>                      | (in ido) next buffer suggestion       |
| \<left\>                      | (in ido) previous buffer suggestion   |
| RET                           | (in ido) display selected buffer      |
| \<ctrl\>+x \<ctrl\>+\<right\> | display next buffer                   |
| \<ctrl\>+x \<ctrl\>+\<left\>  | display previous buffer               |


### Cursor Bindings

| Key                           | Effect                                |
|-------------------------------|---------------------------------------|
| \<home\>                      | move cursor to beginning of line      |
| \<select\>                    | move cursor to end of line            |
| \<alt\>+\<up\>                | move cursor to beginning of buffer    |
| \<alt\>+\<down\>              | move cursor to end of buffer          |
| \<ctrl\>+\<left\>             | move cursor to beginning of word      |
| \<ctrl\>+\<right\>            | move cursor to end of word            |
| \<ctrl\>+x \<right\>          | move cursor to the right window       |
| \<ctrl\>+x \<left\>           | move cursor to the left window        |
| \<ctrl\>+x \<up\>             | move cursor to the top window         |
| \<ctrl\>+x \<down\>           | move cursor to the bottom window      |
| \<ctrl\>+x \<ctrl\>+g         | move cursor to given line             |

### Other bindings

| Key                           | Effect                                |
|-------------------------------|---------------------------------------|
| \<ctrl\>+x \<ctrl\>+f         | open file                             |
| \<alt\>+\<plus\>              | enlarge current window's height       |
| \<alt\>+\<minus\>             | shrink current window's height        |
| \<alt\>+\<delete\>            | delete previous word                  |
| \<alt\>+s                     | display speed-bar                     |
| \<alt\>+/                     | auto-complete current word            |
| \<ctrl\>+d                    | search and replace                    |
| \<ctrl\>+f                    | search and replace regexp             |
| \<alt\>+d                     | align regexp                          |
| \<ctrl\>+\<F11\>              | toggle terminal shell                 |
| \<ctrl\>+l                    | insert current date                   |
| \<alt\>+q                     | comment region                        |
| \<alt\>+a                     | uncomment region                      |
| \<F5\>                        | delete buffer's trailing white-spaces |
| \<ctrl\>+\<F5\>               | refresh buffer syntax colors          |
| \<F11\>                       | display menu                          |


## xtdmacs-find package

```xtdmacs-find``` package provides an overload of standard emacs' ```find-file```
function. This overload allows to open existing files to specified line and
column number.

```bash
# open file to line 38
$ emacs -nw ~/.emacs:38

# open file to line 38 and column 5
$ emacs -nw ~/.emacs:38:5

# open file normally
$ emacs -nw ~/.emacs

# open unexisting file
$ emacs -nw ~/does_not_exist:20:4
# this will literally open the file named "does_not_exist:20:4"
```

To enable Xtdmacs' find overload, you need to load package at start up :
```elisp
;;in your .emacs:
(require 'xtdmacs-find)
```



## xtdmacs-code-mode

This minor mode enabled multi-language tools that help editing code.

### fill-column-indicator

Displays a vertical line at the specified column number, discouraging (but not
preventing) the developer to make too long lines.

The following variables customize the behavior of this minor mode :

| Variable           | Effect                                | Default   |
|--------------------|---------------------------------------|-----------|
| fci-rule-character | Character to use to display the line. | ┊ (UTF-8) |
| fci-rule-color     | Color of the rule character           | #333333   |
| fill-column        | Column number of the line rule        | 90        |



Example : (vertical line on extreme right)

![Fill columns indicator](doc/fci-mode.png "Fill columns indicator")


### linum-mode

Display current line number and fix default window margin

![alt text](doc/xtdmacs-code-linum.png "Logo Title Text 1")

To customize columns number face :
```
M-x customize-face RET linum RET
```

More generally, to customize the linum mode :
```
M-x customize-group RET linum RET
```

### Aligning variables and parameters

xtdmacs-code-mode provides two utility functions that format a specific region
to a **matrix readable** form :

- *xtdmacs-code-align-vars*
- *xtdmacs-code-align-args*

```c++
// given this code snippet :
// mark
void myfunction(const std::string& p_parameter1,
                int p_param2,
                std::vector<std::string>& p_result);
// cursor

// xtdmacs-code-align-args between mark and cursor will produce :
void myfunction(const std::string&        p_parameter1,
                int                       p_param2,
                std::vector<std::string>& p_result);


// given this code snippet :
// mark
  std::cout << "my current process" << l_tmp
            << "is about to fail because of " << l_reason
            << std::endl;
// cursor

// xtdmacs-code-align-args between mark and cursor will produce :
  std::cout << "my current process"           << l_tmp
            << "is about to fail because of " << l_reason
            << std::endl;



// given this code snippet :
void foo(void)
{
// mark
  int l_var1 = 0;
  string l_name = "bar";
  const vector<string> l_contacts = { "foo", "bar" };
// cursor
}


// xtdmacs-code-align-vars between mark and cursor will produce :
void foo(void)
{
  int                  l_var1     = 0;
  string               l_name     = "bar";
  const vector<string> l_contacts = { "foo", "bar" };
}
```


This feature relies on a strict parameter and variables naming convention.
- parameters : ```p[cs]?_.* | p[cs]?[A-Z].*```
- variables  : ```l[cs]?_.* | l[cs]?[A-Z].*```
- members    : ```m[cs]?_.* | m[cs]?[A-Z].*```
- globals    : ```g[cs]?_.* | g[cs]?[A-Z].*```
- counters   : ```c[cs]?_.* | c[cs]?[A-Z].*```

Note: ```c``` and ```s``` optional modifiers stands respectively for const and static



### Code Bindings


| Key                           | Effect                                         |
|-------------------------------|------------------------------------------------|
| \<ctrl\>+\<alt\>+\<up\>       | move cursor to beginning of current expression |
| \<ctrl\>+\<alt\>+\<down\>     | move cursor to end of current expression       |
| \<alt\>+q                     | comment region                                 |
| \<alt\>+a                     | uncomment region                               |
| \<F4\>                        | indent region                                  |
| \<ctrl\>+\<F4\>               | indent buffer                                  |
| \<ctrl\>+\<F1\>               | align variables between mark and cursor        |
| \<ctrl\>+\<F2\>               | align parameters between mark and cursor       |



## xtdmacs-compile++-mode

This minor mode wraps the default compilation mode in order to provide a set of
predefined compilation commands. It also allows to use function instead of
plain string as default compile commands.

There is 3 predefined commands : **compile**, **test** and **deploy**

### Window management

xtdmacs-compile++ dedicates a window to the compilation buffer's preventing
emacs to use it to open new files. It also sets this window's height according
to ```xtdmacs-compile++-buffer-height``` variable and enables optionally
automatic scrolling if ```xtdmacs-compile++-scroll-output``` is non nil.


### Behind the curtain

The predefined commands are defined in the **xtdmacs-compile++-config-alist**
variable.

Where **xtdmacs-compile++-config-alist** is an alist of the form
```lisp
(("command1" . config-alist)
 ("command2" . config-alist))
```

and where each *config-alist* is an alist of the form
```lisp
(("get-params" . function)
 ("command"    . string-or-function))
```

The **get-params** function is called interactively to prompt for specific parameters
of the command. Ex. for c++ "compile" command, we prompt for working directory,
optional environment variables and specific script to run.

The **command** item build the final command send to default compilation-mode. Ex. for
c++ it will construct something like ```cd dir && key=value make -j``` from values
prompted by **get-params**.

Usually, **get-params** uses *xtdmacs-compile++-config-alist* itself to store the values
prompted to user user. Ex :
```lisp
(("compile" .
     (("dir"        . "~/build")
      ("env"        . "VE=1")
      ("bin"        . "make -j 12")
      ("get-params" . (lambda() (xtdmacs-compile++-default-params  "compile")))
      ("command"    . (lambda() (xtdmacs-compile++-default-command "compile"))))))
```

### Compile API

**xtdmacs-compile++-mode** provide utility functions that helps building your own
**get-param** and **command** function values.

* **```xtdmacs-compile++-get-nearest-filename (name)```** returns the closest path parent
  to current buffer file that contains a file or a directory named *name*

* **```xtdmacs-compile++-get-dir-locals-directory```** returns the path containing
  the nearest .dir-locals.el configuration file (nil if none)

* **```xtdmacs-compile++-get-dir-git```** return the closest parent from buffer containing
  a *.git* directory, often used as project root directory.

* **```xtdmacs-compile++-guess-directory```** returns the build directory assuming your
  are using automake's VPATH builds in a directory named .release in your project root

* **``xtdmacs-compile++-get-current-branch``** returns (if any) the git branch name of
  the current buffer

In addition the mode provides some usable default function :

* **```xtdmacs-compile++-default-params```**: this function prompt interactively
  for :
  * a build directory, default given by ```"dir"``` function/value
  * some environment variables, default given by ```"env"``` function/value
  * a build command, , default given by ```"bin"``` function/value

* **```xtdmacs-compile++-default-command```** : build the command as
  * ```cd <bin> && <env> <bin>```

* **```xtdmacs-compile++-docker-params```** : like ```xtdmacs-compile++-default-params```
  but also prompts for :
  * a docker-compose service name, default given by ```"service"``` function/value
  * a docker-compose file path, default given by ```"compose-file"``` function/value

* **```xtdmacs-compile++-docker-run-command```** : build the command as
  * ```cd <dir> && docker-compose -f <compose-file> run [-e <env:key>=<env:val>] <service> <bin>```

* **```xtdmacs-compile++-docker-exec-command```** : same as ```xtdmacs-compile++-docker-run-command```
  but with exec sub command (>= docker v1.11)

### Compile Configuration

Define the number of lines displayed in compilation buffer :
* ```M-x customize-variable RET xtdmacs-compile++-buffer-height RET```

Enables automatic scrolling of compilation buffer :
* ```M-x customize-variable RET xtdmacs-compile++-scroll-output RET```

Set commands configuration interactively :
* ```M-x customize-variable RET xtdmacs-compile++-buffer-local RET```



The following variables targets one of the ```xtdmacs-compile++-config-alist``` keys.
Each command is bound to a specific keyboard key.
- ```M-x customize-variable RET xtdmacs-compile++-command-1 RET``` : default ```compile```
- ```M-x customize-variable RET xtdmacs-compile++-command-2 RET``` : default ```test```
- ```M-x customize-variable RET xtdmacs-compile++-command-3 RET``` : default ```deploy```
- ```M-x customize-variable RET xtdmacs-compile++-command-4 RET``` : default ```deploy```
- ```M-x customize-variable RET xtdmacs-compile++-command-5 RET``` : default ```iwyu```

Customize mode-line face when compile process is running :
* ```M-x customize-face RET xtdmacs-compile++-compiling-face RET```

Customize mode-line face when compile exited with error :
* ```M-x customize-face RET xtdmacs-compile++-error-face RET```


Set commands for a specific project :
```lisp
cat ~/.dir-locals.el
("dev/myproject/"
  . ((nil
    . ((xtdmacs-compile++-config-alist
       . (("compile"
          . (("dir"        . xtdmacs-compile++-get-dir-git)
             ("get-params" . (lambda() (xtdmacs-compile++-docker-params "compile")))
             ("command"    . (lambda() (xtdmacs-compile++-docker-run-command "compile")))
             ("env"        . "")
             ("bin"        . "make -j 12")
             ("service"    . "ws-compile")))
          ("test"
          . (("dir"        . xtdmacs-compile++-get-dir-git)
             ("get-params" . (lambda() (xtdmacs-compile++-docker-params "test")))
             ("command"    . (lambda() (xtdmacs-compile++-docker-run-command "test" "exec")))
             ("env"        . "")
             ("bin" . "bash -c 'cd /build && make test'")
             ("service"    . "ws-rt")))
          ("deploy"
          . (("dir"        . xtdmacs-compile++-get-dir-git)
             ("get-params" . (lambda() (xtdmacs-compile++-docker-params "deploy")))
             ("command"    . (lambda() (xtdmacs-compile++-docker-run-command "deploy" "exec")))
             ("env"        . "")
             ("bin" . "bash -c 'cd /build && sudo -E make install_all'")
             ("service"    . "ws-rt")))))))))
```



### Compile Bindings


| Key                           | Effect                                                                   |
|-------------------------------|--------------------------------------------------------------------------|
| \<F6\>                        | run xtdmacs-compile++-command-1 command (*compile*)                      |
| \<ctrl\>+\<F6\>               | run xtdmacs-compile++-command-1 command (*compile*), interactive version |
| \<F7\>                        | run xtdmacs-compile++-command-2 command (*test*)                         |
| \<ctrl\>+\<F7\>               | run xtdmacs-compile++-command-2 command (*test*), interactive version    |
| \<F8\>                        | run xtdmacs-compile++-command-3 command (*deploy*)                       |
| \<ctrl\>+\<F8\>               | run xtdmacs-compile++-command-3 command (*deploy*), interactive version  |
| \<F8\>                        | run xtdmacs-compile++-command-3 command (*deploy*)                       |
| \<ctrl\>+\<shift\>+\<F6\>     | run xtdmacs-compile++-command-4 command (*deploy*)                       |
| \<ctrl\>+\<shift\>+\<F7\>     | run xtdmacs-compile++-command-5 command (*iwyu*)                         |
| \<alt\>+\<F6\>                | kill running process                                                     |
| \<alt\>+\<F7\>                | kill running process                                                     |
| \<alt\>+\<F8\>                | kill running process                                                     |
| \<F9>                         | goto next compile error                                                  |
| \<ctrl\>+\<F9>                | goto next compile error or warning                                       |


## xtdmacs-code-doxygen-mode

This minor mode provides an enhanced initialization of doxymacs minor mode.

### Templates

It defines two standard documentation templates :

- ```xtdmacs-code-doxymacs-template-doxystyle``` : function documentation layout suitable
  for doxygen

- ```xtdmacs-code-doxymacs-template-phpdoc`` : function documentation layout suitable
  for phpdoc

Doxymacs' current template is defined by the variable
```doxymacs-function-comment-template```, its default value is
 ```xtdmacs-code-doxymacs-template-doxystyle``` .

### Keyword and faces

The defines font lock keywords for doxygen style documentation through the
customizable variable ```xtdmacs-code-doxymacs-keywords-alist```. The defined
keywords handle new doxygen markdown compatibility style.

Defined keywords and arguments are fontified with faces defined in the group
```code-doxymacs``` .

To modify the faces :

```M-x customize-group RET code-doxymacs RET```

Example:

![xtdmacs-code-doxymacs-mode](doc/code-doxymacs.png "xtdmacs-code-doxymacs-mode")


### Doxymacs Bindings

| Key                           | Effect                                         |
|-------------------------------|------------------------------------------------|
| \<ctrl\>+x d                  | insert function/method comment at point        |
| \<ctrl\>+x m                  | insert member variable comment at point        |

## xtdmacs-code-line-mode

This minor mode tweaks the ```mode-line``` format in order to display :
- the ```buffer name``` with the customizable face ```mode-line-buffer-id```
- ```line``` and ```column``` of current point position
- the ```percentage``` of the current buffer
- the current ```function name```, if any, or the current ```buffer directory```

The function name is deduces by ```which-func-mode``` which is customizable with
the following command:

```C-u M-x customize-mode RET which-func-mode RET```


Example:

![xtdmacs-code-line-mode](doc/code-line-mode.png "xtdmacs-code-line-mode")


## xtdmacs-code-spell-mode & xtdmacs-code-spell-prog-mode


This modes are wrapping of ```flyspell-mode``` and ``flyspell-prog-mode```. They
both detected spelling error in current buffer. The first analyzes all available
text and the second only analyzes strings and comment.

### Spell Configuration

The modes are affected by the following customizable variables :

- ```M-x customize-variable RET xtdmacs-code-spell-ignore-regexp RET``` : list of
  regexp patterns to ignore while spelling the buffer.

- ```M-x customize-variable RET xtdmacs-code-spell-max-lines RET``` : maximum
  allowed buffer lines to automatically run flyspell on buffer.

- ```M-x customize-variable RET ispell-local-dictionary RET``` : default spelling
  dictionary


### Spell Faces

The following faces are used by underlying flyspell mode :

- ```M-x customize-face RET flyspell-incorrect RET``` : Face to display detected
  spelling errors

- ```M-x customize-face RET flyspell-duplicate RET``` : Face to display detected
  duplicated words.


### Spell API

Useful functions :

- ```M-X flyspell-buffer RET``` : refresh spelling analysis of current buffer

- ```M-X xtdmacs-code-spell-change-dictionary RET``` : changes spelling dictionary
  and set new dictionary are local file variable.

- ```M-X xtdmacs-code-spell-next-word RET``` : interactively correct the next
  detected error

- ```M-X xtdmacs-code-spell-prev-word RET``` : interactively correct the previous
  detected error

### Spell Bindings

| Key                           | Effect                                     |
|-------------------------------|--------------------------------------------|
| \<ctrl\>+c \<ctrl\>+c         | ```xtdmacs-code-spell-change-dictionary``` |
| \<ctrl\>+c \<ctrl\>+\<down\>  | ```flyspell-buffer```                      |
| \<ctrl\>+c \<ctrl\>+\<right\> | ```xtdmacs-code-spell-next-word```         |
| \<ctrl\>+c \<ctrl\>+\<left\>  | ```xtdmacs-code-spell-prev-word```         |


![xtdmacs-code-spell-mode](doc/code-spell-mode.png "xtdmacs-code-spell-mode")

# Language specific modes

## xtdmacs-code-cpp-mode

This minor modes provides the following features :

- **Fix -std=c++11 enum class** : Aging c++-mode doesn't handle new enum
  class syntax available in c++11 and leads to a broken indentation. This minor
  mode fixes ```c-offsets-alist``` and properly indents this structure.

- **Cycling through headers and implementation files** : When editing a c++ header
  (.hh), we often need to visit the corresponding implementation (.cc) and vice
  versa. The mode defines a function that searches for file that matches current
  buffer file name with the correct extension. Another function does the same but
  creates the file if it doesn't already exist. Because c++ extensions are not
  well standardized, you can set the list of searched extension in the variable
  ```xtdmacs-code-cpp-header-extensions``` .

- **Automatic indentation** : The mode offers to automatically indent
  the whole buffer at open and/or at close. (*)

  (*) Personnal note : Emacs is the best market product for editing and indenting
  code. Sadly, not everybody uses Emacs and real world code is often poorly indented.
  This usage is certainly highly arguable but I've been using this in industrial
  collaborative environment for the past ten years and automatic code indentation
  solved far more problems than it has created.

- **Keywords** : The mode provides many font-lock additional keywords. Some of them
  try to catch the new C++11/14 language keywords like ```nullptr``` or
  ```decltype``` or ```utf-8 strings``` . Others define font-lock rules to color
  particular naming patterns, allowing to easily distinguish local variables,
  parameter, class members, const and static attributes without need to use
  heavy syntax analyzers that often need to actually compile the code.

- **Renaming variables** : The mode defines a function that generated the correct
  ```query-replace-regexp``` call to rename symbol at point to match one of the
  prefix rule defined for local variable, parameters or class member syntax
  coloring.

### C++ Configuration

- ```M-x customize-variable RET xtdmacs-code-cpp-indent-load-auto RET``` : tells
  if buffer should be automatically indented at load.

- ```M-x customize-variable RET xtdmacs-code-cpp-indent-save-auto RET``` :  tells
  if buffer should be automatically indented at save.

- ```M-x customize-variable RET xtdmacs-code-cpp-header-extensions RET``` : defines
  the list of extensions that are searched when cycling through headers and
  implementation files. Note: this list can have more than two elements, this is
  useful to handle template implementations or inline definition files like
  ```.hpp``` or ```.hxx``` .

- ```M-x customize-variable RET xtdmacs-code-cpp-keywords-alist RET``` : alist of
  keywords and faces to add to font-lock when mode is activated.

### C++ Faces

The mode uses faces defined in ```xtdmacs-code-mode```. See
```M-x customize-group RET code RET``` .

### C++ API

- ```M-x xtdmacs-code-cpp-header-cycle RET``` : cycle through extensions defined
  by ```xtdmacs-code-cpp-header-extensions``` .

- ```M-x xtdmacs-code-cpp-header-cycle-create RET``` : cycle through extensions defined
  by ```xtdmacs-code-cpp-header-extensions```, create files if they don't exist.

- ```M-x xtdmacs-code-cpp-rename-variable RET``` : rename variable under cursor. The
  function prompt interactively for renaming prefix.

### C++ Bindings

| Key                           | Effect                                                             |
|-------------------------------|--------------------------------------------------------------------|
| \<F12\>                       | ```xtdmacs-code-cpp-header-cycle```                                |
| \<ctrl\>+\<F12\>              | ```xtdmacs-code-cpp-header-cycle``` (create file if dosen't exist) |
| \<ctrl\>+e                    | ```xtdmacs-code-cpp-rename-variable```                             |

## xtdmacs-code-python-mode

This module provides the following features :

- Automatic indentation on load and/or save

- Overrides default xtdmacs-compile++ configuration :
  - run pylint on current buffer
  - run unittest script

- Defines font-lock keywords to identify local variables, parameters and class
  members

- Defines useful functions used in compilation commands

### Python Faces

The mode uses faces defined in ```xtdmacs-code-mode```.

### Python API

- ```xtdmacs-code-python-module-root``` : Returns buffers' most distant parent directory
  containing a ```___init__.py``` file. If no such file found, returns buffer's
  file directory.

- ```xtdmacs-code-python-project-root``` : Returns parent directory of module root. If
  module root couldn't be identified, returns buffer's file directory.

- ```xtdmacs-code-python-pylint-getargs``` : Constructs argument string to pass to
  compile command. If ```.pylintrc``` is found in project root, includes
  ```--rcfile=file``` in constructed string.

- ```xtdmacs-code-python-pylint-bin``` : Constructs compile command from
  ```xtdmacs-code-python-pylint-bin-path``` and
  ```xtdmacs-code-python-pylint-args``` . The buffer's file path is added as last
  argument on the returned command.

- ```xtdmacs-code-python-test-bin```  Constructs compile command from
  ```xtdmacs-code-python-test-bin-path``` and
  ```xtdmacs-code-python-test-args``` .

- ```xtdmacs-code-python-params``` : same as ```xtdmacs-compile++-default-params```,
  prompt only for directory and binary command.

- ```xtdmacs-code-python-command``` : same as ```xtdmacs-compile++-default-command```,
  construct final compile command from parameters built by
  ```xtdmacs-code-python-params``` .

### Python Configuration

- ```M-x customize-variable RET xtdmacs-code-python-pylint-bin-path RET```
  - pylint static code checker file path

- ```M-x customize-variable RET xtdmacs-code-python-pylint-args RET```
  - Static string or function to use as pylint script argument

- ```M-x customize-variable RET xtdmacs-code-python-test-bin-path RET```
  - Unit test runner file path. If nil, use default xtdmacs runner

- ```M-x customize-variable RET xtdmacs-code-python-test-args RET```
  - Static string or function to use as test binary arguments

- ```M-x customize-variable RET xtdmacs-code-python-indent-save-auto RET```
  - Enables python code auto-indentation on save.

- ```M-x customize-variable RET xtdmacs-code-python-indent-load-auto RET```
  - Enables python code auto-indentation on load.

- ```M-x customize-variable RET xtdmacs-code-python-keywords-alist RET```
  - List of additional python font-lock keywords

- ```M-x customize-variable RET xtdmacs-code-python-compile-alist RET```
  - overrides ```xtdmacs-compile++-config-alist```



## xtdmacs-code-php-mode

This minor mode provides de following features :
- Fix anonymous function indentation introduced in PHP 5.3.0

- Sets default doxymacs comment template ```doxymacs-function-comment-template``` to
  to phpdoc compatible ```xtdmacs-code-doxymacs-template-phpdoc``` .

- Adds font-lock keywords to identify local variables, parameters and class members

- Fixes syntax table for a better work boundary detection

- Automatic indentation on buffer load and/or save

### Php Faces

In addition to faces defined in ```xtdmacs-code-mode```. (See
```M-x customize-group RET code RET``` ), the mode defines :

- ```M-x customize-face RET xtdmacs-code-php-operator RET``` : Used to fontify PHP
  language operators such as ';' or '::'"


### Php Configuration

- ```M-x customize-variable RET xtdmacs-code-php-indent-load-auto RET``` : Enables
  code auto-indentation on buffer load.

- ```M-x customize-variable RET xtdmacs-code-php-indent-save-auto RET``` : Enables
  code auto-indentation on buffer save.


## xtdmacs-code-lisp-mode

### Configuration

- ```M-x customize-variable RET xtdmacs-code-lisp-indent-load-auto RET``` : Enables
  code auto-indentation on buffer load.

- ```M-x customize-variable RET xtdmacs-code-lisp-indent-save-auto RET``` : Enables
  code auto-indentation on buffer save.

## xtdmacs-code-json-mode

This mode loads ```yafolding-mode``` and binds a key to ```yafolding-toggle-element```.
It also sets ```js-indent-level``` to 2.

### Json Bindings

| Key                           | Effect                              |
|-------------------------------|-------------------------------------|
| \<ctrl\>+c \<ctrl\>+f         | toggle node under cursor            |


## xtdmacs-code-web-mode

The modes override default ```comment-start``` and ```comment-end``` that are
poorly set by ```web-mode```.

## xtdmacs-code-makefile-mode

Highlights tabs with ```hi-yellow``` face.

## xtdmacs-code-java-mode

This mode simple adds font-lock keywords :
```M-x customize-variable RET xtdmacs-code-java-keywords-alist RET```

## xtdmacs-code-js-mode

This mode simple adds font-lock keywords :
```M-x customize-variable RET xtdmacs-code-js-keywords-alist RET```

## xtdmacs-code-sphinx-mode

This module overrides default xtdmacs-compile++ configuration :
  - search for directory containing conf.py as compile directory
  - detects compile command as follow :
    - ```make html``` when compile directory has a Makefile
    - ```sphinx-build -M html . build``` otherwise

It also turns off electric-indent-mode which appear to not work very properly
with reStructuredText.

<!-- LocalWords:  xtdmacs config alist RET params cd dir env API dev toc wget -->
<!-- LocalWords:  param filename automake's VPATH sudo ctrl goto xvzf ido fci -->
<!-- LocalWords:  swbuff multi linum doxymacs flyspell reStructuredText -->
<!-- Local Variables: -->
<!-- ispell-local-dictionary: "american" -->
<!-- End: -->
