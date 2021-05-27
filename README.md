- [el-docstring-sap.el Auto display elisp docstring for symbol at point](#org197d70e)
  - [Enabling the minor mode](#orgd0f1de0)
  - [Manually invoke `el-docstring-el-display`](#org680c27a)
  - [Alternatives to `posframe` for displaying the docstring](#orgd2fd183)
    - [quick-peek](#org12079c2)
    - [popup](#org110dc92)
    - [`describe-symbol`](#org846efcc)
  - [Programmatically set your display function](#org9f03615)
  - [Interactively choose  the docstring display function.](#orgcc21fa6)
  - [`use-package` usage example](#org036778a)
  - [Customisation](#org02ab7cf)
    - [`el-docstring-sap--delay`](#orgc3403e5)
    - [`el-docstring-sap--display-func`](#org510347a)
    - [`el-docstring-sap--save-history`](#orga6aada4)



<a id="org197d70e"></a>

# el-docstring-sap.el Auto display elisp docstring for symbol at point

[el-docstring-sap](./el-docstring-at-point.el) defines a minor mode `el-docstring-sap-mode` which enables a "popup" displaying the elisp docstring for the symbol at point after a [configured delay](#orgc3403e5). Typically you add `el-docstring-sap-mode` to `emacs-lisp-mode-hook` to enable.

The package assumes [posframe](https://github.com/tumashu/posframe) is available and falls back to it if you set `el-docstring-sap--display-func` to something that fails.


<a id="orgd0f1de0"></a>

## Enabling the minor mode

Typically simply add to your emacs-lisp-mode-hook.

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'el-docstring-sap-mode)
```


<a id="org680c27a"></a>

## Manually invoke `el-docstring-el-display`

Bind a key to `el-docstring-sap-display` to call regardless of mode. Note, that if `el-docstring-sap-mode` isnt on, the displayed docstring wont auto disappear when you type, instead invoke again with the cursor on whitespace to turn off the display - this is by design.

```emacs-lisp
(global-set-key (kbd "M-<f2>") #'el-docstring-sap-display)
```


<a id="orgd2fd183"></a>

## Alternatives to `posframe` for displaying the docstring

You need to explicitly install the libraries for `quick-peek` or `popup` if you want to use them else `el-docstring-sap--popup` or `el-docstring-sap--quick-peek` will fall back to `el-docstring-sap--posframe` after failing to load the required library.


<a id="org12079c2"></a>

### quick-peek

<https://github.com/cpitclaudel/quick-peek>


<a id="org110dc92"></a>

### popup

<https://github.com/kzk/elisp/blob/master/m/auto-complete/popup.el>


<a id="org846efcc"></a>

### `describe-symbol`

You can set `el-docstring-sap-display-func` to `el-docstring-sap--describe-symbol` for standard help buffer viewing. See [display function customisation](#org510347a) below.


<a id="org9f03615"></a>

## Programmatically set your display function

`(setq el-docstring-sap--display-func 'el-docstring-sap--quick-peek)`

The display function accepts a non zero length string to be displayed. If nil or empty string then erase the last display. See `el-docstring-sap--display-funcs`.


<a id="orgcc21fa6"></a>

## Interactively choose  the docstring display function.

`el-docstring-sap-select-display-func` sets `el-docstring-sap--display-func`. Not permanent. Bring up the custom interface to save it. (`C-h v el-docstring-sap--display-func`).


<a id="org036778a"></a>

## `use-package` usage example

```emacs-lisp
(use-package el-docstring-sap
  :straight (el-docstring-sap :type git :host github :repo "rileyrg/el-docstring-sap" )
  :custom  (el-docstring-sap--display-func 'el-docstring-sap--posframe)
  :hook
  (emacs-lisp-mode . el-docstring-sap-mode)
  :bind
  ("M-<f2>" . el-docstring-sap-display)
  ("M-<f1>" . el-docstring-sap-mode))
```


<a id="org02ab7cf"></a>

## Customisation

Customisation group `el-docstring-sap`


<a id="orgc3403e5"></a>

### `el-docstring-sap--delay`

Delay before docstring for symbol at point


<a id="org510347a"></a>

### `el-docstring-sap--display-func`

Function that takes a string. If nil or empty then hide previous. See `el-docstring-sap--display-funcs` for candidates.

-   `el-docstring-sap--posframe`

    -   `el-docstring-sap--posframe-poshandler`

        The poshandler that `posframe-show` uses-

    -   `el-docstring-sap--posframe-arghandler-plist`

        Customise default values for posframe display.

    -   Example screenshot

        ![img](images/el-docstring-sap--posframe.png "elisp docstring posframe")

-   `el-docstring-sap--describe-symbol`

    -   Example screenshot

        ![img](images/el-docstring-sap--describe-symbol.png "elisp docstring describe-symbol")

-   `el-docstring-sap--quick-peek`

    -   Example screenshot

        ![img](images/el-docstring-sap--quick-peek.png "elisp docstring quick-peek")

-   `el-docstring-sap--popup`

    -   Example screenshot

        ![img](images/el-docstring-sap--popup.png "elisp docstring popup")


<a id="orga6aada4"></a>

### `el-docstring-sap--save-history`

**<span class="underline">Not Used currently</span>**.

When non-nil save the symbol queried to `el-docstring-sap--history`