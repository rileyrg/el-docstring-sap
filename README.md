- [el-docstring-sap.el Auto display elisp docstring for symbol at point](#org91936cf)
  - [Enabling the minor mode](#org2813dca)
  - [Manually invoke `el-docstring-el-display`](#orgbaef32b)
  - [Alternatives to `posframe` for displaying the docstring](#org448d576)
    - [quick-peek](#org25409a3)
    - [popup](#org14c11d0)
    - [`describe-symbol`](#orgb20305e)
  - [Programmatically set your display function](#orgb2c6b1a)
  - [Interactively choose  the docstring display function.](#org5412e9b)
  - [`use-package` usage example](#org6d9e530)
  - [Customisation](#orga3377c9)
    - [`el-docstring-sap--delay`](#org8b05592)
    - [`el-docstring-sap--display-func`](#org0b053b8)
    - [`el-docstring-sap--save-history`](#org430f3d7)



<a id="org91936cf"></a>

# el-docstring-sap.el Auto display elisp docstring for symbol at point

[el-docstring-sap](./el-docstring-at-point.el) defines a minor mode `el-docstring-sap-mode` which enables a "popup" displaying the elisp docstring for the symbol at point after a [configured delay](#org8b05592). Typically you add `el-docstring-sap-mode` to `emacs-lisp-mode-hook` to enable.

The package assumes [posframe](https://github.com/tumashu/posframe) is available and falls back to it if you set `el-docstring-sap--display-func` to something that fails.


<a id="org2813dca"></a>

## Enabling the minor mode

Typically simply add to your emacs-lisp-mode-hook.

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'el-docstring-sap-mode)
```


<a id="orgbaef32b"></a>

## Manually invoke `el-docstring-el-display`

Bind a key to `el-docstring-sap-display` to call regardless of mode. Note, that if `el-docstring-sap-mode` isnt on, the displayed docstring wont auto disappear when you type, instead invoke again with the cursor on whitespace to turn off the display - this is by design.

```emacs-lisp
(global-set-key (kbd "M-<f2>") #'el-docstring-sap-display)
```


<a id="org448d576"></a>

## Alternatives to `posframe` for displaying the docstring

You need to explicitly install the libraries for `quick-peek` or `popup` if you want to use them else `el-docstring-sap--popup` or `el-docstring-sap--quick-peek` will fall back to `el-docstring-sap--posframe` after failing to load the required library.

The custom options are still there for non loaded libraries in the defcustom for `el-docstring-sap--display-func` because I can't make head nor tail of the `defcustom` info pages to figure out how to dynamically construct a list of eligible functions. Educate me!


<a id="org25409a3"></a>

### quick-peek

<https://github.com/cpitclaudel/quick-peek>


<a id="org14c11d0"></a>

### popup

<https://github.com/kzk/elisp/blob/master/m/auto-complete/popup.el>


<a id="orgb20305e"></a>

### `describe-symbol`

You can set `el-docstring-sap-display-func` to `el-docstring-sap--describe-symbol` for standard help buffer viewing. See [display function customisation](#org0b053b8) below.


<a id="orgb2c6b1a"></a>

## Programmatically set your display function

`(setq el-docstring-sap--display-func 'el-docstring-sap--quick-peek)`

The display function accepts a non zero length string to be displayed. If nil or empty string then erase the last display. See `el-docstring-sap--display-funcs`.


<a id="org5412e9b"></a>

## Interactively choose  the docstring display function.

`el-docstring-sap-select-display-func` sets `el-docstring-sap--display-func`. Not permanent. Bring up the custom interface to save it. (`C-h v el-docstring-sap--display-func`).


<a id="org6d9e530"></a>

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


<a id="orga3377c9"></a>

## Customisation

Customisation group `el-docstring-sap`


<a id="org8b05592"></a>

### `el-docstring-sap--delay`

Delay before docstring for symbol at point


<a id="org0b053b8"></a>

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


<a id="org430f3d7"></a>

### `el-docstring-sap--save-history`

**<span class="underline">Not Used currently</span>**.

When non-nil save the symbol queried to `el-docstring-sap--history`
