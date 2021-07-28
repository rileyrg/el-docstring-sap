- [el-docstring-sap.el Auto display elisp docstring for symbol at point](#orgebc21e3)
  - [Enabling the minor mode](#orgd9ba6ff)
  - [Manually invoke `el-docstring-el-display`](#org895bbf3)
  - [Alternatives to `posframe` for displaying the docstring](#org7e81e76)
    - [quick-peek](#org3ee6b3b)
    - [popup](#org8562425)
    - [`describe-symbol`](#org31569d9)
  - [Programmatically set your display function](#org18d8d35)
  - [Interactively choose  the docstring display function.](#orgbdf7ff9)
  - [`use-package` usage example](#orgf5fa0a4)
  - [Customisation](#orgd833e94)
    - [`el-docstring-sap--delay`](#org7fa8cc4)
    - [`el-docstring-sap--display-func`](#org680da60)
    - [`el-docstring-sap--save-history`](#org19afeb5)



<a id="orgebc21e3"></a>

# el-docstring-sap.el Auto display elisp docstring for symbol at point

link: [el-docstring-sap.el](el-docstring-sap.el)

[el-docstring-sap](./el-docstring-at-point.el) defines a minor mode `el-docstring-sap-mode` which enables a "popup" displaying the elisp docstring for the symbol at point after a [configured delay](#org7fa8cc4). Typically you add `el-docstring-sap-mode` to `emacs-lisp-mode-hook` to enable.

The package assumes [posframe](https://github.com/tumashu/posframe) is available and falls back to it if you set `el-docstring-sap--display-func` to something that fails.


<a id="orgd9ba6ff"></a>

## Enabling the minor mode

Typically simply add to your emacs-lisp-mode-hook.

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'el-docstring-sap-mode)
```


<a id="org895bbf3"></a>

## Manually invoke `el-docstring-el-display`

Bind a key to `el-docstring-sap-display` to call regardless of mode. Note, that if `el-docstring-sap-mode` isnt on, the displayed docstring wont auto disappear when you type, instead invoke again with the cursor on whitespace to turn off the display - this is by design.

```emacs-lisp
(global-set-key (kbd "M-<f2>") #'el-docstring-sap-display)
```


<a id="org7e81e76"></a>

## Alternatives to `posframe` for displaying the docstring

You need to explicitly install the libraries for `quick-peek` or `popup` if you want to use them else `el-docstring-sap--popup` or `el-docstring-sap--quick-peek` will fall back to `el-docstring-sap--posframe` after failing to load the required library.


<a id="org3ee6b3b"></a>

### quick-peek

<https://github.com/cpitclaudel/quick-peek>


<a id="org8562425"></a>

### popup

<https://github.com/kzk/elisp/blob/master/m/auto-complete/popup.el>


<a id="org31569d9"></a>

### `describe-symbol`

You can set `el-docstring-sap-display-func` to `el-docstring-sap--describe-symbol` for standard help buffer viewing. See [display function customisation](#org680da60) below.


<a id="org18d8d35"></a>

## Programmatically set your display function

`(setq el-docstring-sap--display-func 'el-docstring-sap--quick-peek)`

The display function accepts a non zero length string to be displayed. If nil or empty string then erase the last display. See `el-docstring-sap--display-funcs`.


<a id="orgbdf7ff9"></a>

## Interactively choose  the docstring display function.

`el-docstring-sap-select-display-func` sets `el-docstring-sap--display-func`. Not permanent. Bring up the custom interface to save it. (`C-h v el-docstring-sap--display-func`).


<a id="orgf5fa0a4"></a>

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


<a id="orgd833e94"></a>

## Customisation

Customisation group `el-docstring-sap`


<a id="org7fa8cc4"></a>

### `el-docstring-sap--delay`

Delay before docstring for symbol at point


<a id="org680da60"></a>

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


<a id="org19afeb5"></a>

### `el-docstring-sap--save-history`

**<span class="underline">Not Used currently</span>**.

When non-nil save the symbol queried to `el-docstring-sap--history`