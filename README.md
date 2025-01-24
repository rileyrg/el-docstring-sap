
# Table of Contents

-   [Introduction](#orgbffa372)
-   [Installation](#org6bff164)
    -   [use-package](#org05b0e9d)
-   [Configuration](#org140bf32)
    -   [Enabling the minor mode](#org9739f77)
    -   [Manually invoke `el-docstring-el-display`](#org2df78ef)
    -   [Alternatives to `posframe` for displaying the docstring](#org3d9a9fe)
        -   [quick-peek](#org4222cd7)
        -   [popup](#org7be6ab4)
        -   [`describe-symbol`](#orgd21c724)
    -   [Programmatically set your display function](#orgc9c8e3d)
    -   [Interactively choose  the docstring display function.](#org9486e1d)
-   [Customisation](#org0d8646d)
        -   [`el-docstring-sap--delay`](#org721aaa4)
        -   [`el-docstring-sap--display-func`](#org1a56f51)
        -   [`el-docstring-sap--save-history`](#org2c289b2)
-   [el-docstring-sap.el](#org09fa342)
    -   [header](#org0737d60)
    -   [customisation group](#org42cf21e)
    -   [main code (to be further broken down)](#org36b2792)



<a id="orgbffa372"></a>

# Introduction

Auto display elisp docstring for symbol at point

link: [el-docstring-sap.el](el-docstring-sap.el)

[el-docstring-sap](./el-docstring-at-point.el) defines a minor mode `el-docstring-sap-mode` which enables a "popup" displaying the elisp docstring for the symbol at point after a [configured delay](#org721aaa4).  Typically you add `el-docstring-sap-mode` to `emacs-lisp-mode-hook` to enable.

The package assumes [posframe](https://github.com/tumashu/posframe) is available and falls back to it if you set `el-docstring-sap--display-func` to
something that fails.


<a id="org6bff164"></a>

# Installation


<a id="org05b0e9d"></a>

## use-package

    (use-package el-docstring-sap
      :straight (el-docstring-sap :type git :host github :repo "rileyrg/el-docstring-sap" )
      :custom  (el-docstring-sap--display-func 'el-docstring-sap--posframe)
      :hook
      (emacs-lisp-mode . el-docstring-sap-mode)
      :bind
      ("M-<f2>" . el-docstring-sap-display)
      ("M-<f1>" . el-docstring-sap-mode))


<a id="org140bf32"></a>

# Configuration


<a id="org9739f77"></a>

## Enabling the minor mode

Typically simply add to your emacs-lisp-mode-hook.

    (add-hook 'emacs-lisp-mode-hook 'el-docstring-sap-mode)


<a id="org2df78ef"></a>

## Manually invoke `el-docstring-el-display`

Bind a key to `el-docstring-sap-display` to call regardless of mode. Note, that if `el-docstring-sap-mode` isnt on, the displayed docstring wont auto disappear when you type, instead invoke again with the cursor on whitespace to turn off the display - this is by design.

    (global-set-key (kbd "M-<f2>") #'el-docstring-sap-display)


<a id="org3d9a9fe"></a>

## Alternatives to `posframe` for displaying the docstring

You need to explicitly  install the  libraries for `quick-peek` or `popup` if you want to use them else `el-docstring-sap--popup` or
`el-docstring-sap--quick-peek` will fall back to `el-docstring-sap--posframe` after failing to load the required library.


<a id="org4222cd7"></a>

### quick-peek

<https://github.com/cpitclaudel/quick-peek>


<a id="org7be6ab4"></a>

### popup

<https://github.com/kzk/elisp/blob/master/m/auto-complete/popup.el>


<a id="orgd21c724"></a>

### `describe-symbol`

You can set `el-docstring-sap-display-func` to `el-docstring-sap--describe-symbol` for standard help buffer viewing. See [display function customisation](#org1a56f51) below.


<a id="orgc9c8e3d"></a>

## Programmatically set your display function

`(setq el-docstring-sap--display-func 'el-docstring-sap--quick-peek)`

The display function accepts a non zero length string to be displayed. If nil or empty string then erase the last display. See `el-docstring-sap--display-funcs`.


<a id="org9486e1d"></a>

## Interactively choose  the docstring display function.

`el-docstring-sap-select-display-func` sets `el-docstring-sap--display-func`.
Not permanent. Bring up the custom interface to save it. (`C-h v el-docstring-sap--display-func`).


<a id="org0d8646d"></a>

# Customisation

Customisation group `el-docstring-sap`


<a id="org721aaa4"></a>

### `el-docstring-sap--delay`

Delay before docstring for symbol at  point


<a id="org1a56f51"></a>

### `el-docstring-sap--display-func`

Function that takes a string. If nil or empty then hide previous.
See `el-docstring-sap--display-funcs` for candidates.

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


<a id="org2c289b2"></a>

### `el-docstring-sap--save-history`

**<span class="underline">Not Used currently</span>**.

When non-nil save the symbol queried to `el-docstring-sap--history`


<a id="org09fa342"></a>

# el-docstring-sap.el

link: [el-docstring-sap.el](el-docstring-sap.el)


<a id="org0737d60"></a>

## header

    ;;; el-docstring-sap.el --- A minor-mode display docstrings for the symbol at point
    ;;
    ;; maintained in el-docstring-sap.org
    ;;
    ;; Copyright (C) 2010-2021 rileyrg
    ;;
    ;; Author: rileyrg <rileyrg@gmx.de>
    ;; Created: 22 April 2021
    ;; Keywords: internal lisp docs help maint tools
    ;; Version : 1.0
    ;; Package-Requires: ((emacs "25.1") (posframe "1.0.1"))
    ;; Optional :  ((quick-peek "1.0") (popup "0.5.8"))
    ;; URL: https://github.com/rileyrg/el-docstring-sap
    ;;;
    
    ;;; commentary:
    ;;
    ;; Enable `el-docstring-sap-mode' to have docstrings auto generated using the function
    ;; referenced by `el-docstring-sap--display-func'.  Defaults to `el-docstring-sap--posframe'.
    ;;
    ;; Usage example:
    ;; (use-package el-docstring-sap
    ;;   :straight (el-docstring-sap :type git :host github :repo "rileyrg/el-docstring-sap" )
    ;;   :custom  (el-docstring-sap--display-func 'el-docstring-sap--posframe)
    ;;   :hook
    ;;   (emacs-lisp-mode . el-docstring-sap-mode)
    ;;   :bind
    ;;   ("M-<f2>" . 'el-docstring-sap-display)
    ;;   ("M-<f1>" . 'el-docstring-sap-mode))
    ;;
    ;; Select display function:-
    ;; M-x: el-docstring-sap-select-display-func
    
    ;;
    ;;; code:


<a id="org42cf21e"></a>

## customisation group

    (defgroup  el-docstring-sap nil "Customisation options for `el-docstring-sap-mode'." :group 'rgr)
    
    (defcustom el-docstring-sap--delay 2.5 "How long to delay before `el-docstring-sap--display-func' is called." :type 'float)
    
    (defcustom el-docstring-sap--save-history t "When non-nil store each search popup to `el-docstring-sap--history'." :type 'boolean)
    (defvar el-docstring-sap--history nil "Store history of docstring references.")
    
    (require 'savehist)
    (add-to-list 'savehist-additional-variables 'el-docstring-sap--history)
    
    (defcustom el-docstring-sap--lighter " SapDoc" "Modeline indicator for `el-docstring-sap-mode'." :type 'string)
    
    (defcustom el-docstring-sap--display-funcs  '(el-docstring-sap--posframe el-docstring-sap--quick-peek el-docstring-sap--popup el-docstring-sap--describe-symbol)
      "Functions to provide `el-docstring-sap-mode' display." :type '(repeat function))
    
    (defcustom el-docstring-sap--display-func 'el-docstring-sap--posframe "The function to display a docstring for symbol at point." :type
      `(choice :value el-docstring-sap--posframe (const :tag "default(posframe)" 'el-docstring-sap--posframe)
               ,(append '(radio :tag "Supported docstring display functions") (mapcar (lambda(e)(cons 'function-item (cons e nil))) el-docstring-sap--display-funcs ))))


<a id="org36b2792"></a>

## TODO main code (to be further broken down)

    (use-package posframe
      :commands (posframe-hide posframe-show))
    
     (defvar el-docstring-sap--timer nil  "Store the `el-docstring-sap-mode' timer." )
     (defvar el-docstring-sap--lastsym nil  "Don't idle-repeat the same symbol twice in a row.")
    
    ;;;###autoload
     (defun el-docstring-sap--describe-symbol(&optional _docstring sym)
    
       "Use the internal `describe-symbol' to show help for symbol SYM."
       (if sym
           (describe-symbol sym)
         (let ((hw (get-buffer-window (help-buffer))))
           (when (and hw (bound-and-true-p el-docstring-sap--auto-hide-describe-symbol-window))
             (delete-window hw)))))
      ;;;###autoload
     (defun el-docstring-sap--docstring(sym)
       "Return the docstring attached to the symbol SYM.  If SYM has no docstring, return nil."
       (if sym
           (let ((docstring
                  (if (or (fboundp sym) (boundp sym))
                      (let ((help-xref-following t))
                        (save-window-excursion
                          (with-temp-buffer
                            (help-mode)
                            (describe-symbol sym)
                            (buffer-string))))
                    nil)))
             (let((gd  (get sym 'group-documentation)))
               (when gd
                 (setq docstring (concat docstring "\n----\ndefgroup " (symbol-name sym) ":\n" gd))))
             (when (facep sym)
               (setq docstring (concat docstring "\n----\nface " (symbol-name sym) ":\n" (get sym 'face-documentation))))
             (if (eq (length docstring) 0)
                 nil
               docstring))
         nil))
    
     (defun el-docstring-sap--display-fail(&optional docstring)
       "Inform an error occured and revert to dislpaying DOCSTRING with `el-docstring-sap--posframe."
       (message "Function  `%s' failed. Library loaded? Reverting to `el-docstring-sap--posframe'." el-docstring-sap--display-func)
       (setq el-docstring-sap--display-func 'el-docstring-sap--posframe)
       (el-docstring-sap--posframe docstring))
    
     (defun el-docstring-sap--hide()
       "Hide the the elisp docstring."
       (funcall el-docstring-sap--display-func nil nil))
    
     (defun el-docstring-sap--timer-func()
       "Function called every `el-docstring-sap--delay' seconds when `el-docstring-sap-mode' is non-nil."
       (when (bound-and-true-p el-docstring-sap-mode)
         (let ((sym (symbol-at-point)))
           (el-docstring-sap-display sym))))
    
     ;;;###autoload
     (defun el-docstring-sap-display(&optional sym)
       "Display docstring for optional SYM, defaulting to `symbol-at-point', using `el-docstring-sap--display-func'."
       (interactive)
       (when (called-interactively-p 'any)
         ;; since there's not a pre-action hook to clean it in many cases
         (el-docstring-sap--hide))
       (let((sym (if sym sym (symbol-at-point))))
         (if (not sym)
             (when (called-interactively-p 'any)
               (message "No symbol at point."))
           ;; only display if we specifically asked for it or it wasnt displayed
           ;; in the last idle timer popup
           (when (or (called-interactively-p 'any) (not (eq sym el-docstring-sap--lastsym)))
             (let ((docstring (el-docstring-sap--docstring sym)))
               (if docstring
                   (progn
                     (setq el-docstring-sap--lastsym sym)
                     (when el-docstring-sap--save-history
                       (add-to-history 'el-docstring-sap--history sym))
                     (save-excursion
                       (condition-case nil
                           (funcall el-docstring-sap--display-func  docstring sym)
                         (error (el-docstring-sap--display-fail docstring )))))))))))
    
     ;;;###autoload
     (defun el-docstring-sap-select-display-func()
       "Select a `el-docstring-sap-mode' display function from `el-docstring-sap--display-funcs'."
       (interactive)
       (let* ((fl (mapcar (lambda(s)(cons (documentation s) s)) el-docstring-sap--display-funcs))
              (f (alist-get (completing-read "function: " fl) fl nil nil #'equal)))
         (when f
           (setq el-docstring-sap--lastsym nil)
           (setq el-docstring-sap--display-func f))))
    
     ;;;###autoload
     (define-minor-mode el-docstring-sap-mode
       "minor-mode to popup help for the elisp symbol at point."
       :lighter el-docstring-sap--lighter
       (if (bound-and-true-p el-docstring-sap-mode)
           (add-hook 'pre-command-hook 'el-docstring-sap--hide nil t)
         (remove-hook 'pre-command-hook 'el-docstring-sap--hide t))
       (el-docstring-sap--hide)
       (unless el-docstring-sap--timer
         (setq  el-docstring-sap--timer
                (run-with-idle-timer
                 el-docstring-sap--delay t
                 'el-docstring-sap--timer-func))))
    
     (defcustom el-docstring-sap--posframe-poshandler  nil "select the PosFrame :poshandler."
       :type '(choice
               (const :tag "Show docstring at point." nil)
               (radio :tag "posframe :poshandler"
                      (function-item posframe-poshandler-frame-top-left-corner)
                      (function-item posframe-poshandler-frame-top-center)
                      (function-item posframe-poshandler-frame-top-right-corner)
                      (function-item posframe-poshandler-frame-bottom-left-corner)
                      (function-item posframe-poshandler-frame-bottom-center)
                      (function-item posframe-poshandler-frame-bottom-right-corner)
                      (function-item posframe-poshandler-frame-center)
                      (function-item posframe-poshandler-window-top-left-corner)
                      (function-item posframe-poshandler-window-top-center)
                      (function-item posframe-poshandler-window-top-right-corner)
                      (function-item posframe-poshandler-window-bottom-left-corner)
                      (function-item posframe-poshandler-window-bottom-center)
                      (function-item posframe-poshandler-window-bottom-right-corner)
                      (function-item posframe-poshandler-window-center))))
    
     (defcustom el-docstring-sap--posframe-arghandler-plist  '(:width 80 :internal-border-width 2 :background-color "#303030" :border-width 2 :border-color "orange") "Posfame's fallback config plist." :type 'plist )
    
     (defun el-docstring-sap--posframe-arghandler (_buffer-or-name arg-name value)
       "Function to override posframe VALUE for ARG-NAME with customs from the plist `el-docstring-sap--posframe-arghandler-plist'"
       (let ((info el-docstring-sap--posframe-arghandler-plist))
         (or (plist-get info arg-name) value)))
    
    
     ;;;###autoload
     (defun el-docstring-sap--posframe(&optional docstring _sym)
       "`posframe-show' to display  DOCSTRING. Pass nil to erase."
       (interactive)
       (posframe-hide "*el-docstring*")
       (when docstring
         (let ((p (point))
               (posframe-arghandler 'el-docstring-sap--posframe-arghandler))
           (save-window-excursion
             (with-current-buffer
                 (get-buffer-create "*el-docstring*")
               (erase-buffer)
               (insert docstring)
               (posframe-show (current-buffer)
                              :string docstring
                              :poshandler el-docstring-sap--posframe-poshandler
                              :position (if (bound-and-true-p el-docstring-sap--posframe-poshandler) t p)))))))
    
     ;;;###autoload
     (defun el-docstring-sap--quick-peek(&optional docstring _sym)
       "`quick-peek-show' to display  DOCSTRING.  Pass nil to erase."
       (interactive)
       (condition-case nil
           (progn
             (quick-peek-hide)
             (when (not (featurep 'quick-peek))
               (use-package quick-peek :commands (quick-peek-hide quick-peek-show)))
             (when docstring
               (quick-peek-show docstring)))
         (error (el-docstring-sap--display-fail docstring))))
    
     ;;;###autoload
     (defun el-docstring-sap--popup(&optional docstring _sym)
       "`popup-tip' to display  DOCSTRING.  Pass nil to erase."
       (interactive)
       (condition-case nil
           (progn
             (when (not (featurep 'popup))
               (use-package popup :commands (popup-tip)))
             (when docstring
               (popup-tip docstring)))
         (error (el-docstring-sap--display-fail docstring))))
    
     (provide 'el-docstring-sap)

(require 'org-capture)

