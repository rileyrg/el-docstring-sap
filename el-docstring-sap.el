;;; el-docstring-sap.el --- A minor-mode display docstrings for the symbol at point
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
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defvar el-docstring-sap--timer nil  "Store the `el-docstring-sap-mode' timer." )
(defvar el-docstring-sap--lastsym nil  "Don't idle-repeat the same symbol twice in a row.")

(require 'use-package)

(defun el-docstring-sap--describe-symbol(&optional _docstring sym)

  "Use the internal `describe-symbol' to show help for symbol SYM."
  (if sym
      (describe-symbol sym)
    (let ((hw (get-buffer-window (help-buffer))))
      (when (and hw (bound-and-true-p el-docstring-sap--auto-hide-describe-symbol-window))
        (delete-window hw)))))

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

(use-package posframe
  :commands (posframe-hide posframe-show el-docstring-sap--posframe)
  :init
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
                           :position (if (bound-and-true-p el-docstring-sap--posframe-poshandler) t p))))))))

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
;;; el-docstring-sap.el ends here
