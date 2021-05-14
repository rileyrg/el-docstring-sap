
# Table of Contents



;;; el-docstring-sap.el &#x2014; A minor-mode display docstrings for the symbol at point -**- lexical-binding: t; -**-
;;
;; Copyright (C) 2010-2021 rileyrg
;;
;; Author: rileyrg <rileyrg@gmx.de>
;; Created: 22 April 2021
;; Keywords: internal lisp docs help maint tools
;; Version : 1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: <https://github.com/rileyrg/el-docstring-sap>
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
;; Enable \`el-docstring-sap-mode' to have docstrings auto generated using the function
;; referenced by \`el-docstring-sap&#x2013;display-func'.  Currently defaults to \`quick-peek'.
;;
;;; test:
;; (funcall (cadr(assoc (completing-read "function: " el-docstring-sap&#x2013;display-funcs) el-docstring-sap&#x2013;display-funcs)) "message")

;;
;;; custom:
(defgroup  el-docstring-sap nil "Customisation options for \`el-docstring-sap-mode'." :group 'rgr)

(defcustom el-docstring-sap&#x2013;delay 2.5 "How long to delay before \`el-docstring-sap&#x2013;display-func' is called." :type 'float)
(defcustom el-docstring-sap&#x2013;lighter " SapDoc" "Modeline indicator for \`el-docstring-sap-mode'" :type 'string)
(defcustom el-docstring-sap&#x2013;display-func  'el-docstring-sap&#x2013;quick-peek  "The function to display a docstring for symbol at point." :type 'function)
(defcustom el-docstring-sap&#x2013;display-funcs  nil  "Functions to provide \`el-docstring-sap-mode' display." :type 'alist )

;;; code:
;; Usage example:
;; (use-package el-docstring-sap
;;   :straight (el-docstring-sap :type git :host github :repo "rileyrg/el-docstring-sap" )
;;   :hook
;;   (emacs-lisp-mode . (lambda()(el-docstring-sap-mode +1)))
;;   :bind
;;   ("M-<f2>" . (lambda()(interactive)(el-docstring-sap&#x2013;display)))
;;   ("M-<f1>" . (lambda()(interactive)(el-docstring-sap-mode 'toggle))))
;;

(defvar el-docstring-sap&#x2013;timer nil  "Store the \`el-docstring-sap-mode' timer." )
(defvar el-docstring-sap&#x2013;lastsym nil  "Don't repeat the same popup.")

(require 'use-package)

(use-package quick-peek
  :demand
  :commands (quick-peek-hide quick-peek-show)
  :init
  (defun el-docstring-sap&#x2013;quick-peek(docstring)
    "\`quick-peek' displays the DOCSTRING for the \`symbol-at-point'."
    (interactive)
    (quick-peek-hide)
    (when docstring
      (quick-peek-show docstring)))
  (add-to-list 'el-docstring-sap&#x2013;display-funcs '("QuickPeek"  . el-docstring-sap&#x2013;quick-peek)))

(use-package popup
  :demand
  :commands (popup-tip)
  :init
  (defun el-docstring-sap&#x2013;popup(docstring)
    "\`popup-tip' displays the DOCSTRING for the \`symbol-at-point'."
    (interactive)
    (when docstring
      (popup-tip docstring)))
  (add-to-list 'el-docstring-sap&#x2013;display-funcs '("PopUp" .  el-docstring-sap&#x2013;popup)))

(use-package posframe
  :demand
  :commands (posframe-hide posframe-show)
  :init
  (defcustom el-docstring-sap&#x2013;posframe-poshandler  'posframe-poshandler-frame-top-right-corner "Non-nil means use this for the posframe poshandler else display at point." :type 'symbol :group 'el-docstring-sap)
  (defcustom el-docstring-sap&#x2013;posframe-arghandler-plist  '(:internal-border-width 2 :background-color "#303030" :border-width 2 :border-color "orange") "Posfame's fallback config plist" :type 'plist :group 'el-docstring-sap)

:config
(defun el-docstring-sap&#x2013;posframe-arghandler (buffer-or-name arg-name value)
  "Function to override posframe VALUE for ARG-NAME with customs from the plist \`el-docstring-sap&#x2013;posframe-arghandler-plist'"
  (ignore buffer-or-name)
  (let ((info el-docstring-sap&#x2013;posframe-arghandler-plist))
    (or (plist-get info arg-name) value)))

(defun el-docstring-sap&#x2013;posframe(docstring)
  "\`posframe-show' displays the DOCSTRING for the \`symbol-at-point'."
  (interactive)
  (posframe-hide "**el-docstring**")
  (when docstring
    (let ((p (point))
          (posframe-arghandler #'el-docstring-sap&#x2013;posframe-arghandler))
      (save-window-excursion
        (with-current-buffer
            (get-buffer-create "**el-docstring**")
          (erase-buffer)
          (insert docstring)
          (posframe-show (current-buffer)
                         :string docstring
                         :poshandler el-docstring-sap&#x2013;posframe-poshandler
                         :position (if el-docstring-sap&#x2013;posframe-poshandler t p)))))))
  (add-to-list 'el-docstring-sap&#x2013;display-funcs '("PosFrame" . el-docstring-sap&#x2013;posframe)))

(defun el-docstring-sap&#x2013;docstring(sym)
  "Return the docstring attached to the symbol SYM."
  (let ((docstring
         (if (or (fboundp sym) (boundp sym))
             (let ((help-xref-following t))
               (save-window-excursion
                 (with-temp-buffer
                  (help-mode)
                  (describe-symbol sym)
                  (buffer-string))))
           nil)))
    (when (get sym 'group-documentation)
      (setq docstring (concat docstring "\n----\ndefgroup " (symbol-name sym) ":\n" (get sym 'group-documentation))))
    (when (facep sym)
      (setq docstring (concat docstring "\n----\nface " (symbol-name sym) ":\n" (get sym 'face-documentation))))
    docstring))

(defun el-docstring-sap&#x2013;hide()
  "Hide the the elisp docstring."
  (funcall el-docstring-sap&#x2013;display-func nil))

(defun el-docstring-sap&#x2013;display(&optional sym)
  "Display the elisp docstring for optional SYM defaulting to \`symbol-at-point'."
  (interactive)
  (let\*((sym (if sym sym (symbol-at-point)))
        (docstring (if  sym (el-docstring-sap&#x2013;docstring sym) nil)))
    (funcall el-docstring-sap&#x2013;display-func  docstring)))

(defun el-docstring-sap&#x2013;timer-func()
  "Function called every \`el-docstring-sap&#x2013;delay' seconds when \`el-docstring-sap-mode' is non-nil."
  (when (bound-and-true-p el-docstring-sap-mode)
    (let ((sym (symbol-at-point)))
      (when (not (eq sym el-docstring-sap&#x2013;lastsym))
        (el-docstring-sap&#x2013;display sym))
      (setq el-docstring-sap&#x2013;lastsym  sym))))

(defun el-docstring-sap&#x2013;select-display-func()
  "Prompt to select a \`el-docstring-sap-mode' display function from \`el-docstring-sap&#x2013;display-funcs'."
  (interactive)
  (el-docstring-sap&#x2013;hide)
  (let ((f (alist-get (completing-read "function: " el-docstring-sap&#x2013;display-funcs)  el-docstring-sap&#x2013;display-funcs el-docstring-sap&#x2013;display-func nil #'equal)))
    (when f
      (setq el-docstring-sap&#x2013;lastsym nil)
      (setq el-docstring-sap&#x2013;display-func f))))

;;;###autoload
(define-minor-mode el-docstring-sap-mode
  "minor-mode to popup help for the elisp symbol at point."
  nil
  :lighter el-docstring-sap&#x2013;lighter
  (if (bound-and-true-p el-docstring-sap-mode)
          (add-hook 'pre-command-hook 'el-docstring-sap&#x2013;hide nil t)
    (remove-hook 'pre-command-hook 'el-docstring-sap&#x2013;hide t))

(el-docstring-sap&#x2013;hide)
(unless el-docstring-sap&#x2013;timer
  (setq  el-docstring-sap&#x2013;timer
         (run-with-idle-timer
          el-docstring-sap&#x2013;delay t
          'el-docstring-sap&#x2013;timer-func))))

(provide 'el-docstring-sap)
;;; el-docstring-sap.el ends here
