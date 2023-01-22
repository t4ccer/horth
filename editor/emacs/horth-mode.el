;;; horth-mode.el --- horth lang support  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Tomasz Maciosowski (t4ccer) <t4ccer@gmail.com>

;; Author: Tomasz Maciosowski (t4ccer) <t4ccer@gmail.com>
;; URL: https://github.com/t4ccer/horth

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(defconst horth-mode--syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b")
    (modify-syntax-entry ?* ". 23")
    (modify-syntax-entry ?\n "> b")
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"")
    (syntax-table))
  "Syntax table for `horth-mode'.")

;;;###autoload
(define-derived-mode horth-mode prog-mode "horth"
  "Major Mode for editing horth source code."
  :syntax-table horth-mode--syntax-table
  (font-lock-add-keywords nil '(("if\\|else\\|proc\\|end" . font-lock-keyword-face)))
  (font-lock-add-keywords nil '(("\\(\s\\|^\\)\\([0-9]+\\)\\(\s\\|$\\)" 2 font-lock-constant-face)))
  (font-lock-add-keywords nil '(("\\(\s\\|(\\)\\(int\\|ptr\\|bool\\)" 2 font-lock-type-face)))
  (setq-local comment-start "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.horth\\'" . horth-mode))

(provide 'horth-mode)
;;; horth-mode.el ends here
