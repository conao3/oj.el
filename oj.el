;;; oj.el --- Competitive programming tools client for AtCoder, Codeforces  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/conao3/oj.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Competitive programming tools client for AtCoder, Codeforces.


;;; Code:

(defgroup oj nil
  "Competitive programming tools client."
  :prefix "oj-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/oj.el"))

(defcustom oj-shell-program shell-file-name
  "Shell program to use `oj'."
  :group 'oj
  :type 'string)


;;; Functions

(defvar comint-process-echoes)
(declare-function comint-send-input "comint")
(declare-function comint-send-string "comint")
(declare-function make-comint-in-buffer "comint")

(defvar oj-buffer nil)

(defun oj--exec-script (script)
  "Exec SCRIPT in `oj-buffer'."
  (oj-open-shell)
  (comint-send-string oj-buffer script)
  (with-current-buffer oj-buffer
    (comint-send-input)))

(defun oj--install-package-maybe ()
  "Install `oj' pip package via `pip3'."
  (unless (executable-find "oj")
    (unless (yes-or-no-p "Install online-judge-tools via pip3?")
      (error "Abort install"))
    (unless (executable-find "python3")
      (error "Missing `python3.'  Please ensure Emacs's PATH and the installing"))
    (unless (executable-find "pip3")
      (error "Missing `pip3'.  Please ensure Emacs's PATH and the installing"))
  (oj--exec-script "pip3 install online-judge-tools")))


;;; Main

(defun oj-open-shell ()
  "Open shell to controll `oj'."
  (interactive)
  (setq oj-buffer (get-buffer-create (format "*oj - %s*" oj-shell-program)))
  (unless (process-live-p (get-buffer-process oj-buffer))
    (with-current-buffer oj-buffer
      (setq comint-process-echoes t))
    (make-comint-in-buffer "oj" oj-buffer shell-file-name))
  (display-buffer oj-buffer)
  (oj--install-package-maybe))

(defun oj-login ()
  "Login online-judge system."
  (oj--exec-script "oj login https://atcoder.jp/"))

(provide 'oj)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; oj.el ends here
