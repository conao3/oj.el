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

(defun oj-install-package ()
  "Install `oj' pip package via `pip3'."
  (interactive)
  (when (executable-find "oj")
    (user-error "You already have `oj'.  Do nothing"))
  (unless (executable-find "python3")
    (error "Missing `python3.'  Please ensure Emacs's PATH and the installing"))
  (unless (executable-find "pip3")
    (error "Missing `pip3.'  Please ensure Emacs's PATH and the installing"))
  (with-current-buffer (get-buffer-create "*pip install oj*")
    (pop-to-buffer (current-buffer))
    (shell-command "echo \"\\$ pip3 install online-judge-tools\n\" && \
pip3 install online-judge-tools && \
echo \"\n\ninstall succeeded!\"" (current-buffer))))

(provide 'oj)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; oj.el ends here
