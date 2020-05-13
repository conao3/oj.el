;;; oj.el --- Competitive programming tools client for AtCoder, Codeforces  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (quickrun "2.2"))
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

(require 'format-spec)
(require 'quickrun)

(defgroup oj nil
  "Competitive programming tools client."
  :prefix "oj-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/oj.el"))

(defcustom oj-shell-program shell-file-name
  "Shell program to use `oj'."
  :group 'oj
  :type 'string)

(defcustom oj-home-dir (locate-user-emacs-file "oj")
  "Directory path for `oj'."
  :group 'oj
  :type 'directory)

(defcustom oj-default-programming-language 'cpp
  "Default online judge service.

This variable symbol is used for `--template' of `atcoder-tools'."
  :group 'oj
  :type
  '(choice (const :tag "C++" 'cpp)
           (const :tag "Java" 'java)
           (const :tag "Rust" 'rust)
           (const :tag "Python" 'python)
           (const :tag "Nim" 'nim)
           (const :tag "D" 'd)
           (const :tag "C#" 'cs)))

(defvar oj-programming-languages
  '((cpp    . ((raw . "C++")    (name . "cpp")    (ext . "cpp")))
    (java   . ((raw . "Java")   (name . "java")   (ext . "java")))
    (rust   . ((raw . "Rust")   (name . "rust")   (ext . "rs")))
    (python . ((raw . "Python") (name . "python") (ext . "py")))
    (nim    . ((raw . "Nim")    (name . "nim")    (ext . "nim")))
    (d      . ((raw . "D")      (name . "d")      (ext . "d")))
    (cs     . ((raw . "C#")     (name . "cs")     (ext . "cs")))))

(defcustom oj-default-online-judge 'atcoder
  "Default online judge service."
  :group 'oj
  :type
  '(choice (const :tag "Aizu Online Judge (Arena)" 'aoj-arena)
           (const :tag "Aizu Online Judge"         'aoj)
           (const :tag "Anarchy Golf"              'anrchy-golf)
           (const :tag "AtCoder"                   'atcoder)
           (const :tag "Codeforces"                'codeforces)
           (const :tag "CS Academy"                'cs-academy)
           (const :tag "Facebook Hacker Cup"       'facebook-hacker-cup)
           (const :tag "HackerRank"                'hackerrank)
           (const :tag "Kattis"                    'kattis)
           (const :tag "PKU JudgeOnline"           'pku)
           (const :tag "Topcoder"                  'topcoder)
           (const :tag "Toph (Problem Archive)"    'toph)
           (const :tag "CodeChef"                  'codechef)
           (const :tag "Sphere online judge"       'soj)
           (const :tag "yukicoder"                 'yukicoder)
           (const :tag "Library Checker"           'library-checker)))

(defvar oj-online-judges
  '((aoj-arena         . ((name . "Aizu Online Judge (Arena)") (url . "https://onlinejudge.u-aizu.ac.jp/services/arena.html")))
    (aoj               . ((name . "Aizu Online Judge")         (url . "https://onlinejudge.u-aizu.ac.jp/home")))
    (anrchy-golf       . ((name . "Anarchy Golf")              (url . "http://golf.shinh.org/")))
    (atcoder           . ((name . "AtCoder")                   (url . "https://atcoder.jp/")))
    (codeforces        . ((name . "Codeforces")                (url . "https://codeforces.com/")))
    (cs-academy        . ((name . "CS Academy")                (url . "https://csacademy.com/")))
    (facebook          . ((name . "Facebook Hacker Cup")       (url . "https://www.facebook.com/hackercup/")))
    (hackerrank        . ((name . "HackerRank")                (url . "https://www.hackerrank.com/")))
    (kattis            . ((name . "Kattis")                    (url . "https://open.kattis.com/")))
    (pku               . ((name . "PKU JudgeOnline")           (url . "http://poj.org/")))
    (topcoder          . ((name . "Topcoder")                  (url . "https://www.topcoder.com/")))
    (toph              . ((name . "Toph (Problem Archive)")    (url . "https://toph.co/")))
    (codechef          . ((name . "CodeChef")                  (url . "https://www.codechef.com/")))
    (soj               . ((name . "Sphere online judge")       (url . "https://www.spoj.com/")))
    (yukicoder         . ((name . "yukicoder")                 (url . "https://yukicoder.me/")))
    (library-checker   . ((name . "Library Checker")           (url . "https://judge.yosupo.jp/"))))
  "Supported online judges.

- Download sample cases
  (aoj-arena aoj anrchy-golf atcoder codeforces
   cs-academy facebook hackerrank kattis pku toph
   codechef soj yukicoder library-checker)

- Download system cases
  (aoj yukicoder)

- Submit solution source code
  (atcoder codeforces topcoder hackerrank toph)")


;;; Functions

(defvar comint-process-echoes)
(declare-function comint-send-input "comint")
(declare-function comint-delete-input "comint")
(declare-function comint-send-string "comint")
(declare-function make-comint-in-buffer "comint")

(defvar oj-buffer nil)

(defun oj--exec-script (script)
  "Exec SCRIPT in `oj-buffer'."
  (oj-open-shell)
  (with-current-buffer oj-buffer
    (goto-char (point-max))
    (comint-delete-input)
    (comint-send-string oj-buffer script)
    (with-current-buffer oj-buffer
      (comint-send-input))))

(defun oj--file-readable (file)
  "Return FILE if readable."
  (when (file-readable-p file) file))


;;; Main

(defun oj-open-shell ()
  "Open shell to controll `oj'."
  (interactive)
  (setq oj-buffer (get-buffer-create (format "*oj - %s*" oj-shell-program)))
  (unless (process-live-p (get-buffer-process oj-buffer))
    (with-current-buffer oj-buffer
      (setq comint-process-echoes t))
    (make-comint-in-buffer "oj" oj-buffer shell-file-name))
  (display-buffer oj-buffer))

(defun oj-install-package ()
  "Install `oj', `atcoder-tools' pip package via `pip3'."
  (interactive)
  (unless (yes-or-no-p "Install tools via pip3?")
    (error "Abort install"))
  (dolist (elm '(("oj" . "online-judge-tools")
                 ("atcoder-tools" . "atcoder-tools")))
    (unless (executable-find (car elm))
      (unless (executable-find "python3")
        (error "Missing `python3'.  Please ensure Emacs's PATH and the installing"))
      (unless (executable-find "pip3")
        (error "Missing `pip3'.  Please ensure Emacs's PATH and the installing"))
      (oj--exec-script (format "pip3 install %s" (cdr elm))))))

(defun oj-login ()
  "Login online-judge system."
  (oj--exec-script "oj login https://atcoder.jp/"))

(defun oj-generate (contest)
  "Generate new CONTEST working folder."
  (interactive (list (read-string "Contest name (agc001): " nil nil "agc001")))
  (let-alist (alist-get oj-default-programming-language oj-programming-languages)
    (oj--exec-script
     (concat
      (format "atcoder-tools gen %s --workspace %s --lang %s"
              contest (expand-file-name "atcoder" oj-home-dir) .name)
      (when-let (file (oj--file-readable
                       (expand-file-name
                        (format "template/template.%s" .ext) oj-home-dir)))
        (format " --template %s" file))
      (when-let (file (oj--file-readable
                       (expand-file-name
                        "conf/atcoder-tools.conf" oj-home-dir)))
        (format " --config %s" file)))))
  (oj--exec-script
   (format "cd %s"
           (expand-file-name
            "A" (expand-file-name
                 contest (expand-file-name "atcoder" oj-home-dir)))))
  (when (executable-find "tree")
    (oj--exec-script "tree")))

(defun oj-test (&optional dir)
  "Run test at DIR."
  (interactive)
  (let* ((alist (quickrun--command-info
                (quickrun--command-key (buffer-file-name))))
         (spec (quickrun--template-argument alist (buffer-file-name))))
    (when-let (fmt (alist-get :compile-only alist))
      (oj--exec-script (format-spec fmt spec))))
  (when dir (oj--exec-script (format "cd %s" dir)))
  (oj--exec-script "atcoder-tools test"))

(defun oj-submit (&optional dir)
  "Submit code at DIR."
  (interactive)
  (when dir (oj--exec-script (format "cd %s" dir)))
  (oj--exec-script "atcoder-tools submit"))

(provide 'oj)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; oj.el ends here
