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
(require 'subr-x)
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

(defcustom oj-default-online-judge 'atcoder
  "Default online judge service."
  :group 'oj
  :type
  '(choice (const :tag "Aizu Online Judge"      'aoj)
           (const :tag "Anarchy Golf"           'anrchy-golf)
           (const :tag "AtCoder"                'atcoder)
           (const :tag "Codeforces"             'codeforces)
           (const :tag "CS Academy"             'cs-academy)
           (const :tag "Facebook Hacker Cup"    'facebook-hacker-cup)
           (const :tag "HackerRank"             'hackerrank)
           (const :tag "Kattis"                 'kattis)
           (const :tag "PKU JudgeOnline"        'pku)
           (const :tag "Topcoder"               'topcoder)
           (const :tag "Toph (Problem Archive)" 'toph)
           (const :tag "CodeChef"               'codechef)
           (const :tag "Sphere online judge"    'soj)
           (const :tag "yukicoder"              'yukicoder)
           (const :tag "Library Checker"        'library-checker)))

(defvar oj-online-judges
  '((aoj                . ((name . "Aizu Online Judge")        (url . "https://onlinejudge.u-aizu.ac.jp/")))
    (aoj2               . ((name . "Aizu Online Judge (Beta)") (url . "https://onlinejudge.u-aizu.ac.jp/courses/")))
    (anrchy-golf        . ((name . "Anarchy Golf")             (url . "http://golf.shinh.org/p.rb?")))
    (atcoder            . ((name . "AtCoder")                  (url . "https://atcoder.jp/contests/")))
    (codeforces         . ((name . "Codeforces")               (url . "https://codeforces.com/contests/")))
    (cs-academy         . ((name . "CS Academy")               (url . "https://csacademy.com/contests/")))
    (facebook           . ((name . "Facebook Hacker Cup")      (url . "https://www.facebook.com/hackercup/")))
    (hackerrank         . ((name . "HackerRank")               (url . "https://www.hackerrank.com/challenges/")))
    (hackerrank-contest . ((name . "HackerRank Contest")       (url . "https://www.hackerrank.com/contests/")))
    (kattis             . ((name . "Kattis")                   (url . "https://open.kattis.com/probrems/")))
    (poj                . ((name . "PKU JudgeOnline")          (url . "http://poj.org/problem?id=")))
    (topcoder           . ((name . "Topcoder")                 (url . "https://www.topcoder.com/challenges/")))
    (toph               . ((name . "Toph (Problem Archive)")   (url . "https://toph.co/p/")))
    (codechef           . ((name . "CodeChef")                 (url . "https://www.codechef.com/problems/")))
    (soj                . ((name . "Sphere online judge")      (url . "https://www.spoj.com/problems/")))
    (yukicoder          . ((name . "yukicoder")                (url . "https://yukicoder.me/problems/no/")))
    (yukicoder-contest  . ((name . "yukicoder Contest")        (url . "https://yukicoder.me/contests/")))
    (library-checker    . ((name . "Library Checker")          (url . "https://judge.yosupo.jp/problem/"))))
  "Supported online judges.

- Download sample cases
  (aoj-arena aoj anrchy-golf atcoder codeforces
   cs-academy facebook hackerrank kattis poj toph
   codechef soj yukicoder library-checker)

- Download system cases
  (aoj yukicoder)

- Submit solution source code
  (atcoder codeforces topcoder hackerrank toph)

NOTE: online-judge symbol MUST NOT include slash (\"/\").")


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

(defun oj--online-judges ()
  "Return supported online-judge symbols."
  (mapcar #'car oj-online-judges))

(defun oj--first-value (fn lst)
  "Apply FN for LST and return first non-nil value."
  (when-let (val (cl-member-if fn lst))
    (funcall fn (car val))))

(defun oj--url-to-online-judge (url)
  "Detect online-judge service from URL."
  (oj--first-value
   (lambda (elm)
     (let ((sym (car elm))
           (alist (cdr elm)))
       (when (string-match (alist-get 'url alist) url)
         sym)))
   oj-online-judges))

(defun oj--url-to-dirs (url)
  "Detect suitable dirs from URL."
  (or
   ;; http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ITP1_8_C
   ;; http://judge.u-aizu.ac.jp/onlinejudge/description.jsp
   (and (string-match
         "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp\\?id=\\([^/]+\\)" url)
        `("aizu" ,(match-string 1 url)))
   (and (string-match
         "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp" url)
        `("aizu" "1000"))

   ;; https://onlinejudge.u-aizu.ac.jp/courses/lesson/2/ITP1/1/ITP1_1_A
   (and (string-match
         "https://onlinejudge.u-aizu.ac.jp/courses/[^/]*/[^/]*/[^/]*/[^/]*/\\([^/]+\\)/?" url)
        `("aizu" ,(match-string 1 url)))

   ;; http://golf.shinh.org/p.rb?hello+world
   (and (string-match
         "http://golf.shinh.org/p.rb\\?\\([^/]+\\)" url)
        `("anrchy-golf" ,(replace-regexp-in-string "\\+" "_" (match-string 1 url))))

   ;; https://atcoder.jp/contests/abc167/tasks/abc167_a
   ;; https://atcoder.jp/contests/abc167
   (and (string-match
         "https://atcoder.jp/contests/[^/]*/tasks/\\([^/]+\\)/?" url)
        `("atcoder" ,(match-string 1 url)))
   (and (string-match
         "https://atcoder.jp/contests/\\([^/]+\\)/?" url)
        `("atcoder" ,(match-string 1 url)))

   ;; https://codeforces.com/contest/1349/problem/A
   (and (string-match
         "https://codeforces.com/contest/\\([^/]+\\)/problem/\\([^/]+\\)/?" url)
        `("codeforces" ,(format "%s_%s" (match-string 1 url) (match-string 2 url))))
   (and (string-match
         "https://codeforces.com/contest/\\([^/]+\\)/?" url)
        `("codeforces" ,(match-string 1 url)))

   ;; https://csacademy.com/contest/interview-archive/task/3-divisible-pairs/
   ;; https://csacademy.com/contest/archive/task/gcd/
   (and (string-match
         "https://csacademy.com/contest/[^/]+/task/\\([^/]+\\)/?" url)
        `("cs-academy" ,(match-string 1 url)))

   ;; https://www.hackerrank.com/challenges/c-tutorial-basic-data-types
   ;; https://www.hackerrank.com/challenges/c-tutorial-basic-data-types/problem
   ;; https://www.hackerrank.com/contests/projecteuler/challenges/euler254/problem
   (and (string-match
         "https://www.hackerrank.com/challenges/\\([^/]+\\)/?\\(problem\\)?" url)
        `("hackerrank" ,(match-string 1 url)))
   (and (string-match
         "https://www.hackerrank.com/contests/\\([^/]+\\)/challenges/\\([^/]+\\)/?\\(problem\\)?" url)
        `("hackerrank" ,(format "%s_%s" (match-string 1 url) (match-string 2 url))))
   (and (string-match
         "https://www.hackerrank.com/contests/\\([^/]+\\)/?" url)
        `("hackerrank" ,(match-string 1 url)))

   ;; https://open.kattis.com/problems/hello
   (and (string-match
         "https://open.kattis.com/problems/\\([^/]+\\)/?" url)
        `("kattis" ,(match-string 1 url)))

   ;; http://poj.org/problem?id=1000
   (and (string-match
         "http://poj.org/problem\\?id=\\([^/]+\\)" url)
        `("poj" ,(match-string 1 url)))

   ;; https://www.topcoder.com/challenges/30125517
   (and (string-match
         "https://www.topcoder.com/challenges/\\([^/]+\\)/?" url)
        `("topcoder" ,(match-string 1 url)))

   ;; https://toph.co/p/count-the-chaos
   (and (string-match
         "https://toph.co/p/\\([^/]+\\)/?" url)
        `("toph" ,(match-string 1 url)))

   ;; https://www.codechef.com/problems/TREEMX
   (and (string-match
         "https://www.codechef.com/problems/\\([^/]+\\)/?" url)
        `("codechef" ,(match-string 1 url)))

   ;; https://www.spoj.com/problems/TEST/
   ;; https://www.spoj.com/problems/PRIME1/
   (and (string-match
         "https://www.spoj.com/problems/\\([^/]+\\)/?" url)
        `("soj" ,(match-string 1 url)))

   ;; https://yukicoder.me/contests/262
   ;; https://yukicoder.me/problems/no/1046
   (and (string-match
         "https://yukicoder.me/contests/\\([^/]+\\)/?" url)
        `("yukicoder" ,(match-string 1 url)))
   (and (string-match
         "https://yukicoder.me/problems/no/\\([^/]+\\)/?" url)
        `("yukicoder" ,(match-string 1 url)))

   ;; https://judge.yosupo.jp/problem/aplusb
   (and (string-match
         "https://judge.yosupo.jp/problem/\\([^/]+\\)/?" url)
        `("library-checker" ,(match-string 1 url)))

   `("test")))

(defun oj--shortname-to-url (shortname &optional judge)
  "Convert SHORTNAME to URL for JUDGE.

  (oj--shortname-to-url \"abc167\" 'atcoder)
  ;;=> https://atcoder.jp/contests/abc167

  (oj--shortname-to-url \"abc167/a\" 'atcoder)
  ;;=> https://atcoder.jp/contests/abc167/tasks/abc167_a"
  (let ((judge* (or judge oj-default-online-judge))
        (args (split-string shortname "/")))
    (cl-case (length args)
      (1
       (concat (alist-get 'url (alist-get judge* oj-online-judges)) shortname))
      (2
       (cl-case judge*
         (atcoder
          (format "https://atcoder.jp/contests/%s/tasks/%s"
                  (nth 0 args)
                  (format "%s_%s" (nth 0 args) (nth 1 args))))
         (codeforces
          (format "https://codeforces.com/contest/%s/problem/%s"
                  (nth 0 args) (nth 1 args)))
         (hackerrank
          (format "https://www.hackerrank.com/contests/%s/challenges/%s"
                  (nth 0 args) (nth 1 args)))
         (otherwise
          (error "Slash input (%s) for %s is not supported" shortname judge*)))))))


;;; Main

(defun oj-install-package ()
  "Install `oj', `oj-template', `selenium' pip package via `pip3'."
  (interactive)
  (unless (yes-or-no-p "Install `oj', `oj-template', `selenium' via pip3?")
    (error "Abort install"))
  (dolist (elm '(("oj" . "online-judge-tools")
                 ("oj-template" . "online-judge-template-generator")
                 ("selenium" . "selenium")))
    (unless (executable-find (car elm))
      (unless (executable-find "python3")
        (error "Missing `python3'.  Please ensure Emacs's PATH and the installing"))
      (unless (executable-find "pip3")
        (error "Missing `pip3'.  Please ensure Emacs's PATH and the installing"))
      (oj--exec-script (format "pip3 install %s" (cdr elm))))))

(defun oj-open-shell ()
  "Open shell to controll `oj'."
  (interactive)
  (setq oj-buffer (get-buffer-create (format "*oj - %s*" oj-shell-program)))
  (unless (process-live-p (get-buffer-process oj-buffer))
    (with-current-buffer oj-buffer
      (setq comint-process-echoes t))
    (make-comint-in-buffer "oj" oj-buffer shell-file-name))
  (display-buffer oj-buffer))

(defun oj-login (name)
  "Login online-judge system.

NAME is string of (mapcar #'car oj-online-judges).
NAME is also whole URL to login."
  (interactive (list (completing-read "Login for: " (mapcar #'car oj-online-judges))))
  (let ((url (if (string-match "/" name)
                 name
               (alist-get 'url (alist-get (intern name) oj-online-judges)))))
    (oj--exec-script (format "oj login %s" url))))

(defun oj-generate (name)
  "Generate new NAME working folder."
  (interactive
   (list (read-string
          (format "Contest name (`abc167' for %s, or URL): " oj-default-online-judge)
          nil nil "abc167")))
  (let ((url (if (string-prefix-p "http" name) name (oj--shortname-to-url name))))
    (let* ((dirs (oj--url-to-dirs url))
           (path (expand-file-name (mapconcat #'identity dirs "/") oj-home-dir)))
      (oj--exec-script
       (format "mkdir -p %s && cd %s" path path))
      (oj--exec-script
       (concat
        (format "oj-prepare %s" url))))))

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
