;;; projectile-direnv.el --- Set environment variables from .envrc -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Christian Romney <crommney@pointslope.com>
;; URL: https://github.com/christianromney/projectile-direnv
;; Package-Version: 20160305.1738
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (s "1.11.0") (dash "2.12.0") (projectile "0.13.0"))

;; This program is free software; you can redistribute it and/or modify
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

;; I want to launch Emacs as a GUI and have it automatically
;; set the same environment variables that direnv sets for me
;; at the shell when I am in a Projectile project.
;;
;; See README.org for more info.
;; 
;;; Code:

(require 's)
(require 'dash)
(require 'projectile)

(defun direnv-data (dir)
  ;; TODO: use dir for folder or smart current-project-dir variable
  (let ((cmd (concat "/bin/bash -i -c '" "cd " dir " && direnv export bash'")))
    (shell-command-to-string cmd)))

;;(direnv-data "~/src/direnv")
(defun commands-from-direnv (text)
  (cl-remove-if 's-blank?
                (split-string (first (last (split-string text "\n"))) ";")))

(defun line->pair (line)
  (split-string (string-join (rest (split-string line " ")) " ") "="))

(defun remove-$-and-quotes (val)
  (s-with val
    (s-chop-prefix "$")
    (s-chop-prefix "'")
    (s-chop-suffix "'")
    (s-chop-prefix "\"")
    (s-chop-suffix "\"")))

(defun line->kv (line)
  (let* ((pair (line->pair line))
         (key (first pair))
         (value (remove-$-and-quotes (first (last pair)))))
    (list key value)))

(defun is-export? (str)
  (s-starts-with? "export" str))

(defun is-ignored-key? (ls)
  (let ((key (first ls)))
    (or
     (s-starts-with? "DIRENV" key)
     ;; (s-starts-with? "PATH" key)
     )))

(defun extract-exports (cmds)
  (mapcar 'line->kv
          (cl-remove-if-not 'is-export?
                            (commands-from-direnv cmds))))

(defun commands->list (cmds)
  (let ((exports (extract-exports cmds)))
    (cl-remove-if 'is-ignored-key? exports)))

(defun setenv-pair (pair)
  (let* ((k (first pair))
         (v (first (last pair))))
    (setenv k v)))

(defun set-env-from-direnv (dir)
  (let* ((data (direnv-data dir))
         (pairs (commands->list data)))
    (mapcar 'setenv-pair pairs)))

(defvar projectile-direnv-envrc nil
  "Contains the path to the last loaded .envrc")

(defun projectile-direnv-parse-export (line)
  "Parses a single line of the form export VAR=VAL into a cons
cell where the car is the var name and the cdr is its value."
  (let* ((parts (s-split "=" line))
         (varname (car (last (s-split " " (car parts)))))
         (varval (car (last parts))))
    (cons varname varval)))

(defun projectile-direnv-read-file-as-string (filename)
  "Returns a the file's contents as a string"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun projectile-direnv-set-env-var (pair)
  "Sets an environment variable. Expects a pair of (VARNAME . VALUE)"
  (setenv (car pair) (cdr pair)))

(defun projectile-direnv-list-exports (exports)
  "Returns a string of the form '+VAR1 +VAR2' listing the exported envvars"
  (s-join " " (-map (lambda (e) (s-append (car e) "+")) exports)))

(defun projectile-direnv-export-variables ()
  "Reads a .envrc file in the Projectile project root, and sets
environment variables for any defined exports"
  (interactive)
  (when (projectile-project-p)
    (let ((envrc (expand-file-name ".envrc" (projectile-project-root))))
      (when (and (file-exists-p envrc)
                 (not (string= envrc projectile-direnv-envrc)))
        (set-env-from-direnv (projectile-project-root))))))

(provide 'projectile-direnv)
;;; projectile-direnv.el ends here
