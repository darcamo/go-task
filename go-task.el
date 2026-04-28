;;; go-task.el --- Use go-task from Emacs.           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darlan@darlan-desktop>

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Commentary:

;; This package provides an interface to use go-task from Emacs. It allows you to run go-task commands and manage your tasks directly from Emacs.

;;; Code:

(require 'compile)
(require 'seq)
(require 'subr-x)


(defgroup go-task nil
  "Run go-task commands from Emacs."
  :group 'tools
  :prefix "go-task-")


(defcustom go-task-command "go-task"
  "Executable used to run go-task commands."
  :group 'go-task
  :type 'string)


(defun go-task--call (&rest args)
  "Call go-task synchronously with ARGS.
Return a cons cell of the form (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (let ((exit (apply #'process-file go-task-command nil t nil args)))
      (cons exit (buffer-string)))))


(defun go-task--call-or-error (&rest args)
  "Call go-task with ARGS, raising an `error' on failure.
Return the trimmed command output on success."
  (pcase-let ((`(,exit . ,output) (apply #'go-task--call args)))
    (if (zerop exit)
        (string-trim-right output "\n+")
      (error "`go-task %s` failed: %s"
             (mapconcat #'identity args " ")
             (string-trim output)))))


(defun go-task--run-command (&rest args)
  "Run go-task asynchronously with ARGS, showing output in a buffer."
  (let* ((command
          (mapconcat #'shell-quote-argument (cons go-task-command args) " "))
         (buffer-name "*go-task*"))
    (compilation-start command 'compilation-mode (lambda (_) buffer-name)))
  (message "Running go-task%s"
           (if args
               (format " %s" (mapconcat #'identity args " "))
             "")))


;;;###autoload
(defun go-task-init ()
  "Run `go-task --init' to generate a Taskfile."
  (interactive)
  (pcase-let ((`(,exit . ,output) (go-task--call "--init")))
    (if (zerop exit)
        (message "Taskfile.yml created with go-task --init")
      (user-error "`go-task --init` failed: %s" (string-trim output)))))

(defun go-task--parse-task-list (output)
  "Convert go-task --list OUTPUT into a list of task names."
  (split-string output "\n" t))


(defun go-task--get-tasks ()
  "Return a list of available task names from go-task."
  (let ((output (go-task--call-or-error "--list" "--silent")))
    (or (go-task--parse-task-list output)
        (user-error "No go-task tasks found"))))


;;;###autoload
(defun go-task-run-task (prefix)
  "Prompt for a task and run it via go-task.
With PREFIX (\[universal-argument]), run the default task without prompting."
  (interactive "P")
  (if prefix
      (go-task--run-command)
    (let* ((tasks (go-task--get-tasks))
           (default (car tasks))
           (choice
            (completing-read "Run go-task task: " tasks nil t nil nil default)))
      (go-task--run-command choice))))


;;;###autoload
(defun go-task-list-tasks ()
  "Show the output of `go-task --list' in a dedicated buffer."
  (interactive)
  (let ((output (go-task--call-or-error "--list")))
    (with-current-buffer (get-buffer-create "*go-task tasks*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (view-mode 1))
      (display-buffer (current-buffer)))))

(provide 'go-task)
;;; go-task.el ends here
