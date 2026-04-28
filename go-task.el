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
(require 'comint)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'tabulated-list)


(defgroup go-task nil
  "Run go-task commands from Emacs."
  :group 'tools
  :prefix "go-task-")


(defcustom go-task-command "go-task"
  "Executable used to run go-task commands."
  :group 'go-task
  :type 'string)


(defcustom go-task-use-minibuffer-password-prompts t
  "When non-nil, detect password prompts and read passwords in minibuffer.

This uses `comint-watch-for-password-prompt', which handles common prompts
such as sudo password requests."
  :group 'go-task
  :type 'boolean)


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
  (let* ((task-name (or (car args) "default"))
         (base-buffer-name (format "*go-task: %s*" task-name))
         (buffer-name
          (if (when-let* ((buffer (get-buffer base-buffer-name))
                          (process (get-buffer-process buffer)))
                (process-live-p process))
              (generate-new-buffer-name base-buffer-name)
            base-buffer-name))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((process-connection-type t))
      (apply #'make-comint-in-buffer "go-task" buffer go-task-command nil args))
    (with-current-buffer buffer
      (setq-local comint-password-prompt-regexp
                  (if go-task-use-minibuffer-password-prompts
                      comint-password-prompt-regexp
                    "\\`a\\'"))
      (goto-char (point-max)))
    (pop-to-buffer buffer))
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


(defun go-task--get-tasks ()
  "Return a list of available tasks from go-task.

The list is obtained by calling `go-task --list --json` and parsing the
output as JSON. Each task is represented as a plist with keys :name,
:desc, and :default."
  (when-let* ((output (go-task--call-or-error "--list" "--json"))
              (json-array-type 'list)
              (json-object-type 'plist)
              (tasks-plist (json-read-from-string output))
              (tasks (plist-get tasks-plist :tasks)))
    tasks))


(defun go-task--get-tasks-for-completing-read ()
  "Return an alist of task names and tasks.

All this does is getting the list of tasks via `go-task--get-tasks' and
converting it into an alist where the keys are task names and the values
are the corresponding task. This list is suitable for `completing-read'."
  (when-let* ((tasks (go-task--get-tasks)))
    (seq-map (lambda (task) (cons (plist-get task :name) task)) tasks)))


;;;###autoload
(defun go-task-run-task (prefix)
  "Prompt for a task and run it via go-task.
With PREFIX (\[universal-argument]), run the default task without prompting."
  (interactive "P")
  (if prefix
      (go-task--run-command)
    (let* ((tasks (go-task--get-tasks-for-completing-read))
           (max-name-width
            (apply #'max
                   0
                   (mapcar (lambda (task) (string-width (car task))) tasks)))
           (completion-extra-properties
            `(:affixation-function
              ,(lambda (candidates)
                 (mapcar
                  (lambda (candidate)
                    (let* ((task (alist-get candidate tasks nil nil #'string=))
                           (desc (or (plist-get task :desc) ""))
                           (padding
                            (make-string
                             (max 2
                                  (+ 2
                                     (- max-name-width
                                        (string-width candidate))))
                             ?\s))
                           (suffix
                            (if (string-empty-p desc)
                                ""
                              (concat
                               padding
                               (propertize desc 'face 'font-lock-doc-face)))))
                      (list candidate "" suffix)))
                  candidates))))
           (choice (completing-read "Run go-task task: " tasks nil t nil nil))
           (task (alist-get choice tasks nil nil #'string=)))
      (go-task--run-command (or (plist-get task :name) choice)))))


;;;###autoload
(defun go-task-list-tasks ()
  "Show available go-task tasks in a tabulated list buffer."
  (interactive)
  (let ((tasks (go-task--get-tasks)))
    (with-current-buffer (get-buffer-create "*go-task tasks*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (tabulated-list-mode)
        (setq-local tabulated-list-format
                    [("Task" 30 t)
                     ("Description" 50 t)
                     ("Up-to-date" 12 t)])
        (setq-local tabulated-list-entries
                    (mapcar
                     (lambda (task)
                       (let* ((name (or (plist-get task :name) ""))
                               (desc (or (plist-get task :desc) ""))
                               (up-to-date-value (plist-get task :up_to_date))
                               (is-up-to-date
                                (and up-to-date-value
                                     (not (eq up-to-date-value :json-false))))
                               (up-to-date
                                (if is-up-to-date
                                    "yes"
                                  "no")))
                          (list name
                                (vector
                                 (propertize name 'face 'font-lock-function-name-face)
                                 (propertize desc 'face 'font-lock-doc-face)
                                 (propertize up-to-date
                                             'face (if is-up-to-date
                                                       'success
                                                     'error))))))
                     tasks))
        (tabulated-list-init-header)
        (tabulated-list-print t))
      (display-buffer (current-buffer)))))

(provide 'go-task)
;;; go-task.el ends here
