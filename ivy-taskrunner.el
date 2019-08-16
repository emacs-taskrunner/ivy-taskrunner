;;; ivy-taskrunner.el --- Retrieve build system/taskrunner tasks via ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/ivy-taskrunner
;; Version: 1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: build-system taskrunner build task-runner tasks ivy

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This package provides an ivy interfaces to the taskrunner library

;;;; Installation

;;;;; MELPA
;; If installed form MELPA then you are done.

;;;;; Manual

;; Install these required packages:

;; projectile
;; taskrunner
;; ivy

;; Then put this folder in your load-path, and put this in your init:

;; (require 'ivy-taskrunner)

;;;; Usage

;; When in any buffer, either call the command `ivy-taskrunner' or
;; `ivy-taskrunner-update-cache' to be presented with a list of targets/tasks in
;; your project.
;; Additionally, if you would like to rerun the last ran command, use
;; `ivy-taskrunner-rerun-last-command'.

;;;; Credits

;; This package would not have been possible without the following
;; packages:
;; ivy.el[1] which helped me create the interface
;;
;; [1] https://github.com/abo-abo/swiper

;;; License:

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

;;; Code:

;;;; Requirements


(require 'ivy)
(require 'taskrunner)

(defgroup ivy-taskrunner nil
  "Group for ivy-taskrunner.")

;;;; Variables
(defcustom ivy-taskrunner-project-warning
  "The currently visited buffer must be in a project in order to select a task!
   Please switch to a project which is recognized by projectile!"
  "Warning used when the user tries to run ivy-taskrunner while
not in a project which is recognized by projectile."
  :group 'ivy-taskrunner
  :type 'string)

(defvaralias 'ivy-taskrunner-preferred-js-package-manager 'taskrunner-preferred-js-package-manager)
(defvaralias 'ivy-taskrunner-get-all-make-targets 'taskrunner-retrieve-all-make-targets)
(defvaralias 'ivy-taskrunner-leiningen-buffer-name 'taskrunner-leiningen-buffer-name)
(defvaralias 'ivy-taskrunner-leiningen-task-section-regexp 'taskrunner-leiningen-task-section-header-regexp)
(defvaralias 'ivy-taskrunner-gradle-taskbuffer-name 'taskrunner-gradle-tasks-buffer-name)
(defvaralias 'ivy-taskrunner-gradle-heading-regexps 'taskrunner-gradle-heading-regexps)
(defvaralias 'ivy-taskrunner-ant-tasks-buffer-name 'taskrunner-ant-tasks-buffer-name)

;; Users can add additional actions by appending to this variable
(defvar ivy-taskrunner-actions
  '(("r" ivy-taskrunner--root-task "Run task in root without extra args")
    ("R" ivy-taskrunner--root-task-prompt "Run task in root with extra args")
    ("c" ivy-taskrunner--current-dir "Run task in current folder without args")
    ("C" ivy-taskrunner--current-dir-prompt "Run task in current folder with args")
    )
  "A list of extra actions which can be used when running a task selected through ivy.")

;;;; Functions

(defun ivy-taskrunner--root-task (TASK)
  "Run the task TASK in the project root without asking for extra args.
This is the default command when selecting/running a task/target."
  (taskrunner-run-task TASK)
  )

(defun ivy-taskrunner--root-task-prompt (TASK)
  "Run the task TASK in the project root and ask the user for extra args."
  (taskrunner-run-task TASK nil t)
  )

(defun ivy-taskrunner--current-dir (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Do not prompt the user to supply any extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      ;; (message "FILENAME: %s Task: %s" (file-name-directory curr-file) TASK)
      (taskrunner-run-task TASK (file-name-directory curr-file) nil))
    )
  )

(defun ivy-taskrunner--current-dir-prompt (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Prompt the user to supply extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      ;; (message "FILENAME: %s Task: %s" (file-name-directory curr-file) TASK)
      (taskrunner-run-task TASK (file-name-directory curr-file) t))
    )
  )

(defun ivy-taskrunner--check-if-in-project ()
  "Check if the currently visited buffer is in a project.
If it is not, prompt the user to select a project"
  ;; If we are not in a project, ask the user to switch to one
  (if (not (projectile-project-p))
      ;; If counsel is intalled, use that, otherwise use the default
      ;; projectile-switch-project interface. The command returns
      (if (package-installed-p 'counsel)
          (progn
            (require 'counsel)
            (counsel-projectile-switch-project)))
    (setq in-project-p (projectile-switch-project)))
  )

(defun ivy-taskrunner ()
  "Launch ivy to select a task to run in the current project."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  ;; Run the ivy interface only if a user selects a project. If the user
  ;; leaves the projectile-switch-project prompt then there is nothing
  ;; returned and the value stays nil.
  (if (projectile-project-p)
      (progn
        ;; Add extra actions
        (ivy-set-actions
         'ivy-taskrunner
         ivy-taskrunner-actions)
        
        ;; Run ivy
        (ivy-read "Task to run: "
                  (taskrunner-get-tasks-from-cache)
                  :require-match t
                  :action 'ivy-taskrunner--root-task))
    (message ivy-taskrunner-project-warning)
    )
  )

(defun ivy-taskrunner-rerun-last-command ()
  "Rerun the last task ran in the currently visited project."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-rerun-last-task (projectile-project-root))
    (message ivy-taskrunner-project-warning))
  )

(provide 'ivy-taskrunner)
;;; ivy-taskrunner.el ends here
