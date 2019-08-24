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
;; async
;; ivy

;; Then put this folder in your load-path, and put this in your init:

;; (require 'ivy-taskrunner)

;;;; Usage

;; When in any buffer recognized by projectile, call the command
;; `ivy-taskrunner' to launch an ivy menu which shows all possible tasks/targets
;; in the project.  If you add new tasks then call `ivy-taskrunner-update-cache'
;; to make sure that the newly added commands will be shown.  You can use the
;; command `ivy-taskrunner-task-buffers' to show all buffers which were used to
;; run a task Additionally, if you would like to rerun the last ran command, use
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
  "Group for ivy-taskrunner."
  :group 'convenience)

;;;; Variables
(defcustom ivy-taskrunner-project-warning
  "The currently visited buffer must be in a project in order to select a task!
Please switch to a project which is recognized by projectile!"
  "Warning to indicate that a project must be visited to call ivy-taskrunner."
  :group 'ivy-taskrunner
  :type 'string)

(defcustom ivy-taskrunner-no-files-found-warning
  "ivy-taskrunner: There are no configuration files for any taskrunner/build system in the current project."
  "Warning used to indicate that no configuration files were found in the current project."
  :group 'ivy-taskrunner
  :type 'string)

(defcustom ivy-taskrunner-no-targets-found-warning
  "ivy-taskrunner: No targets found in the current project!"
  "Warning used to indicate that no targets were found."
  :group 'ivy-taskrunner
  :type 'string)

(defvaralias 'ivy-taskrunner-preferred-js-package-manager 'taskrunner-preferred-js-package-manager)
(defvaralias 'ivy-taskrunner-get-all-make-targets 'taskrunner-retrieve-all-make-targets)
(defvaralias 'ivy-taskrunner-leiningen-buffer-name 'taskrunner-leiningen-buffer-name)
(defvaralias 'ivy-taskrunner-leiningen-task-section-regexp 'taskrunner-leiningen-task-section-header-regexp)
(defvaralias 'ivy-taskrunner-gradle-taskbuffer-name 'taskrunner-gradle-tasks-buffer-name)
(defvaralias 'ivy-taskrunner-gradle-heading-regexps 'taskrunner-gradle-heading-regexps)
(defvaralias 'ivy-taskrunner-ant-tasks-buffer-name 'taskrunner-ant-tasks-buffer-name)

(defconst ivy-taskrunner-no-buffers-warning
  "ivy-taskrunner: No taskrunner buffers are currently opened!"
  "Warning used to indicate that there are not task buffers opened.")

(defvar ivy-taskrunner--project-files '()
  "Used to store the project files and their paths.")

;; Users can add additional actions by appending to this variable
(defvar ivy-taskrunner-actions
  '(("r" ivy-taskrunner--root-task "Run task in root without extra args")
    ("R" ivy-taskrunner--root-task-prompt "Run task in root with extra args")
    ("c" ivy-taskrunner--current-dir "Run task in current folder without args")
    ("C" ivy-taskrunner--current-dir-prompt "Run task in current folder with args")
    )
  "A list of extra actions which can be used when running a task selected through ivy.")

(defvar ivy-taskrunner-buffer-actions
  '(("s" switch-to-buffer "Switch to buffer")
    ("k" ivy-taskrunner--kill-buffer "Kill buffer")
    ("K" ivy-taskrunner--kill-all-buffers "Kill all buffers"))
  "A list of extra actions used when selecting a compilation buffer through ivy.")

;;;; Functions

(defun ivy-taskrunner--kill-buffer (BUFFER-NAME)
  "Kill the buffer name BUFFER-NAME."
  (kill-buffer BUFFER-NAME))

(defun ivy-taskrunner--kill-all-buffers ()
  "Kill all helm-taskrunner task buffers.
The argument TEMP is simply there since a Helm action requires a function with
one input."
  (taskrunner-kill-compilation-buffers))

(defun ivy-taskrunner--root-task (TASK)
  "Run the task TASK in the project root without asking for extra args.
This is the default command when selecting/running a task/target."
  (taskrunner-run-task TASK))

(defun ivy-taskrunner--root-task-prompt (TASK)
  "Run the task TASK in the project root and ask the user for extra args."
  (taskrunner-run-task TASK nil t))

(defun ivy-taskrunner--current-dir (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Do not prompt the user to supply any extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      (taskrunner-run-task TASK (file-name-directory curr-file) nil))))

(defun ivy-taskrunner--current-dir-prompt (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Prompt the user to supply extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      (taskrunner-run-task TASK (file-name-directory curr-file) t))))

(defun ivy-taskrunner--check-if-in-project ()
  "Check if the currently visited buffer is in a project.
If it is not, prompt the user to select a project"
  ;; If we are not in a project, ask the user to switch to one
  (if (not (projectile-project-p))
      ;; If counsel is intalled, use that, otherwise use the default
      ;; projectile-switch-project interface. The command returns
      (if (package-installed-p 'counsel-projectile)
          (progn
            (require 'counsel-projectile)
            (counsel-projectile-switch-project))
        (projectile-switch-project))
    t))


(defun ivy-taskrunner--run-ivy-for-targets (TARGETS)
  (if (null TARGETS)
      (message ivy-taskrunner-no-targets-found-warning)
    (progn
      ;; Add extra actions for ivy
      (ivy-set-actions
       'ivy-taskrunner
       ivy-taskrunner-actions)
      
      ;; Run ivy
      (ivy-read "Task to run: "
                TARGETS
                :require-match t
                :action 'ivy-taskrunner--root-task))))

;;;###autoload
(defun ivy-taskrunner ()
  "Launch ivy to select a task to run in the current project.
This command runs asynchronously so the ivy prompt might not show
for several seconds."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  ;; Run the ivy interface only if a user selects a project.
  (if (projectile-project-p)
      (taskrunner-get-tasks-async 'ivy-taskrunner--run-ivy-for-targets)
    (message ivy-taskrunner-project-warning)))

;;;###autoload
(defun ivy-taskrunner-update-cache ()
  "Refresh the task cache for the current project and show all tasks."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  ;; Run the ivy interface only if a user selects a project.
  (if (projectile-project-p)
      (taskrunner-refresh-cache-async 'ivy-taskrunner--run-ivy-for-targets)
    (message ivy-taskrunner-project-warning)))

;;;###autoload
(defun ivy-taskrunner-rerun-last-command ()
  "Rerun the last task ran in the currently visited project."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-rerun-last-task (projectile-project-root))
    (message ivy-taskrunner-project-warning)))

;;;###autoload
(defun ivy-taskrunner-task-buffers ()
  "Show all ivy-taskrunner buffers."
  (interactive)
  (let ((taskrunner-buffers (taskrunner-get-compilation-buffers)))

    (if taskrunner-buffers
        (progn
          ;; Add extra actions
          (ivy-set-actions
           'ivy-taskrunner-task-buffers
           ivy-taskrunner-buffer-actions)
          (ivy-read "Buffer to open: "
                    taskrunner-buffers
                    :require-match t
                    :action 'switch-to-buffer))
      (message ivy-taskrunner-no-buffers-warning))))

;;;###autoload
(defun ivy-taskrunner-kill-all-buffers ()
  "Kill all ivy-taskrunner compilation buffers."
  (taskrunner-kill-compilation-buffers))

(defun ivy-taskrunner--open-file (FILENAME)
  "Open the file FILENAME.
This function is meant to be used with helm only."
  (setq ivy-taskrunner--project-files  (car (alist-get (intern FILENAME) ivy-taskrunner--project-files)))
  (find-file ivy-taskrunner--project-files))

(defun ivy-taskrunner--select-system (SYS)
  "Retrieve the files for the taskrunner/build system SYS."
  (setq ivy-taskrunner--project-files   (car (alist-get (intern SYS) ivy-taskrunner--project-files)))
  (if (stringp ivy-taskrunner--project-files)
      (find-file ivy-taskrunner--project-files)
    (ivy-read "Select a file: "
              (map 'list (lambda (elem)
                           (car elem))
                   ivy-taskrunner--project-files)
              :require-match t
              :action 'ivy-taskrunner--open-file)))

;;;###autoload
(defun ivy-taskrunner-config-files ()
  "Open the configuration files(if any are present) at project root."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  (setq ivy-taskrunner--project-files (taskrunner-collect-taskrunner-files (projectile-project-root)))
  (if ivy-taskrunner--project-files
      (ivy-read "Select build system: "
                (map 'list (lambda (elem)
                             (car elem))
                     ivy-taskrunner--project-files)
                :require-match t
                :action 'ivy-taskrunner--select-system)
    (message ivy-taskrunner-no-files-found-warning)))

(provide 'ivy-taskrunner)
;;; ivy-taskrunner.el ends here
