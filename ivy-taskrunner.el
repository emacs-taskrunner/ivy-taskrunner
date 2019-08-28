;;; ivy-taskrunner.el --- Retrieve build system/taskrunner tasks via ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/ivy-taskrunner
;; Version: 0.9
;; Package-Requires: ((emacs "25.1"))
;; Keywords: build-system taskrunner build task-runner tasks ivy convenience

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
;; (OPTIONAL) counsel-projectile

;; Then put this folder in your load-path, and put this in your init:

;; (require 'ivy-taskrunner)

;;;; Usage

;; When in any buffer recognized by projectile, call the command
;; `ivy-taskrunner' to launch an ivy menu which shows all possible tasks/targets
;; in the project.  If you add new tasks then call `ivy-taskrunner-update-cache'
;; to make sure that the newly added commands will be shown.  You can use the
;; command `ivy-taskrunner-task-buffers' to show all buffers which were used to
;; run a task.  If you would like to kill all buffers then you can use the
;; command `ivy-taskrunner-kill-all-buffers'.  Additionally, if you would like to
;; rerun the last ran command, use `ivy-taskrunner-rerun-last-command'.

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
(require 'cl-lib)

(defgroup ivy-taskrunner nil
  "Group for `ivy-taskrunner'."
  :prefix "ivy-taskrunner-"
  :group 'convenience)

;;;; Variables

;; Customizable Variables
(defcustom ivy-taskrunner-project-warning
  "The currently visited buffer must be in a project in order to select a task!
Please switch to a project which is recognized by projectile!"
  "Warning to indicate that a project must be visited to call `ivy-taskrunner'."
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

(defcustom ivy-taskrunner-no-buffers-warning
  "ivy-taskrunner: No taskrunner buffers are currently opened!"
  "Warning used to indicate that there are not task buffers opened."
  :group 'ivy-taskrunner
  :type 'string)

(defcustom ivy-taskrunner-command-history-empty-warning
  "ivy-taskrunner: Command history is empty!"
  "Warning used to indicate that the command history is empty for the project."
  :group 'ivy-taskrunner
  :type 'string)

(defcustom ivy-taskrunner-no-custom-commands-warning
  "ivy-taskrunner: There are no custom commands for this project!"
  "Warning used to indicate that there are no custom commands for the project.."
  :group 'ivy-taskrunner
  :type 'string)

(defcustom ivy-taskrunner-prompt-before-show nil
  "Whether or not to prompt the user before showing a `ivy-taskrunner' window."
  :group 'ivy-taskrunner
  :type 'boolean
  :options '(t nil))

(defcustom ivy-taskrunner-tasks-being-retrieved-warning
  "ivy-taskrunner: The tasks are currently being retrieved. They will be displayed when ready."
  "Warning used to indicate that the tasks are being retrieved.
This is only used when the minor mode is on."
  :group 'ivy-taskrunner
  :type 'string)

;; Variable aliases for customizable variables used in the backend
(defvaralias 'ivy-taskrunner-preferred-js-package-manager 'taskrunner-preferred-js-package-manager)
(defvaralias 'ivy-taskrunner-get-all-make-targets 'taskrunner-retrieve-all-make-targets)
(defvaralias 'ivy-taskrunner-gradle-heading-regexps 'taskrunner-gradle-heading-regexps)
(defvaralias 'ivy-taskrunner-build-dir-list 'taskrunner-build-dir-list)
(defvaralias 'ivy-taskrunner-source-dir-list 'taskrunner-source-dir-list)
(defvaralias 'ivy-taskrunner-go-task-bin-path 'taskrunner-go-task-bin-path)
(defvaralias 'ivy-taskrunner-mage-bin-path 'taskrunner-mage-bin-path)
(defvaralias 'ivy-taskrunner-doit-bin-path 'taskrunner-doit-bin-path)
(defvaralias 'ivy-taskrunner-no-previous-command-ran-warning 'taskrunner-no-previous-command-ran-warning)
(defvaralias 'ivy-taskrunner-command-history-size 'taskrunner-command-history-size)

;; Private variables
(defvar ivy-taskrunner--retrieving-tasks-p nil
  "Variable used to indicate if tasks are being retrieved in the background.")

(defvar ivy-taskrunner--tasks-queried-p nil
  "Variable used to indicate if the user queried for tasks before they were ready.")

(defvar ivy-taskrunner--project-files '()
  "Used to store the project files and their paths.")

(defvar ivy-taskrunner--project-cached-p nil
  "Stores the status of the project in the cache.
Used to enable prompts before displaying `ivy-taskrunner'.")

;; Users can add additional actions by appending to this variable
(defconst ivy-taskrunner-actions
  '(("r" ivy-taskrunner--root-task "Run task in root without extra args")
    ("R" ivy-taskrunner--root-task-prompt "Run task in root with extra args")
    ("c" ivy-taskrunner--current-dir "Run task in current folder without args")
    ("C" ivy-taskrunner--current-dir-prompt "Run task in current folder with args")
    ("s" ivy-taskrunner--select-dir "Run task in another directory")
    ("S" ivy-taskrunner--select-dir-prompt "Run task in another directory with args")
    ("a" ivy-taskrunner--customize-command "Customize command")
    ("D" ivy-taskrunner-delete-all-custom-commands "Delete all custom commands"))
  "A list of extra actions which can be used when running a task selected through ivy.")

(defconst ivy-taskrunner-buffer-actions
  '(("s" switch-to-buffer "Switch to buffer")
    ("k" ivy-taskrunner--kill-buffer "Kill buffer")
    ("K" ivy-taskrunner--kill-all-buffers "Kill all buffers"))
  "A list of extra actions used when selecting a compilation buffer through ivy.")

;;;; Functions

;; Minor mode related

;; TODO: There might be an issue if the user switches projects too quickly(as in
;; open one project and then directly open another). This might lead to the
;; caches being corrupted.

(defun ivy-taskrunner--projectile-hook-function ()
  "Collect tasks in the background when `projectile-switch-project' is called."
  (setq ivy-taskrunner--retrieving-tasks-p t)
  (taskrunner-get-tasks-async (lambda (TARGETS)
                                (setq ivy-taskrunner--retrieving-tasks-p nil)
                                ;; If the tasks were queried, show them to the user
                                (when ivy-taskrunner--tasks-queried-p
                                  (setq ivy-taskrunner--tasks-queried-p nil)
                                  (ivy-taskrunner--run-ivy-for-targets TARGETS)))
                              (projectile-project-root)))

;; Thanks to Marcin Borkowski for the `:init-value' tip
;; http://mbork.pl/2018-11-03_A_few_remarks_about_defining_minor_modes
;;;###autoload
(define-minor-mode ivy-taskrunner-minor-mode
  "Minor mode for asynchronously collecting project tasks when a project is switched to."
  :init-value nil
  :lighter " IvT"
  :global t
  ;; Add/remove the hooks when minor mode is toggled on or off
  (if ivy-taskrunner-minor-mode
      (add-hook 'projectile-after-switch-project-hook #'ivy-taskrunner--projectile-hook-function)
    (remove-hook 'projectile-after-switch-project-hook #'ivy-taskrunner--projectile-hook-function)))

;; Functions related to buffers
(defun ivy-taskrunner--kill-buffer (BUFFER-NAME)
  "Kill the buffer name BUFFER-NAME."
  (kill-buffer BUFFER-NAME))

(defun ivy-taskrunner--kill-all-buffers ()
  "Kill all `ivy-taskrunner' task buffers.
The argument TEMP is simply there since a ivy action requires a function with
one input."
  (taskrunner-kill-compilation-buffers))

;; Functions related to running tasks in a directory

(defun ivy-taskrunner--root-task (TASK)
  "Run the task TASK in the project root without asking for extra args.
This is the default command when selecting/running a task/target."
  (taskrunner-run-task TASK nil nil t))

(defun ivy-taskrunner--root-task-prompt (TASK)
  "Run the task TASK in the project root and ask the user for extra args."
  (taskrunner-run-task TASK nil t t))

(defun ivy-taskrunner--current-dir (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Do not prompt the user to supply any extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      (taskrunner-run-task TASK (file-name-directory curr-file) nil t))))

(defun ivy-taskrunner--current-dir-prompt (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Prompt the user to supply extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      (taskrunner-run-task TASK (file-name-directory curr-file) t t))))

(defun ivy-taskrunner--select-dir (TASK)
  "Run the task TASK in a directory chosen by the user."
  (let ((command-directory (read-directory-name "Directory: " (projectile-project-root))))
    (message command-directory)
    (when command-directory
      (taskrunner-run-task TASK command-directory nil))))

(defun ivy-taskrunner--select-dir-prompt (TASK)
  "Run the task TASK in a directory chosen by the user.
Prompt the user to supply extra arguments."
  (let ((command-directory (read-directory-name "Directory: " (projectile-project-root))))
    (when command-directory
      (taskrunner-run-task TASK command-directory t))))

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
            ;; This code will never be reached unless ivy-projectile is
            ;; installed but this is necessary in order to silence the
            ;; bytecompiler warning
            (when (fboundp 'counsel-projectile-switch-project)
              (counsel-projectile-switch-project)))
        (projectile-switch-project))
    t))


;; Add extra actions for main ivy instance
(ivy-set-actions
 'ivy-taskrunner
 ivy-taskrunner-actions)

(defmacro ivy-taskrunner--show-ivy-task-instance (TARGET-LIST)
  "Show an instance of `ivy' for TARGET-LIST."
  `(ivy-read "Task to run: "
             ,TARGET-LIST
             :require-match t
             :action 'ivy-taskrunner--root-task
             :caller 'ivy-taskrunner))

(defun ivy-taskrunner--run-ivy-for-targets (TARGETS)
  "Run an instance of `ivy' with TARGETS as candidates for selection.
If TARGETS is nil then show a warning to indicate that there are not targets."
  (if (null TARGETS)
      (message ivy-taskrunner-no-targets-found-warning)
    (if (and ivy-taskrunner-prompt-before-show
             ivy-taskrunner--project-cached-p)
        (when (y-or-n-p "Show ivy-taskrunner? ")
          (ivy-taskrunner--show-ivy-task-instance TARGETS))
      (ivy-taskrunner--show-ivy-task-instance TARGETS))))

;;;###autoload
(defun ivy-taskrunner ()
  "Launch `ivy' to select a task to run in the current project.
This command runs asynchronously so the ivy prompt might not show
for several seconds."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  ;; Run the ivy interface only if a user selects a project.
  (if (projectile-project-p)
      (progn
        (setq ivy-taskrunner--project-cached-p (not (taskrunner-project-cached-p (projectile-project-root))))
        (if (and ivy-taskrunner-minor-mode
                 ivy-taskrunner--retrieving-tasks-p)
            (progn
              (setq ivy-taskrunner--tasks-queried-p t)
              (message ivy-taskrunner-tasks-being-retrieved-warning))
          (taskrunner-get-tasks-async 'ivy-taskrunner--run-ivy-for-targets)))
    (message ivy-taskrunner-project-warning)))

;; Customizing Commands

(defun ivy-taskrunner--customize-command (COMMAND)
  "Customize the command COMMAND and add it to cache."
  (let* ((taskrunner-program (car (split-string COMMAND " ")))
         ;; Concat the arguments since we might be rerunning a command with arguments from history
         (task-name (mapconcat 'identity
                               (cdr (split-string COMMAND " ")) " "))
         (new-task-name (read-string "Arguments to add to command: " task-name)))
    (when new-task-name
      (taskrunner-add-custom-command (projectile-project-root) (concat taskrunner-program " " new-task-name))
      (when (y-or-n-p "Run new command? ")
        (taskrunner-run-task (concat taskrunner-program " " new-task-name) (projectile-project-root) nil t)))))

(defun ivy-taskrunner--delete-selected-command (COMMAND)
  "Remove the command COMMAND from the custom command cache."
  (when COMMAND
    (taskrunner-delete-custom-command (projectile-project-root) COMMAND)))

;;;###autoload
(defun ivy-taskrunner-delete-custom-command ()
  "Delete a custom command and remove it from the tasks output."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((custom-tasks (taskrunner-get-custom-commands (projectile-project-root))))
        (if custom-tasks
            (ivy-read
             "Command to remove: "
             custom-tasks
             :require-match t
             :action 'ivy-taskrunner--delete-selected-command)
          (message ivy-taskrunner-no-custom-commands-warning)))
    (message ivy-taskrunner-project-warning)))

;;;###autoload
(defun ivy-taskrunner-delete-all-custom-commands (&optional _)
  "Delete all custom commands for the currently visited project."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-delete-all-custom-commands (projectile-project-root))
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
  "Show all `ivy-taskrunner' buffers."
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
                    :action 'switch-to-buffer
                    :caller 'ivy-taskrunner-task-buffers))
      (message ivy-taskrunner-no-buffers-warning))))

;;;###autoload
(defun ivy-taskrunner-kill-all-buffers ()
  "Kill all `ivy-taskrunner' compilation buffers."
  (taskrunner-kill-compilation-buffers))

(defun ivy-taskrunner--open-file (FILENAME)
  "Open the file FILENAME.
This function is meant to be used with `ivy' only."
  (setq ivy-taskrunner--project-files  (car (alist-get (intern FILENAME) ivy-taskrunner--project-files)))
  (find-file ivy-taskrunner--project-files))

(defun ivy-taskrunner--select-system (SYS)
  "Retrieve the files for the taskrunner/build system SYS."
  (setq ivy-taskrunner--project-files   (car (alist-get (intern SYS) ivy-taskrunner--project-files)))
  (if (stringp ivy-taskrunner--project-files)
      (find-file ivy-taskrunner--project-files)
    (ivy-read "Select a file: "
              (cl-map 'list (lambda (elem)
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
                (cl-map 'list (lambda (elem)
                                (car elem))
                        ivy-taskrunner--project-files)
                :require-match t
                :action 'ivy-taskrunner--select-system)
    (message ivy-taskrunner-no-files-found-warning)))

;; Add extra actions for main ivy instance
(ivy-set-actions
 'ivy-taskrunner-history
 ivy-taskrunner-actions)

;;;###autoload
(defun ivy-taskrunner-command-history ()
  "Show the command history for the currently visited project."
  (interactive)
  (ivy-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((commands-ran (taskrunner-get-commands-from-history (projectile-project-root))))
        (if commands-ran
            (ivy-read "Command to run: "
                      commands-ran
                      :require-match t
                      :action 'ivy-taskrunner--root-task
                      :caller 'ivy-taskrunner-history)
          (message ivy-taskrunner-command-history-empty-warning)))
    (message ivy-taskrunner-project-warning)))

;; Notifications

;; If the compilation function is present then that means that the Emacs using
;; this package has notifications
(when (fboundp 'taskrunner--compilation-notification)
  (defun ivy-taskrunner-notifications-on ()
    "Turn on `ivy-taskrunner' desktop notifications when a task is finished."
    (interactive)
    (taskrunner-notification-on))

  (defun ivy-taskrunner-notifications-off ()
    "Turn off `ivy-taskrunner' desktop notifications when a task is finished."
    (interactive)
    (taskrunner-notification-off)))

(provide 'ivy-taskrunner)
;;; ivy-taskrunner.el ends here
