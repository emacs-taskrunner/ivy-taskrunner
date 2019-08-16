;; Test interface for taskrunner

(require 'ivy)
(require 'taskrunner)

;; Users can add additional actions by appending to this variable
(defvar ivy-taskrunner-actions
  '(("r" ivy-taskrunner--root-task "Run task in root without extra args")
    ("R" ivy-taskrunner--root-task-prompt "Run task in root with extra args")
    ("c" ivy-taskrunner--current-dir "Run task in current folder without args")
    ("C" ivy-taskrunner--current-dir-prompt "Run task in current folder with args")
    )
  "A list of extra actions which can be used when running a task selected through ivy.")

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

(defun ivy-taskrunner ()
  "Launch ivy to select a task to run in the current project."
  (interactive)
  (let ((in-project-p (projectile-project-p)))
    ;; If we are not in a project, ask the user to switch to one
    (if (not in-project-p)
        ;; If counsel is intalled, use that, otherwise use the default
        ;; projectile-switch-project interface. The command returns
        (if (package-installed-p 'counsel)
            (setq in-project-p
                  (progn
                    (require 'counsel)
                    (counsel-projectile-switch-project)))
          (setq in-project-p (projectile-switch-project))))

    ;; Run the ivy interface only if a user selects a project. If the user
    ;; leaves the projectile-switch-project prompt then there is nothing
    ;; returned and the value stays nil.
    (when in-project-p
      ;; Add extra actions
      (ivy-set-actions
       'ivy-taskrunner
       ivy-taskrunner-actions)
      
      ;; Run ivy
      (ivy-read "Task to run: "
                (taskrunner-get-tasks-from-cache)
                :require-match t
                :action 'ivy-taskrunner--root-task))
    )
  )

(provide 'ivy-taskrunner)
;;; ivy-taskrunner.el ends here
