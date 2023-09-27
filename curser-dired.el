(defvar global-marked-files nil
  "List of marked files across all modes.")

(defun list-global-marked-files ()
  "List marked files stored in the global variable."
  (interactive)
  (if global-marked-files
      (with-output-to-temp-buffer "*Marked Files*"
        (dolist (file global-marked-files)
          (princ (format "%s\n" file))))
    (message "No globally marked files.")))

(defun add-current-file-to-global-marked ()
  "Add the current file to the global list of marked files."
  (interactive)
  (when-let ((current-file (buffer-file-name)))
    (unless (member current-file global-marked-files)
      (add-to-list 'global-marked-files current-file))
    (message "Added: %s" current-file))
  (unless (buffer-file-name)
    (message "Not a file buffer.")))

(global-set-key (kbd "C-c M") 'add-current-file-to-global-marked)

(defun project-add-file-to-global-marked ()
  "Use project-find-file to add a file to the global marked list."
  (interactive)
  (when-let ((current-project-root (cdr (project-current))))
    (let* ((file (project-find-file nil nil current-project-root))
           (full-file (concat current-project-root file)))
      (unless (member full-file global-marked-files)
        (add-to-list 'global-marked-files full-file))
      (message "Added: %s" full-file)))
  (unless (cdr (project-current))
    (message "Not in a project.")))

(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "l") 'project-add-file-to-global-marked))

(defun my-dired-unmark-all-files ()
  "Unmark all marked files in all Dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'dired-mode)
        (dired-unmark-all-marks)))))

