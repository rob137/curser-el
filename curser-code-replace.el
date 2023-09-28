(defvar gptel-directive-prefix  "You are a code editor. Rewrite my code more professionally. Only respond with the rewritten code - nothing more. Precisely follow the following instructions to edit the code: ")

(defun curser-code-replace (bounds)
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))))
  (let ((directive (concat gptel-directive-prefix (read-string "Code instructions: "))))
    (message "Processing. Please wait...")
    (gptel-request
     (buffer-substring-no-properties (car bounds) (cdr bounds)) ;the prompt
     :system directive
     :buffer (current-buffer)
     :context (cons (set-marker (make-marker) (car bounds))
                    (set-marker (make-marker) (cdr bounds)))
     :callback
     (lambda (response info)
       (if (not response)
           (message "ChatGPT response failed with: %s" (plist-get info :status))
         (let* ((bounds (plist-get info :context))
                (beg (car bounds))
                (end (cdr bounds))
                (buf (plist-get info :buffer)))
           (with-current-buffer buf
             (save-excursion
               (goto-char beg)
               (kill-region beg end)
               (insert response)
               (set-marker beg nil)
               (set-marker end nil)
               (message "Rewrote line. Original line saved to kill-ring.")))))))))
