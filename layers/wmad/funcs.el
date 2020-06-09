;; Upload to Netsuite:
(defun wmad/upload-to-netsuite ()
  "Send buffer to Netsuite."
  (interactive)
  (let ((cmd (concat "ns-upload" " " (buffer-file-name))))
    (message (shell-command-to-string cmd))
    ))
;; Upload to Netsuite ends here

;; JIRA replace:
(defun wmad/replace-jira ()
  (interactive)
  (move-beginning-of-line 1)
  (replace-string "~" "" )
  (move-beginning-of-line 1)
  (replace-string "{anchor}" "_" ))
;; JIRA replace: ends here

;; Duplicate line:
(defun wmad/duplicate-line ()
  (interactive)
  (let* ((cursor-column (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)
    (move-to-column cursor-column)))
;; Duplicate line: ends here
