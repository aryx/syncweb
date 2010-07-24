
(defvar hide-regexp 
  "list of regexps specifying what to hide"
  nil)

(defun hide-regexp ()
   (interactive)
   (activate-hide-regexp)
   (save-excursion
     (mapcar (lambda (e)
       ;simple:     (goto-char (point-min))
       ;simple:     (while (re-search-forward "history:" nil t)
       ;simple:       (let ((ov (make-overlay (progn (beginning-of-line) (point)) (progn (end-of-line) (point)))))
	 (goto-char (point-min))
	 (while (re-search-forward (car e) nil t)
;	   (message (match-string 0))
; if = overview | index | ... then do nothing (special case cos sometimes want see them (and only them)
	   (let* ((matched (match-string 0))
		 (ov (make-overlay 
;		      (save-excursion (beginning-of-line) (point)) ; customize: if want hide all (good for flyspell)
		      (point)
		      (if (cdr e) 
			(progn (re-search-forward (cdr e) nil t) (point)) 
			;(progn (end-of-line) (point)) ; customize: if dont want pack together keywords
			(progn (forward-line)
			       (while (re-search-forward ;(car e) 
				       matched
				       (save-excursion (end-of-line) (point)) t)
				 (forward-line))
			       (forward-line -1)
			       (end-of-line)
			       (point))
			))))
   	     ; if outline is activated then buffer-invisibility-spec is changed
	     ; and so put t for 'invisible is not enough
	     (overlay-put ov 'invisible 'hr)
	     ; want tag this overlay so can differientiate when have to
 	     ; delete them to avoid delete other overlay (such as outline
	     ; one, selective-display, ...)
	     (overlay-put ov 'invisible 'hr)
	     (overlay-put ov 'evaporate t) ; if region to hide is empty then do nothing (useful for %plan: in latex)
	     (overlay-put ov 'hr t) 
	     (overlay-put ov 'after-string "...")
	     )))
	hide-regexp-alist   
	)
     )
   )
 
(defun activate-hide-regexp ()
  (interactive)
;  (add-to-list 'buffer-invisibility-spec '(hr . nil))
  (setq buffer-invisibility-spec '((hr . nil) (outline . t)))
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t) ;src: hide-search.el 
  )

(defun unhide-regexp ()
  (interactive)
  (mapcar (lambda (ov)
	       (when (overlay-get ov 'hr) (delete-overlay ov) ; dont touch other overlays
	       )) 
	  (overlays-in (point-min) (point-max)))
  )

(provide 'hide-regexp)
