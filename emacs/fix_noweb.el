(require 'noweb-mode)

; By default noweb tries to be lazy and font-lockify chunks or latex parts
; only when the cursor is on the chunk. This function allows instead to force
; the font-lockifying when opening the file.
(defun pad-noweb-force-mode-all-file ()
  (interactive)
  (message "hook")
  (save-excursion
    (while (not (eobp))
      (next-line-nomark)
      (noweb-select-mode)
      )
    ))

(setq noweb-mode-hook 'pad-noweb-force-mode-all-file)

; The default behavior of newline in noweb is electric, I hate it.
(define-key noweb-minor-mode-map  [return] 'newline)


; trick to hide those special comments. need hide-regexp.el 
(require 'hide-regexp)

; for ocaml (tuareg-mode), you can easily extend it to other langauges.
(setq hide-regexp-alist  
      '(
        ("\<(* nw_s: " . nil) 
        ("(* nw_e: " . nil)
        ("(* nw_s: .*|" . nil) 
        ("(*s: " . nil)
        ("(*e: " . nil) 
;        ("(* nw_e: .*|" . nil) dont need for the end
        ))

(add-hook 'tuareg-mode-hook (lambda () (hide-regexp)))

; better colorization of syncweb marks, less intrusive
(defun Set-face-foreground (f c) (make-face f) (set-face-foreground f c))
(Set-face-foreground 'pad-syncweb-face "DimGray")

; Mostly copy paste of tuareg.el. Modified for my syncweb fontification.
; Could not do it via a regular font-lock-add-keywords as in:
;   (font-lock-add-keywords 'tuareg-mode
;                        '(("\\((\*s:.*\\)"
;                               1 font-lock-builtin-face prepend)))
; because the function below takes precedence over font-lock :(
(defun tuareg-fontify (begin end)
    (if (eq major-mode 'tuareg-mode)
	(save-excursion
	  (tuareg-modify-syntax)

	  (let ((case-fold-search nil)
		(modified (buffer-modified-p))) ; Emacs hack (see below)
	    (goto-char begin)
	    (beginning-of-line)
	    (setq begin (point))
	    (goto-char (1- end))
	    (end-of-line)
	    ;; Dirty hack to trick `font-lock-default-unfontify-region'
	    (if (not tuareg-with-xemacs) (forward-line 2))
	    (setq end (point))

	    (while (> end begin)
	      (goto-char (1- end))
	      (tuareg-in-literal-or-comment)
	      (cond
	       ((cdr tuareg-last-loc)
		(tuareg-beginning-of-literal-or-comment)
		(put-text-property (max begin (point)) end 'face
                  ; pad: monkey patch
                  (if (looking-at
                       "(\\*[se]:")
                      'pad-syncweb-face
                    (if (looking-at
                         "(\\*[Tt][Ee][Xx]\\|(\\*\\*[^*]")
                        tuareg-doc-face
                      'font-lock-comment-face)))
		(setq end (1- (point))))
	       ((car tuareg-last-loc)
		(tuareg-beginning-of-literal-or-comment)
		(put-text-property (max begin (point)) end 'face
				   'font-lock-string-face)
		(setq end (point)))
	       (t (while (and tuareg-cache-local
			      (or (> (caar tuareg-cache-local) end)
				  (eq 'b (cadar tuareg-cache-local))))
		    (setq tuareg-cache-local (cdr tuareg-cache-local)))
		  (setq end (if tuareg-cache-local
				(caar tuareg-cache-local) begin)))))
	    (if (not (or tuareg-with-xemacs modified)) ; properties taken
		(set-buffer-modified-p nil)))          ; too seriously...

	  (tuareg-restore-syntax))))
