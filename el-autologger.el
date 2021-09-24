;;; el-autologger.el --- Convenient look-through of Function Results in Common Lisp -*- lexical-binding: t -*-

;;; Need to clean up the below code

(defun autologger-next-hashkey (hashkey)
  (let ((new-hashkey (copy-list hashkey)))
    (setf (car (last new-hashkey)) (+ (car (last new-hashkey)) 1))
    new-hashkey))

(defun autologger-prev-hashkey (hashkey)
  (let ((new-hashkey (copy-list hashkey)))
    (setf (car (last new-hashkey)) (- (car (last new-hashkey)) 1))
    new-hashkey))

(defun autologger-up-hashkey (hashkey)
  (butlast hashkey))

(defun autologger-down-hashkey (hashkey)
  (append hashkey '(1)))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Buffer_002dLocal.html

(defun autologger-kill-all-buffers ()
  (interactive)
  (kill-matching-buffers "^autologger" nil t)
  (pop-to-buffer "*slime-repl sbcl*"))

(defun autologger-bury-buffer ()
  (interactive)
  (bury-buffer))

(defun autologger-create-buffer (data-from-cl)
  (let* ((new-buffer (generate-new-buffer "autologger"))
	 (buffer-name (buffer-name new-buffer)))
    (with-current-buffer new-buffer
      (autologger-mode)
      (make-local-variable 'autologger-hash)
      (make-local-variable 'autologger-hashkey)
      (make-local-variable 'autologger-menu-data)
      (setq autologger-menu-mode nil)
      (setq autologger-hash (autologger-hash data-from-cl))
      (setq autologger-hashkey '(1))
      (autologger-draw-screen autologger-hashkey)
      (read-only-mode))
    (pop-to-buffer new-buffer)
    (message buffer-name)))




(global-set-key (kbd "C-c a") 'autologger-jump-to-buffer)

(defun autologger-jump-to-buffer (arg)
  (interactive "sEnter autologger buffer number to jump to (RET to go to newest): ")
  (cond ((equal arg "")
	 (let* ((buffer-names (loop for buffer in (buffer-list)
				    collect (buffer-name buffer)))
		(new-buffer-list (remove-if-not #'autologger-buffer-p buffer-names)))
	   (switch-to-buffer (car (sort new-buffer-list #'string-greaterp)))))
	(t
	 (if (or (equal arg "0")
		 (equal arg "1"))
	     (switch-to-buffer "autologger")
	   (switch-to-buffer (concat "autologger<" arg ">"))))))

(defun autologger-goto (arg)
  (interactive "nEnter number to jump to: ")
  (if autologger-menu-mode
      (progn
	(let ((line-pos (+ arg 11)))
	  (when (> line-pos 11)
	    (goto-char (point-min))   
	    (beginning-of-line line-pos))))
    (unless (null (gethash (append autologger-hashkey (list arg)) autologger-hash))
      (read-only-mode 'toggle)
      (setq autologger-hashkey (append autologger-hashkey (list arg)))
      (autologger-draw-screen autologger-hashkey)
      (read-only-mode))))

(defun autologger-goto-hashkey (hashkey num)
  (let ((new-hashkey (copy-list hashkey)))
    (setf (car (last new-hashkey)) num)
    new-hashkey))

(defun autologger-buffer-p (buffer-name)
  (when (string-match "^autologger" buffer-name)
    (unless (string-match-p (regex-quote ".") buffer-name)
      t)))

(defun autologger-down ()
  (interactive)
  (unless (null (gethash (autologger-down-hashkey autologger-hashkey) autologger-hash))
    (read-only-mode 'toggle)
    (setq autologger-hashkey (autologger-down-hashkey autologger-hashkey))
    (autologger-draw-screen autologger-hashkey)
    (read-only-mode)))

(defun autologger-up ()
  (interactive)
  (when (> (length autologger-hashkey) 1)
    (read-only-mode 'toggle)
    (setq autologger-hashkey (autologger-up-hashkey autologger-hashkey))
    (autologger-draw-screen autologger-hashkey)
    (read-only-mode)))

(defun autologger-next ()
  (interactive)
  (unless (null (gethash (autologger-next-hashkey autologger-hashkey) autologger-hash))
    (read-only-mode 'toggle)
    (setq autologger-hashkey (autologger-next-hashkey autologger-hashkey))
    (autologger-draw-screen autologger-hashkey)
    (read-only-mode)))

(defun autologger-prev ()
  (interactive)
  (when (> (car (last autologger-hashkey)) 1)
    (read-only-mode 'toggle)
    (setq autologger-hashkey (autologger-prev-hashkey autologger-hashkey))
    (autologger-draw-screen autologger-hashkey)
    (read-only-mode)))

(defun autologger-toggle ()
  (interactive)
  (let ((line-pos (line-number-at-pos)))
    (let ((new-menu-data
	   (cons (car autologger-menu-data)
		 (loop for item in (cdr autologger-menu-data)
		       collect (if (cdr item)
				   (cons (car item) nil)
				 (cons (car item) t))))))
      (setq autologger-menu-data new-menu-data)
      (read-only-mode 'toggle)
      (autologger-draw-menu autologger-menu-data)
      (read-only-mode 'toggle)
      (goto-char (point-min))
      (beginning-of-line line-pos))))

(defun autologger-item-toggle ()
  (interactive)
  (let ((line-pos (line-number-at-pos)))
    (when (> line-pos 11)
	(setf (cdr (nth (- line-pos 12)
		    (cdr autologger-menu-data)))
	  (not (cdr (nth (- line-pos 12)
			 (cdr autologger-menu-data)))))
	(read-only-mode 'toggle)
	(autologger-draw-menu autologger-menu-data)
	(read-only-mode 'toggle)
	(goto-char (point-min))
	(beginning-of-line lineq-pos))))


(defun autologger-menu-reset ()
  (interactive)
  (let ((line-pos (line-number-at-pos)))
    (let ((new-menu-data
	   (cons (car autologger-menu-data)
		 (loop for item in (cdr autologger-menu-data)
		       collect (cons (car item) nil)))))
      (setq autologger-menu-data new-menu-data)
      (read-only-mode 'toggle)
      (autologger-draw-menu autologger-menu-data)
      (read-only-mode 'toggle)
      (goto-char (point-min))
      (beginning-of-line line-pos))))


cl:eval (cl:read-from-string 

(defun autologger-menu-save ()
  (interactive)
  (let ((symbol-data (loop for item in (cdr autologger-menu-data)
			   collect (cons (list (format "%s" (first (car item)))
					       (format "%s" (second (car item))))
					 (cdr item)))))
    (slime-eval `(autologger::set-function-logs ',symbol-data))))

(define-derived-mode autologger-mode lisp-mode "autologger" :syntax-table nil
  "major mode for viewing common lisp function results."
  (use-local-map (copy-keymap text-mode-map))
  (local-set-key (kbd "u") #'autologger-up)
  (local-set-key (kbd "d") #'autologger-down)
  (local-set-key (kbd "n") #'autologger-next)
  (local-set-key (kbd "p") #'autologger-prev)
  (local-set-key (kbd "g") #'autologger-goto)
  (local-set-key (kbd "w") #'autologger-kill-all-buffers)
  (local-set-key (kbd "t") #'autologger-toggle)
  (local-set-key (kbd "i") #'autologger-item-toggle)
  (local-set-key (kbd "r") #'autologger-menu-reset)
  (local-set-key (kbd "s") #'autologger-menu-save)
  (local-set-key (kbd "k") #'(lambda ()
			       (interactive)
			       (forward-line -1)))
  (local-set-key (kbd "j") #'(lambda ()
			       (interactive)
			       (forward-line)))  
  (local-set-key (kbd "1") #'(lambda ()
			       (interactive)
			       (autologger-goto 1)))
  (local-set-key (kbd "2") #'(lambda ()
			       (interactive)
			       (autologger-goto 2)))
  (local-set-key (kbd "3") #'(lambda ()
			       (interactive)
			       (autologger-goto 3)))
  (local-set-key (kbd "4") #'(lambda ()
			       (interactive)
			       (autologger-goto 4)))
  (local-set-key (kbd "5") #'(lambda ()
			       (interactive)
			       (autologger-goto 5)))
  (local-set-key (kbd "6") #'(lambda ()
			       (interactive)
			       (autologger-goto 6)))
  (local-set-key (kbd "7") #'(lambda ()
			       (interactive)
			       (autologger-goto 7)))
  (local-set-key (kbd "8") #'(lambda ()
			       (interactive)
			       (autologger-goto 8)))
  (local-set-key (kbd "9") #'(lambda ()
			       (interactive)
			       (autologger-goto 9)))
  (local-set-key (kbd "q") #'(lambda ()
			       (interactive)
			       (kill-all-local-variables)
			       (kill-current-buffer)
			       (pop-to-buffer "*slime-repl sbcl*")))
  (make-local-variable 'autologger-menu-data)
  (make-local-variable 'autologger-menu-mode)
  ;; (use-local-map autologger-mode-map) -> will override all keymaps
  )

(defun autologger-endcons (a b)
  (reverse (cons b (reverse a))))

(defun autologger-draw-levels (list)
  (insert (propertize (format "%s" (car list)) 'font-lock-face 'bold))
  (loop for item in (cdr list)
	do (unless (null item)
	     (insert (propertize (format ".%s" item) 'font-lock-face 'bold) ))))

(defun autologger-recursive-draw-paths (hash-key counter)
  (let* ((new-key (autologger-endcons hash-key counter))
	 (hash-value (car (gethash new-key autologger-hash))))
    (when hash-value
      (insert (format "%s" counter) ": " (propertize (upcase (format "%s" (first hash-value)))
						     'font-lock-face '(:foreground "blue")))
      (newline)
      (autologger-recursive-draw-paths hash-key (+ counter 1)))))

(defun autologger-draw-inputs (hash-data)
  (loop for param-name in (second hash-data)
	for param-value in (fourth hash-data)
	do (insert (format "%s" param-name) " -> " (format "%s" param-value) "\n")))

;; Creating hash tables

(defun autologger-hash (data-from-cl)
  (let ((my-hash (make-hash-table :test #'equal)))
    (loop for item in data-from-cl
	  do (puthash (car item) (cons (cdr item) 0) my-hash))
    my-hash))

(defun autologger-draw-screen (hash-key)
  "Removes existing contents of buffer and inserts new screen based on hash-key supplied."
  (let ((hash-data (car (gethash hash-key autologger-hash))))
    (erase-buffer)
    (insert (propertize "~ Autologger v0.1 ~\n" 'font-lock-face 'bold))
    (insert (propertize "~ Utilising IBCL http://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/index.html ~\n"))
    (insert "~ Significant help & code refactoring by PJB & Beach (+ others) on #CLSCHOOL (IRC.LIBERA.CHAT) ~\n\n")
    (insert (propertize "u" 'font-lock-face '(bold (:foreground "blue"))) ": up    "
	    (propertize "d" 'font-lock-face '(bold (:foreground "blue"))) ": down  "
	    (propertize "g" 'font-lock-face '(bold (:foreground "blue"))) ": goto  "
	    (propertize "m" 'font-lock-face '(bold (:foreground "blue"))) ": minimise-this-buffer\n")
    (insert (propertize "n" 'font-lock-face '(bold (:foreground "blue"))) ": next  "
	    (propertize "p" 'font-lock-face '(bold (:foreground "blue"))) ": prev  "
            (propertize "q" 'font-lock-face '(bold (:foreground "blue"))) ": quit  "
	    (propertize "w" 'font-lock-face '(bold (:foreground "blue"))) ": kill-all-autologger-buffers\n")
    (newline 2)
    (insert "= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =\n")
    (insert (propertize "Autologger for " 'font-lock-face 'bold)
	    (propertize (upcase (format "%s" (first hash-data))) 'font-lock-face '('bold (:foreground "blue")))
	    (propertize " on level " 'font-lock-face 'bold))
    (autologger-draw-levels hash-key)
    (newline)
    (insert "= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =\n")
    (newline)
    (insert "Outputs\n")
    (insert "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n")
    (insert (format "%s\n" (fifth hash-data)))
    (newline 1)    
    (insert "Inputs\n")
    (insert "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n")
    (autologger-draw-inputs hash-data)
    (newline 2)
    (insert "Step Into (for numbers > 9, use goto via " (propertize "g" 'font-lock-face 'bold) ")\n")
    (insert "- - - - - - - - - - - - - - - - - - - - - - - - - - -\n")
    (autologger-recursive-draw-paths hash-key 1)
    (newline 2)
    (insert "Function Definition\n")
    (insert "- - - - - - - - - - - - - - - - - - - - - - - - - - -")
    (cl-prettyprint (append (list 'defun
				  (first hash-data)
				  (second hash-data))
			    (third hash-data)))
    (goto-char (point-min)))
    ;; Ensure autologger loads in Emacs state for Evil:
  ;; Test if this is required
  (add-hook 'autologger-mode-hook 'turn-on-font-lock))


(defun autologger-create-menu-buffer (data-from-cl)
    (let* ((new-buffer (generate-new-buffer "autologger"))
	 (buffer-name (buffer-name new-buffer)))
    (with-current-buffer new-buffer
      (autologger-mode)
      (setq autologger-menu-mode t)
      (setq autologger-menu-data data-from-cl)
      (autologger-draw-menu autologger-menu-data)
      (read-only-mode))
    (pop-to-buffer new-buffer)
    (message buffer-name)))

(defun autologger-draw-menu (menu-data)
  (erase-buffer)
  (insert (propertize "~ Autologger v0.1 ~\n" 'font-lock-face 'bold))
  (insert (propertize "~ Utilising IBCL http://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/index.html ~\n"))
  (insert "~ Significant help & code refactoring by PJB & Beach (+ others) on #CLSCHOOL (IRC.LIBERA.CHAT) ~\n\n")
  (insert (propertize "j" 'font-lock-face '(bold (:foreground "blue"))) ": down  "
 	  (propertize "t" 'font-lock-face '(bold (:foreground "blue"))) ": toggle-all   "
	  (propertize "r" 'font-lock-face '(bold (:foreground "blue"))) ": unlog-all  "
	  (propertize "g" 'font-lock-face '(bold (:foreground "blue"))) ": goto  "
	  (propertize "m" 'font-lock-face '(bold (:foreground "blue"))) ": minimise-this-buffer\n")
  (insert (propertize "k" 'font-lock-face '(bold (:foreground "blue"))) ": up    "
	  (propertize "i" 'font-lock-face '(bold (:foreground "blue"))) ": toggle-item  "
	  (propertize "s" 'font-lock-face '(bold (:foreground "blue"))) ": save       "
          (propertize "q" 'font-lock-face '(bold (:foreground "blue"))) ": quit  "
	  (propertize "w" 'font-lock-face '(bold (:foreground "blue"))) ": kill-all-autologger-buffers\n")
  (newline 2)
  (insert "= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =\n")
  (insert "Logging Menu for " (propertize (upcase (format "%s" (car menu-data))) 'font-lock-face '('bold (:foreground "blue"))))
  (insert " (" (propertize "purple" 'font-lock-face '(:foreground "purple"))
	  " for logged functions and " (propertize "blue" 'font-lock-face '(:foreground "blue"))
	  " for unlogged functions)")
  (newline 1)
  (loop for item in (cdr menu-data)
	for index from 1 to (length (cdr menu-data))
	do (progn
	     (newline)
	     (if (cdr item)
		 (progn
		   (insert (format "%s:" index) (propertize (upcase (concat (second (car item)) ":"
									  (when (equal (third (car item)) ":INTERNAL") ":")
									  (first (car item)))) 'font-lock-face '(:foreground "purple"))))
	       (insert (format "%s:" index) (propertize (upcase (concat (second (car item)) ":"
									  (when (equal (third (car item)) ":INTERNAL") ":")
									  (first (car item)))) 'font-lock-face '(:foreground "blue"))))))
  (goto-char (point-min))
  (beginning-of-line 12))



(add-to-list 'evil-emacs-state-modes 'autologger-mode)



(provide 'autologger-mode)

;;; autologger.el ends here

;; (defvar autologger-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "u") #'autologger-up)
;;     (define-key map (kbd "d") #'autologgger-down)
;;     (define-key map (kbd "n") #'autologger-next)
;;     (define-key map (kbd "p") #'autologger-prev)
;;     (define-key map (kbd "g") #'autologger-goto)
;;     (define-key map (kbd "q") #'autologger-quit)
;;     map))


;; 13.0 Load into Slime - > - > - > - >

(defun math-mode-load-mathql-data ()
  (interactive)
  (let ((output (math-mode-get-data)))
    (slime-eval `(mql::holder-function ',(math-mode-get-data)))))





;; Font Locks


;; Define Minor Mode

(define-minor-mode math-mode
  "A Math Entry & Typesetting Minor Mode."
  :lighter " MM"
  :keymap math-mode-map
  (if math-mode
      (font-lock-add-keywords nil math-mode-highlights 'at-end)
    (font-lock-remove-keywords nil math-mode-highlights))
  (font-lock-flush))


