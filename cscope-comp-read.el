(require 'projectile)
(require 'pulse)

(defgroup cscope-comp-read nil
  "Cscope with the interface of ivy."
  :group 'tools)

(defcustom cscope-comp-read-menu-alist '((symbol	.	0)
					 (definition	.	1)
					 (callee	.	2)
					 (caller	.	3)
					 (text		.	4)
					 (pattern	.	6)
					 (file		.	7)
					 (includer	.	8)
					 (assignment	.	9))
  "The mapping of cscope menu and index.

The index will be fed to the option \"-Lnum\"."
  :type '(alist :key-type symbol :value-type integer))

(defcustom cscope-comp-read-program "cscope"
  "The program name (or path) of cscope."
  :type 'string)

(defcustom cscope-comp-read-option-extra '("-k" "-q")
  "Extra options when running cscope."
  :type '(repeat string))

(defcustom cscope-comp-read-marker-ring-length 10
  "The length of the jumping history."
  :type 'integer)

(defvar cscope-comp-read-marker-ring
  (make-ring cscope-comp-read-marker-ring-length))

(defun cscope-comp-read--get-root-dir ()
  "Returns project root dir if a project is detected,
or `default-directory' otherwise."
  (or (projectile-project-root) default-directory))

(defun cscope-comp-read--do-search (menu query)
  "Search QUERY in MENU.

Returns cscope output if success, nil otherwise.  If error, the
output is appended to a buffer named \"*cscope-comp-read error*\".

See `cscope-comp-read-menu-alist' for possible menu items."
  (let ((default-directory (cscope-comp-read--get-root-dir))
	(menu-idx (alist-get menu cscope-comp-read-menu-alist)))
    (with-temp-buffer
      (if (= 0
	     (apply 'process-file cscope-comp-read-program nil t nil
		    (format "-L%d" menu-idx)
		    query
		    cscope-comp-read-option-extra))
	  (buffer-string)
	(append-to-buffer (get-buffer-create "*cscope-comp-read error*")
			  (point-min) (point-max))
	nil))))

(defun cscope-comp-read--parse-cscope-entry (entry)
  "Parse an entry of newline-split cscope output into a list of

    file path, function name, line number, line content.

Returns nil if error."
  (if (string-match
       "^\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)"
       entry)
      (let ((path (substring entry (match-beginning 1) (match-end 1)))
	    (func (substring entry (match-beginning 2) (match-end 2)))
	    (lnum (substring entry (match-beginning 3) (match-end 3)))
	    (line (substring entry (match-beginning 4) (match-end 4))))
	(list
	 (if (file-name-absolute-p path)
	     (concat (file-remote-p default-directory) path)
	   (concat (cscope-comp-read--get-root-dir) path))
	 func
	 (string-to-number lnum)
	 line))
    nil))

(defun cscope-comp-read--mark-current-position ()
  "Push current position into `cscope-comp-read-marker-ring'."
  (ring-insert-at-beginning cscope-comp-read-marker-ring
			    (cons (current-buffer) (point))))

(defun cscope-comp-read--pulse-momentarily ()
  "Give a visual pulse on the current line"
  (pcase-let ((`(,beg . ,end)
               (save-excursion
                 (or (back-to-indentation)
                     (if (eolp)
			 (cons (line-beginning-position) (1+ (point)))
                       (cons (point) (line-end-position)))))))
    (pulse-momentary-highlight-region beg end 'next-error)))

(defun cscope-comp-read--jump-to (method filename linum)
  (cscope-comp-read--mark-current-position)
  (funcall method filename)
  (goto-char (point-min))
  (forward-line (1- linum))
  (cscope-comp-read--pulse-momentarily))

(defun cscope-comp-read--select-entry (entry)
  (let ((ent (cscope-comp-read--parse-cscope-entry entry)))
    (when ent
      (cscope-comp-read--jump-to 'find-file (nth 0 ent) (nth 2 ent)))))


(defun cscope-comp-read--find (menu query &optional disable-fast-select)
  (let ((result (cscope-comp-read--do-search menu query)))
    (if (not result)
	(message "Error, see buffer *cscope-comp-read error*")
      (let ((col (split-string result "\n" t)))
	(if (and (not disable-fast-select) (= (length col) 1))
	    (cscope-comp-read--select-entry (car col))
	  (cscope-comp-read--select-entry 
	   (completing-read "Result: " col nil t)))))))


;;;###autoload
(defun cscope-comp-read-pop-mark ()
  "Jump back to the location before the last cscope-comp-read jump."
  (interactive)
  (when (not (ring-empty-p cscope-comp-read-marker-ring))
    (let* ((m (ring-remove cscope-comp-read-marker-ring))
	   (buf (car m))
	   (pos (cdr m)))
      (switch-to-buffer buf)
      (goto-char pos)
      (cscope-comp-read--pulse-momentarily))))

;;;###autoload
(defun cscope-comp-read-find-symbol (query)
  (interactive "sFind symbol: ")
  (cscope-comp-read--find 'symbol query t))

;;;###autoload
(defun cscope-comp-read-find-definition (query)
  (interactive "sFind definition: ")
  (cscope-comp-read--find 'definition query t))

;;;###autoload
(defun cscope-comp-read-find-definition-at-point ()
  "Find definition of the symbol at point."
  (interactive)
  (let ((sym (symbol-name (symbol-at-point))))
    (if (string-empty-p sym)
	(message "No symbol at point")
      (cscope-comp-read--find 'definition sym nil))))

;;;###autoload
(defun cscope-comp-read-find-callee (query)
  (interactive "sFind callee of: ")
  (cscope-comp-read--find 'callee query t))

;;;###autoload
(defun cscope-comp-read-find-caller (query)
  (interactive "sFind caller of: ")
  (cscope-comp-read--find 'caller query t))

;;;###autoload
(defun cscope-comp-read-find-text (query)
  (interactive "sSearch text: ")
  (cscope-comp-read--find 'text query t))

;;;###autoload
(defun cscope-comp-read-find-pattern (query)
  (interactive "sSearch for pattern: ")
  (cscope-comp-read--find 'pattern query t))

;;;###autoload
(defun cscope-comp-read-find-file (query)
  (interactive "sFind file: ")
  (cscope-comp-read--find 'file query t))

;;;###autoload
(defun cscope-comp-read-find-includer (query)
  (interactive "sFind files including: ")
  (cscope-comp-read--find 'includer query t))

;;;###autoload
(defun cscope-comp-read-find-assignment (query)
  (interactive "sFind assignment: ")
  (cscope-comp-read--find 'assignment query t))

;;;###autoload
(defvar cscope-comp-read-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'cscope-comp-read-find-symbol)
    (define-key map (kbd "d") 'cscope-comp-read-find-definition)
    (define-key map (kbd ".") 'cscope-comp-read-find-definition-at-point)
    (define-key map (kbd ",") 'cscope-comp-read-pop-mark)
    (define-key map (kbd "C") 'cscope-comp-read-find-callee)
    (define-key map (kbd "c") 'cscope-comp-read-find-caller)
    (define-key map (kbd "t") 'cscope-comp-read-find-text)
    (define-key map (kbd "e") 'cscope-comp-read-find-pattern)
    (define-key map (kbd "f") 'cscope-comp-read-find-file)
    (define-key map (kbd "i") 'cscope-comp-read-find-includer)
    (define-key map (kbd "=") 'cscope-comp-read-find-assignment)
    map)
  "Keymap for cscope-comp-read command.")

(provide 'cscope-comp-read)
