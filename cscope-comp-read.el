;; Dependencies
;; - projectile.el (for discovering project root)
;; - pulse.el (part of Emacs)

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
  "Extra options when running cscope.

For example, use \"-d\"
additionally to disable the auto-rebuild to keep the query time
low especially for large projects."
  :type '(repeat string))

(defcustom cscope-comp-read-marker-ring-length 10
  "The length of the jumping history."
  :type 'integer)

(defcustom cscope-comp-read-candidate-face-path nil
  "Face of file path when displaying candidates."
  :type 'face)

(defcustom cscope-comp-read-candidate-face-lnum nil
  "Face of line number when displaying candidates."
  :type 'face)

(defcustom cscope-comp-read-candidate-face-func
  'font-lock-function-name-face
  "Face of function name when displaying candidates."
  :type 'face)

(defcustom cscope-comp-read-candidate-face-line nil
  "Face of full line when displaying candidates."
  :type 'face)

(defvar cscope-comp-read-marker-ring
  (make-ring cscope-comp-read-marker-ring-length))

(defun cscope-comp-read--get-root-dir ()
  "Returns project root dir if a project is detected,
or `default-directory' otherwise."
  (projectile-project-root))

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

(defun cscope-comp-read--list-to-candidate (l)
  (string-join l " "))
(defun cscope-comp-read--candidate-to-list (cand)
  (split-string cand " " t))

(defun cscope-comp-read--propertize (entry)
  "Propertize an entry of newline-split cscope output."
  (if (string-match
       "^\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)"
       entry)
      (let ((path (substring entry (match-beginning 1) (match-end 1)))
	    (lnum (substring entry (match-beginning 3) (match-end 3)))
	    (func (substring entry (match-beginning 2) (match-end 2)))
	    (line (substring entry (match-beginning 4) (match-end 4))))
	(cscope-comp-read--list-to-candidate
	 (list
	  (propertize path 'face cscope-comp-read-candidate-face-path)
	  (propertize lnum 'face cscope-comp-read-candidate-face-lnum)
	  (propertize func 'face cscope-comp-read-candidate-face-func)
	  (propertize line 'face cscope-comp-read-candidate-face-line))))))

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
  (let* ((l (cscope-comp-read--candidate-to-list entry))
	 (path (nth 0 l))
	 (fullpath (if (file-name-absolute-p path)
		       (concat (file-remote-p default-directory) path)
		     (concat (cscope-comp-read--get-root-dir) path)))
	 (lnum (string-to-number (nth 1 l))))
    (cscope-comp-read--jump-to 'find-file fullpath lnum)))

(defun cscope-comp-read--select-result (result fast-select)
  (let ((candidates (mapcar
		     'cscope-comp-read--propertize
		     (split-string result "\n" t))))
    (if (and fast-select (= (length candidates) 1))
	(cscope-comp-read--select-entry (car candidates))
      (cscope-comp-read--select-entry
       (completing-read "Result: " candidates nil t)))))

(defun cscope-comp-read--find (menu query fast-select)
  (let ((result (cscope-comp-read--do-search menu query)))
    (if (not result)
	(message "Error, see buffer *cscope-comp-read error*")
      (cscope-comp-read--select-result result fast-select))))

(defun cscope-comp-read--find-at-point (menu fast-select)
  (let ((query (symbol-name (symbol-at-point))))
    (if (string-empty-p query)
	(message "No symbol at point")
      (cscope-comp-read--find menu query fast-select))))

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
(defun cscope-comp-read-find-symbol (&optional prefix)
  "Find symbol using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'symbol (read-string "Find symbol: ") t)
    (cscope-comp-read--find-at-point 'symbol t)))

;;;###autoload
(defun cscope-comp-read-find-definition (&optional prefix)
  "Find definition using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'definition (read-string "Find definition: ") t)
    (cscope-comp-read--find-at-point 'definition t)))

;;;###autoload
(defun cscope-comp-read-find-callee (&optional prefix)
  "Find callee using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'callee (read-string "Find callee of: ") t)
    (cscope-comp-read--find-at-point 'callee t)))

;;;###autoload
(defun cscope-comp-read-find-caller (&optional prefix)
  "Find caller using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'caller (read-string "Find caller of: ") t)
    (cscope-comp-read--find-at-point 'caller t)))

;;;###autoload
(defun cscope-comp-read-find-text (&optional prefix)
  "Find text using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'text (read-string "Search text: ") t)
    (cscope-comp-read--find-at-point 'text t)))

;;;###autoload
(defun cscope-comp-read-find-pattern (&optional prefix)
  "Find pattern using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'pattern (read-string "Search for pattern: ") t)
    (cscope-comp-read--find-at-point 'pattern t)))

;;;###autoload
(defun cscope-comp-read-find-file (&optional prefix)
  "Find file using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'file (read-string "Find file: ") t)
    (cscope-comp-read--find-at-point 'file t)))

;;;###autoload
(defun cscope-comp-read-find-includer (&optional prefix)
  "Find includer using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'includer (read-string "Find files including: ") t)
    (cscope-comp-read--find-at-point 'includer t)))

;;;###autoload
(defun cscope-comp-read-find-assignment (&optional prefix)
  "Find assignment using cscope.
Without PREFIX, find the symbol at point.
When PREFIX, prompt for the cscope query."
  (interactive "P")
  (if prefix
      (cscope-comp-read--find 'assignment (read-string "Find assignment: ") t)
    (cscope-comp-read--find-at-point 'assignment t)))

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
