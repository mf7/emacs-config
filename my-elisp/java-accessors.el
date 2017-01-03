(setq gets-sets nil)
(setq class-vars nil)

(defun create-accessors ()
  "Create getters and setters"
  (interactive)
  (setq gets-sets nil)
  (setq class-vars nil)
  (match-fields)
  (goto-char (point-min))
  (insert (concat (get-class-vars) "
"))
  )

(defun match-fields ()
	""
	(interactive)
	(goto-char (point-min))
	(while (re-search-forward "private \\([^ ]+\\) \\([^ ;]+\\);\\(.*\\)" nil t)
	       (replace-match "private \\1 \\2; \\3

	public void set\\2(\\1 \\2){
		this.\\2 = \\2;
	}

	public \\1 get\\2(){
		return this.\\2;
	}
")
	)
	(goto-char (point-min))
	(while (re-search-forward "set\\(.\\)\\([^(]+\\)(" nil t)
	       (replace-match (concat "set" (upcase (match-string 1)) (match-string 2) "(") t nil)
	)
	(goto-char (point-min))
	(while (re-search-forward "get\\(.\\)\\([^(]+\\)(" nil t)
	       (replace-match (concat "get" (upcase (match-string 1)) (match-string 2) "(") t nil)
	)
	(goto-char (point-min))
	(while (re-search-forward "\\(private .*\\)\n" nil t)
	       (replace-match (progn (add-to-list 'class-vars (match-string 1)) "") t nil)
	)
)
