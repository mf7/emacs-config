(defun kill-whole-line nil
  "kills the entire line on which the cursor is located, and places the
cursor as close to its previous position as possible."
  (interactive)
  (progn
    (let ((y (current-column))
      (a (progn (beginning-of-line) (point)))
      (b (progn (forward-line 1) (point))))
      (kill-region a b)
      (move-to-column y))))

(fset 'dd 'kill-whole-line)


;; opens the file highlighted (uses the current dir to start with)
(fset 'of
   [?\M-w ?\C-x ?\C-f ?\C-y return])



;;add ctrl-TAB functionality
(global-set-key (kbd "<C-tab>") '(lambda () (interactive)
  (switch-to-buffer (other-buffer))))
	

(defun word-count nil "Count words in buffer" (interactive)
(shell-command-on-region (point-min) (point-max) "wc"))

(fset 'wc 'word-count)

(defun cf-comment-region ()
  "Insert a Coldfusion comment around a region."
  (interactive)
    (goto-char (region-end)) (insert "--->")
    (goto-char (region-beginning)) (insert "<!---")
)

(defun tag-surround-region (str)
  "Insert a tag around a region."
  (interactive "sTag name:")
    (goto-char (region-end)) (insert "</" str ">")
    (goto-char (region-beginning)) (insert "<" str ">")
)

(defun tag-insert (str txt)
  "Insert a tag"
  (interactive "sTag name: \nsText: ")
  (insert "<" str ">")
  (insert txt)
  (set-mark (point))
  (insert "</" str ">")
  (goto-char (region-beginning))
)

(defun tag-insert-single (str)
  "Insert a tag around a region."
  (interactive "sTag name:")
  (insert "<" str " ")
  (set-mark (point))
  (insert " />")
  (goto-char (region-beginning))
)

(global-set-key (kbd "<f10>") 'tag-insert-single)
(global-set-key (kbd "<f11>") 'tag-insert)
(global-set-key (kbd "<f12>") 'tag-surround-region)

(fset 'para
   "\355<p>\C-e</p>")


(defun paragraph-region ()
	"Replaces funny chars in text that is going to end up as HTML"
	(interactive)
	(goto-char (point-min))
	(save-excursion 
		(while (re-search-forward "^\\(.+\\)$" nil t)
	    	   (replace-match "<p>\\1</p>")))
)

(defun replace-special-chars-text ()
	"Replaces funny chars in text that is going to end up as HTML"
	(interactive)
	(goto-char (point-min))
	(while (re-search-forward "&" nil t)
	       (replace-match "&amp;"))
	(goto-char (point-min))
	(while (re-search-forward "’" nil t)
	       (replace-match "&rsquo;"))
	(goto-char (point-min))
	(while (re-search-forward "‘" nil t)
	       (replace-match "&lsquo;"))
	(goto-char (point-min))
	(while (re-search-forward "–" nil t)
	       (replace-match "-"))
	(goto-char (point-min))
	(while (re-search-forward "£" nil t)
	       (replace-match "&pound;"))
	(goto-char (point-min))
	(while (re-search-forward "“" nil t)
	       (replace-match "&ldquo;"))
	(goto-char (point-min))
	(while (re-search-forward "”" nil t)
	       (replace-match "&rdquo;"))
	(goto-char (point-min))
	(while (re-search-forward "'" nil t)
	       (replace-match "&rsquo;"))
)

(defun prelude-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))


(defun cf-clean-tags (begin end)
  "Close tags off corrctly with />"
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (re-search-forward "\(<input[^>]+[^/]\)>" nil t)
      (replace-match (match-string 1 " />") t nil)
      (backward-char) (insert "\n") (incf end))
    (indent-region begin end nil)
    ;;(normal-mode)
    )
  (message "All cleaned!"))



(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defalias 'xml-format 'bf-pretty-print-xml-region)

(defun shell-command-on-buffer (command)
  (interactive "sShell command on buffer: ")
	(let ((full-command (concat command " " (buffer-file-name))))
		  (message (concat "Running:" full-command))
		  (shell-command full-command (get-buffer-create "*Shell Command Output*"))))

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))


(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun wpath ()
	"Simply replaces forward slashes with back slashes"
	(interactive)
	(goto-char (point-min))
	(save-excursion 
		(while (re-search-forward "\/" nil t)
	    	   (replace-match "\\\\")))
)



(defun poor-richard ()
  "Echos a line from the poor richard file...for a gentleman's improvement"
  (interactive)
  	(message (cookie richard-file "starting..." "ending..."))
)
(defalias 'pr 'poor-richard)
(global-set-key (kbd "C-x p") 'poor-richard)

 ;; Enable ibuffer-filter-by-filename to filter on directory names too.
    (eval-after-load "ibuf-ext"
      '(define-ibuffer-filter filename
         "Toggle current view to buffers with file or directory name matching QUALIFIER."
         (:description "filename"
          :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
         (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                            (buffer-local-value 'dired-directory buf))
           (string-match qualifier it))))


(defalias 'http 'get-http-code-description)
(defun get-http-code-description (status-code)
  	"Returns the text for the given http code"
	(interactive "sStatus code:")
	(load http-codes-file)
	(message "%s" (cdr (assoc status-code http_codes)))
)


(defun run-wget (url-string)
  "Runs the wget command"
  (shell-command (concat "wget --output-document=" (expand-file-name "~") "/tmp-wget " url-string) (get-buffer-create "foo")))

(shell-command "pwd" (get-buffer-create "foo"))


  
(run-wget "www.google.com")
