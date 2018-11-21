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
    "https://duckduckgo.com/?q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "DuckDuckGo: ")))))


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

;;(shell-command "pwd" (get-buffer-create "foo"))


  
;;(run-wget "www.google.com")


(defun java-lookup ()
  "Searches duckduckgo, and then picks the first result "
  (interactive)
  (browse-url
   (concat
    "https://duckduckgo.com/?q=! java javadoc "
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "DuckDuckGo: ")))))

(defun aws-lookup ()
  "Searches aws for the server"
  (interactive)
  (browse-url
   (concat
    "https://eu-west-1.console.aws.amazon.com/ec2/v2/home?region="
	(read-string "AWS region: " "eu-west-1" nil '("eu-west-1" "us-east-1"))
	"#Instances:search="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "AWS Instance: "))
	";sort=tag:Name")))

(defun youtube-lookup ()
  "Searches youtube for the given text"
  (interactive)
    (browse-url
   (concat
    "https://www.youtube.com/results?search_query="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Search: ")))))


;; Calvin and Hobbes modules
(defun kill-if-buffer-exists (bufname)
  (when (get-buffer bufname)
	(kill-buffer bufname)))

(defun get-calvin ()
  "Fetch the most recent Calvin image and show it in the buffer."
  (interactive)
  (save-excursion
	(let ((cmd "/usr/bin/wget -q -P /tmp/calvin http://calvinhobbesdaily.tumblr.com/rss > /dev/null 2>&1"))
	  (shell-command "rm -rf /tmp/calvin")
	  (kill-if-buffer-exists "rss")
	  (kill-if-buffer-exists "*calvin*")
	  (shell-command cmd)
	  (find-file "/tmp/calvin/rss")
	  (goto-char (point-min))
	  (when (search-forward-regexp "img src=\\\"\\\(.+?\\\)\\\"" (point-max) t 1)
		(message "%s" (match-string 1))
		(shell-command (format "/usr/bin/wget -q -P /tmp/calvin %s > /dev/null 2>&1" (match-string 1)))
		(switch-to-buffer (get-buffer-create "*calvin*"))
		(setq cmdStr (concat "/usr/bin/convert -scale 100% -quality 100% " (car (directory-files "/tmp/calvin" t "gif")) " " (file-name-sans-extension(car (directory-files "/tmp/calvin" t "gif"))) "-c.gif"))
		(shell-command cmdStr)
		(insert-image (create-image (car (directory-files "/tmp/calvin" t "gif"))))
		(kill-if-buffer-exists "rss")
		(kill-if-buffer-exists "*Shell Command Output*")))))


(defun run-checkstyle ()
  ""
  (interactive)
  (let ((cmd (concat "java -jar /home/mark/progs/checkstyle-8.12-all.jar -c /home/mark/brandworkz/brandworkzSOA/config/checkstyle.xml " (buffer-file-name (get-buffer (current-buffer))))))
    (message cmd)
    (switch-to-buffer-other-window (get-buffer-create "*checkstyle-output*"))
    (shell-command cmd (get-buffer "*checkstyle-output*"))))



(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "/" string))
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string 
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0)) 
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (goto-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))
