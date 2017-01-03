
(defun dired-sort-criteria (criteria)
  "sort-dired by different criteria by Robert Gloeckner "
  (interactive 
   (list 
    (or (completing-read "criteria [name]: "
			 '("size(S)" "extension(X)" "creation-time(ct)"
			   "access-time(ut)" "time(t)" "name()"))
	"")))
  (string-match ".*(\\(.*\\))" criteria)
  (dired-sort-other
   (concat dired-listing-switches 
	   (match-string 1 criteria))))




(defun dired-sort-size ()
  "Dired sort by size."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S")))

(defun dired-sort-extension ()
  "Dired sort by extension."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X")))

(defun dired-sort-ctime ()
  "Dired sort by create time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ct")))

(defun dired-sort-utime ()
  "Dired sort by access time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ut")))

(defun dired-sort-time ()
  "Dired sort by time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))

(defun dired-sort-name ()
  "Dired sort by name."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "")))


(defun my-dired-init ()
        "Bunch of stuff to run for dired, either immediately or when it's
         loaded."
        ;; <add other stuff here>
        (define-key dired-mode-map [return] 'joc-dired-single-buffer)
        (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
        (define-key dired-mode-map "^"
              (function
               (lambda nil (interactive) (joc-dired-single-buffer "..")))))

      ;; if dired's already loaded, then the keymap will be bound
      (if (boundp 'dired-mode-map)
              ;; we're good to go; just add our bindings
              (my-dired-init)
        ;; it's not loaded yet, so add our bindings to the load-hook
        (add-hook 'dired-load-hook 'my-dired-init))
