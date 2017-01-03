(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs"  
"#london-hack-space" "##java")))

(global-set-key (kbd "C-c C-e C-f") (lambda () (interactive)
                                 (erc :server "irc.freenode.net" :nick "mf7")))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))


;; Pool of colors to use when coloring IRC nicks.
(setq erc-colors-list '("blue" "red"
                        "dark gray" "dark orange"
                        "dark magenta" "maroon"
                        "indian red" "black" "forest green"
                        "midnight blue" "dark violet"))
;; special colors for some people
(setq erc-nick-color-alist '(("robonaut" . "blue")
                             ("tomwyatt" . "red")
                             ))

(defun erc-get-color-for-nick (nick)
   "Gets a color for NICK. If NICK is in erc-nick-color-alist, use that  
color, else hash the nick and use a random color from the pool"
   (or (cdr (assoc nick erc-nick-color-alist))
       (nth
        (mod (string-to-number
             (substring (md5 (downcase nick)) 0 6) 16)
            (length erc-colors-list))
        erc-colors-list)))

(defun erc-put-color-on-nick ()
   "Modifies the color of nicks according to erc-get-color-for-nick"
   (save-excursion
     (goto-char (point-min))
     (if (looking-at "<\\([^>]*\\)>")
        (let ((nick (match-string 1)))
          (put-text-property (point-min) (point-max) 'face
                             (cons 'foreground-color
                                   (erc-get-color-for-nick nick)))))))

(add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)

(setq erc-max-buffer-size 30000)
(setq erc-truncate-buffer-on-save t)

(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)

(defun erc-cmd-FLUSH (&rest ignore)
      "Erase the current buffer."
      (let ((inhibit-read-only t))
        (erase-buffer)
        (message "Flushed contents of channel")
        t))
