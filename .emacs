;;(expand-file-name "~")

(setq em-dir "~/")

(setq emacs-site-lisp (concat em-dir "emacs/site-lisp/"))

(add-to-list 'load-path emacs-site-lisp)

;;https://www.emacswiki.org/emacs/BuildTags
;;ctags -e -R /home/mark/brandworkz/brandworkzSOA
(setq path-to-ctags "/home/mark/brandworkz/brandworkzSOA/TAGS")

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(global-set-key (kbd "TAB") 'self-insert-command)
(setq default-tab-width 4)

;;(transient-mark-mode nil)
(set-fill-column 100)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'cf-mode)
(add-hook 'cf-mode-user-hook 'turn-on-font-lock)


(setq auto-mode-alist (cons '("\\.mxml$" . xml-mode) auto-mode-alist))


(cond ((fboundp 'global-font-lock-mode)
       ;; Customize face attributes
       (setq font-lock-face-attributes
             '((font-lock-comment-face       "Grey57")
               (font-lock-string-face        "Blue")
               (font-lock-keyword-face       "Red")
               (font-lock-function-name-face "Red")
               (font-lock-variable-name-face "White")
               (font-lock-type-face          "White")
               ))
       ;; Load the font-lock package.
       (require 'font-lock)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)))

(setq make-backup-files nil)
(setq auto-save-default nil)

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 70)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(set-frame-height (selected-frame) 90)
(set-frame-width (selected-frame) 212)
(set-frame-position (selected-frame) 10 0)

(setq inhibit-splash-screen t)

(setq c-syntactic-indentation nil)
(setq c-syntactic-indentation-in-macros nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;(require 'linum)


;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir (concat em-dir "emacs_backups/"))

(make-directory autosave-dir t)

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

(require 'dired-single)
(load (concat em-dir ".dired-config.el"))

;; So that you can open up many files in the same emacs instance
(server-start)

;;(require 'psvn)
;;(require 'sudoku)
;;(require 'sql)

(load (concat em-dir "my-elisp/mf_funcs.el"))
(load (concat em-dir "my-elisp/bwkz-hibernate.el"))
(load (concat em-dir "my-elisp/java-accessors.el"))
(load (concat em-dir "my-elisp/java/quote-query.el"))
(load (concat em-dir "my-elisp/services.el"))

;;(setq svn-status-verbose nil)

(put 'narrow-to-region 'disabled nil)

(require 'php-mode)

(require 'findstr)

;;(iswitchb-mode nil)


(require 'rect-mark)

(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)

(setq desk_dir (concat em-dir ".emacs.d/desktop"))
(setq desktop-dir desk_dir)
(setq desktop-path (list desk_dir))

(desktop-save-mode 1)
(setq desktop-restore-frames 1)
(setq desktop-auto-save-timeout 60)

(winner-mode 1)

(windmove-default-keybindings 'meta)

;; Customization follows below
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)


(defalias 'search-buffers 'multi-occur-in-matching-buffers)

(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mode))



;;(add-to-list 'load-path (concat emacs-site-lisp "gnus/lisp"))
;;(if (featurep 'xemacs)
;;    (add-to-list 'Info-directory-list  (concat emacs-site-lisp "texi/"))
;;  (add-to-list 'Info-default-directory-list (concat emacs-site-lisp "texi/")))
;;(require 'gnus-load)

;;(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'jira)

;;(require 'nero)


;;(global-set-key (kbd "C-x n b") 'nero-new-tab)




;;(load (concat emacs-site-lisp "yasnippet-0.6.1c/yasnippet.el"))
(require 'yasnippet)
(yas/global-mode 1)
(setq yas/root-directory (concat em-dir ".emacs.d/snippets"))
(yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/completing-prompt))
;;(add-hook 'term-mode-hook (lambda()
;;							(yas-minor-mode -1)))

(global-set-key (kbd "<f1>") 'speedbar-get-focus)

(global-set-key (kbd "C-x C-g") 'prelude-google)

(global-set-key (kbd "C-D") 'backward-delete-char)

(ido-mode t)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

(setq yow-file (concat em-dir ".emacs.d/yow_file_zippy_pinhead_quotes.txt.gz" ))
(setq richard-file (concat emacs-site-lisp "poor-richard.gz"))
(setq http-codes-file (concat em-dir "my-elisp/http_status_codes.txt"))



;;(paredit-mode nil)
;;(global-set-key (kbd "<") 'paredit-open-angled)
;;(global-set-key (kbd ">") 'paredit-close-angled)


(load (concat em-dir ".ercrc.el"))
(setq default-directory "~/brandworkzSOA")
;;(shell "*general*")
;;(shell "*versioning*")

;;(load "~/.emacs.d/.gnus")

;;(gnus)


;;(require 'layout-restore)
;; save layout key
;;(global-set-key [?\C-c ?l] 'layout-save-current)
;; load layout key
;;(global-set-key [?\C-c ?r] 'layout-restore)
;; cancel(delete) layout key
;;(global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)


;;If there are two dired windows open, then when you want to do a copy, this variable will set the copy dir to be that of the other dired window.
(setq dired-dwim-target 1)


;;(add-to-list 'load-path "C:/emacs/emacs-24.3/site-lisp/emacs-neotree-dev")
;;  (require 'neotree)
;;  (global-set-key [f8] 'neotree-find)

;;(require 'workgroups)

;;(setq wg-prefix-key (kbd "C-c w"))

(require 'workgroups2)
(setq wg-session-file "~/.emacs_workgroups")
(global-set-key (kbd "<pause>")     'wg-reload-session)
(global-set-key (kbd "C-S-<pause>") 'wg-save-session)
(global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
(global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)
(workgroups-mode 1)




;;(load  (concat emacs-site-lisp "vtl-mode.el"))
;;(require 'vtl)

;;(autoload 'vtl-mode "vtl" "Some documentation." t)
;;(add-to-list 'auto-mode-alist '("\\.vm\\'" . vtl-mode))
;;(setq auto-mode-alist (cons '("\\.vm$" . vtl-mode) auto-mode-alist))
;;(add-to-list 'auto-mode-alist (cons "\\.vm$" 'vtl-mode))
(add-to-list 'auto-mode-alist (cons "\\.vm$" 'html-mode))

;;(wg-load "~/Main.workgroup")

;;(add-to-list 'load-path "c:/emacs/emacs-24.3/site-lisp/jdee-2.4.1/lisp")
;;(load "jde")


;;(setq display-time-day-and-date t display-time-24hr-format t)
;;(display-time)

;;(setq chess-default-display 'chess-plain)
(undo-tree-mode t)

(setq org-log-done 'time)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key "\C-d" 'delete-forward-char)

(add-hook 'c-mode-common-hook
	(lambda ()
		(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
			(ggtags-mode 1))))


;;(defun my-set-eww-buffer-title ()
;;  (let* ((title  (plist-get eww-data :title))
;;         (url    (plist-get eww-data :url))
;;         (result (concat "*eww-" (or title
;;                              (if (string-match "://" url)
;;                                  (substring url (match-beginning 0))
;;                                url)) "*")))
;;    (rename-buffer result t)))
;;
;;(add-hook 'eww-after-render-hook 'my-set-eww-buffer-title)
;;
;;(when (fboundp 'eww)
;;  (progn
;;    (defun xah-rename-eww-hook ()
;;      "Rename eww browser's buffer so sites open in new page."
;;      (rename-buffer "eww" t))
;;    (add-hook 'eww-mode-hook 'xah-rename-eww-hook)))
;;
;;(setq jdee-server-dir "/usr/bin")
;;(setq jde-global-classpath "~/brandworkz/brandworkzSOA")


(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
	("38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" default)))
 '(display-time-mode t)
 '(helm-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 83 :width normal)))))


;;(require 'helm-config)
;;(helm-mode nil)
;;
;;(global-set-key (kbd "M-x") 'helm-M-x)
;;(setq helm-mode-fuzzy-match t)
;;(setq helm-M-x-fuzzy-match t)
;;(setq helm-completion-in-region-fuzzy-match t)
;;(helm-autoresize-mode 1)


(setq emacs-site-lisp (concat em-dir "emacs/site-lisp/"))

(load (concat emacs-site-lisp "goto-last-change.el"))
(global-set-key (kbd "C-.") 'goto-last-change)


;; Shift the selected region right if distance is postive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-left ()
  (interactive)
  (shift-region -4))

(global-set-key (kbd "<backtab>") 'shift-left) ;;shift+tab

 (setq ring-bell-function 'ignore)

(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;;(add-to-list 'load-path "~/emacs/emacs-eclim")

;;(custom-set-variables
;;  '(eclim-eclipse-dirs '("/usr/lib64/eclipse/eclipse"))
;;  '(eclim-executable "~/.eclipse/org.eclipse.platform_793567567_linux_gtk_x86_64/eclim"))

;; regular auto-complete initialization
;;(require 'auto-complete-config)
;;(ac-config-default)

;;(load "~/emacs/emacs-eclim/ac-emacs-eclim-source.el")

;; add the emacs-eclim source
;;(require 'ac-emacs-eclim-source)
;;(ac-emacs-eclim-config)

;;(setq jdee-server-dir )

(scroll-bar-mode -1)

;;(load "~/emacs/jde-findbugs.el")

;;(require 'jde-findbugs)

;;(add-hook 'java-mode-hook 'projectile-mode)

(progn (setq kill-ring nil) (garbage-collect))

(global-set-key (kbd "M-n") 'newline-and-indent)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(helm-autoresize-mode t)

(global-set-key (kbd "C-<f1>") 'other-frame)

(set-default-font "Inconsolata-8")
