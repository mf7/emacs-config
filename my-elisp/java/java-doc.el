(defun create-method-jdoc ()
  ""
  (interactive)
  (setq funct-list (split-string (get-method-signature)))
  (princ funct-list)
  (go-to-previous-empty-line)
  (insert "/**")
  (newline-and-indent)
  (insert " * ")
  (insert (read-string "Method description: "))
  (newline-and-indent)
  (setq args (get-method-arguments funct-list))
  (while args
	(setq arg (car args))
	(insert (concat " * @param " arg))
	(insert (concat " " (read-string (concat arg " description: "))))
	(newline-and-indent)
	(setq args (cdr args)))
  (setq returns (get-returns (nth 1 funct-list)))
  (if returns
	  (progn
		(insert (concat " * @returns " returns))
		(insert (concat " " (read-string "Return description: ")))
		(newline-and-indent)))
  (insert "**/"))


(defun get-method-arguments (funct-list)
  "Returns a list of the argument names"
  (setq args ())
  (setq elem (car funct-list))
  (while (not (string= "|" elem))
	;;(message elem)
	(setq elem (car funct-list))
	(setq funct-list (cdr funct-list)))
  ;;(message "processing args")
  (setq elem (car funct-list))
  (while (not (string= "|" elem))
	(setq elem (nth 1 funct-list))
	;;(message elem)
	(setq args (cons elem args))
	(setq funct-list (cdr (cdr funct-list)))
	(setq elem (car funct-list))
	;;(message elem))
	args
  )

;;(get-method-arguments '("public" "String" "getTranslation" "|" "String" "original_text" "String" "lang_code" "|" "throws" "Exception"))

(defun get-returns (return-type)
  "Returns the class if one is applicable"
  (if (not (string= return-type "void"))
	  return-type nil))

;;(get-returns "String")

(defun get-method-signature ()
  "Returns the string of the method"
  (interactive)
  (back-to-indentation)
  (push-mark)
  (move-end-of-line nil)
  (setq str (buffer-substring (point) (mark)))
  (dired-replace-in-string "[\(\)]" " | " (dired-replace-in-string "[\{,]" "" str)))

(defun go-to-previous-empty-line ()
  "Goes to the nearest empty line before the method signature"
  (setq at-empty-line nil)
  (while (not at-empty-line)
  	(move-end-of-line 0)
	(push-mark)
  	(back-to-indentation)
	(if (eq (mark) (point)) (setq at-empty-line t))
	)
  )

(not nil)


(split-string "hello my name is bob" "e")

(let (
	  (bob " is a monkey")
	  (mike (concat "that bob" 'bob)))
	  (message mike))

(setq mike '("Integer" "mike") )
(setq var_one "this")
(setq var_two "that")
(cons (this that) mike)
