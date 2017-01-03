(defun quote-query ()
	"Turns a multi-line query into a concated java string"
	(interactive)
	(goto-char (point-min))
	(while (re-search-forward "\\([[:space:]]*\\)\\(.+\\)\\([[:space:]]*\\)$" nil t)
	       (replace-match "\\1\"\\2 \" \+"))
	(goto-char (point-min))
	(while (re-search-forward "\\([[:space:]]\\)[[:space:]]" nil t)
	       (replace-match "\\1"))
)

