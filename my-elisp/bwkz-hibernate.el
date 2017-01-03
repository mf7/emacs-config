(setq data-type-map '(("TINYINT" "Byte") ("INT" "Integer") ("SMALLINT" "Integer") ("BIT" "Integer") ("NVARCHAR([^)]+)" "String") ("VARCHAR([^)]+)" "String") ("NTEXT" "String") ("TEXT" "String") ("CLOB" "Clob" "java.sql.Clob;") ("DATETIME" "Date" "java.util.Date;") ("UNIQUEIDENTIFIER" "String") ("CHAR([^)]+)" "String") ("REAL([^)]+)" "Double") ("DECIMAL([^)]+)" "Float")))

(setq default-imports '("javax.persistence.Column;" "javax.persistence.Entity;" "javax.persistence.Id;" "javax.persistence.Table;" "javax.persistence.JoinColumn;"))

(setq gets-sets nil)
(setq class-vars nil)

(defun create-entity (table-name entity-name)
  "Create an entity class from a db table"
  (interactive "sTable: \nsEntity: ")
  (setq imports default-imports)
  (setq gets-sets nil)
  (setq class-vars nil)
  (remove-crud)
  (replace-datatypes)
  (match-columns)
  (place-boiler-plate table-name entity-name)
)

(defun place-boiler-plate (table-name entity-name)
  (goto-char (point-min))
  (insert "package brandworkz.model;

")
(insert (concat (get-imports) "

"))
(insert (concat "\@Entity
\@Table(name=\"" table-name "\")
"))
(insert (concat "public class " entity-name " {

"))
(insert (concat (get-class-vars) "
"))
(goto-char (point-max))
(insert "}")
)

(defun remove-crud ()
	"Removes any static crap that will get in the way"
	(interactive)
	(goto-char (point-min))
	(while (re-search-forward "CREATE TABLE.*$" nil t)
	       (replace-match ""))
	(goto-char (point-max))
	(re-search-backward "^[:space:]?).*$" nil t)
	       (replace-match "")
)

(defun match-columns ()
	""
	(interactive)
	(goto-char (point-min))
	(while (re-search-forward "\"\\([^\"]+\\)\" \\([^[:space:]]+\\).*" nil t)
	       (replace-match "@Column(name=\"\\1\")
	private \\2 \\1;

	public void set\\1(\\2 \\1){
		this.\\1 = \\1;
	}

	public \\2 get\\1(){
		return this.\\1;
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
	(while (re-search-forward "\\([[:space:]]+@Column.*\n.*;\\)\n" nil t)
	       (replace-match (progn (add-to-list 'class-vars (match-string 1)) "") t nil)
	)
)

(defun replace-datatypes ()
	"Replaces the SQL datatypes with java ones"
	(interactive)
	(goto-char (point-min))
	(let ((case-fold-search nil)) ;;Switch on case sensitivity for the search
	(dolist (list data-type-map)
	  	(goto-char (point-min))
		(while (re-search-forward (nth 0 list) nil t)
		  	(replace-match (nth 1 list) t)
			(if (nth 2 list)
					(add-to-list 'imports (nth 2 list))
			)
		)
	))
)

(defun get-imports () (mapconcat (function (lambda (x) (concat "import " x))) imports "\n"))
(defun get-class-vars () (setq class-vars (reverse class-vars)) (mapconcat 'identity class-vars "

"))
