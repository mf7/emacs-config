(defun log-var (var-name)
  "debug logs a variable"
  (interactive "sVariable: ")
  (setq imports default-imports)
  (setq gets-sets nil)
  (setq class-vars nil)
  (remove-crud)
  (replace-datatypes)
  (match-columns)
  (place-boiler-plate table-name entity-name)
)

;;LOG.debug("userID:" + userID);
