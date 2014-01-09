;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (phunculist/align=) "phunculist/commands/alignment"
;;;;;;  "phunculist/commands/alignment.el" (21092 34499 0 0))
;;; Generated autoloads from phunculist/commands/alignment.el

(autoload 'phunculist/align= "phunculist/commands/alignment" "\
Aligns equal signs in the region or buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/insert-header-comment) "phunculist/commands/comments"
;;;;;;  "phunculist/commands/comments.el" (21092 34507 0 0))
;;; Generated autoloads from phunculist/commands/comments.el

(autoload 'phunculist/insert-header-comment "phunculist/commands/comments" "\
Inserts a header wrapped in the appropriate comment characters.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/erb-insert-or-toggle-erb-tag) "phunculist/commands/erb"
;;;;;;  "phunculist/commands/erb.el" (21092 34519 0 0))
;;; Generated autoloads from phunculist/commands/erb.el

(autoload 'phunculist/erb-insert-or-toggle-erb-tag "phunculist/commands/erb" "\
Insert an ERb tag if the point isn't currently in one, or toggle the type.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/htmlify-buffer-light phunculist/htmlify-buffer
;;;;;;  phunculist/html-insert-open-and-close-tag) "phunculist/commands/html"
;;;;;;  "phunculist/commands/html.el" (21092 34541 0 0))
;;; Generated autoloads from phunculist/commands/html.el

(autoload 'phunculist/html-insert-open-and-close-tag "phunculist/commands/html" "\
Generates an open and close HTML snippet using the current word.

\(fn)" t nil)

(autoload 'phunculist/htmlify-buffer "phunculist/commands/html" "\
Converts the entire buffer in syntax highlighted HTML.

\(fn)" t nil)

(autoload 'phunculist/htmlify-buffer-light "phunculist/commands/html" "\
Converts the entire buffer in syntax highlighted HTML using a light theme.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/erc-connect) "phunculist/commands/irc"
;;;;;;  "phunculist/commands/irc.el" (21092 34547 0 0))
;;; Generated autoloads from phunculist/commands/irc.el

(autoload 'phunculist/erc-connect "phunculist/commands/irc" "\
Prompts for a channel, then connects to IRC.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/newline-between phunculist/newline-above
;;;;;;  phunculist/newline-below) "phunculist/commands/lines" "phunculist/commands/lines.el"
;;;;;;  (21092 34548 0 0))
;;; Generated autoloads from phunculist/commands/lines.el

(autoload 'phunculist/newline-below "phunculist/commands/lines" "\
Insert a new line below the current line and indent it.

\(fn SKIP-EOL)" t nil)

(autoload 'phunculist/newline-above "phunculist/commands/lines" "\
Insert a new line above the current line and indent it.

\(fn)" t nil)

(autoload 'phunculist/newline-between "phunculist/commands/lines" "\
Splits content before and after the point to insert new content between.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/markdown-preview phunculist/markdown-yank-as-pre)
;;;;;;  "phunculist/commands/markdown" "phunculist/commands/markdown.el"
;;;;;;  (21092 34548 0 0))
;;; Generated autoloads from phunculist/commands/markdown.el

(autoload 'phunculist/markdown-yank-as-pre "phunculist/commands/markdown" "\
Yank in some text as preformatted.

\(fn)" t nil)

(autoload 'phunculist/markdown-preview "phunculist/commands/markdown" "\
Preview the current buffer in Marked.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/org-new-heading-above phunculist/org-new-heading-below)
;;;;;;  "phunculist/commands/org" "phunculist/commands/org.el" (21092
;;;;;;  34549 0 0))
;;; Generated autoloads from phunculist/commands/org.el

(autoload 'phunculist/org-new-heading-below "phunculist/commands/org" "\
Creates a new heading below the current one.

\(fn)" t nil)

(autoload 'phunculist/org-new-heading-above "phunculist/commands/org" "\
Creates a new heading above the current one.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/rails-dbconsole phunculist/rails-console)
;;;;;;  "phunculist/commands/rails" "phunculist/commands/rails.el"
;;;;;;  (21092 34550 0 0))
;;; Generated autoloads from phunculist/commands/rails.el

(autoload 'phunculist/rails-console "phunculist/commands/rails" "\
Invoke inf-ruby with Rails environment loaded.

\(fn)" t nil)

(autoload 'phunculist/rails-dbconsole "phunculist/commands/rails" "\
Open a SQL shell using the settings from config/database.yml.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/toggle-ruby-hash-type phunculist/toggle-ruby-string-and-symbol
;;;;;;  phunculist/toggle-ruby-regex-type phunculist/toggle-ruby-block-type
;;;;;;  phunculist/toggle-ruby-test) "phunculist/commands/ruby" "phunculist/commands/ruby.el"
;;;;;;  (21092 34550 0 0))
;;; Generated autoloads from phunculist/commands/ruby.el

(autoload 'phunculist/toggle-ruby-test "phunculist/commands/ruby" "\
Toggler between positive and negative assertions.

\(fn)" t nil)

(autoload 'phunculist/toggle-ruby-block-type "phunculist/commands/ruby" "\
Toggle between brace and do/end block types.

\(fn)" t nil)

(autoload 'phunculist/toggle-ruby-regex-type "phunculist/commands/ruby" "\
Toggle between standard and choose-your-own-quotes regex types.

\(fn)" t nil)

(autoload 'phunculist/toggle-ruby-string-and-symbol "phunculist/commands/ruby" "\
Toggle between strings and symbols.

\(fn)" t nil)

(autoload 'phunculist/toggle-ruby-hash-type "phunculist/commands/ruby" "\
Toggle between symbol key hash types.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/open-shell-in-ansi-term) "phunculist/commands/shell"
;;;;;;  "phunculist/commands/shell.el" (21092 34551 0 0))
;;; Generated autoloads from phunculist/commands/shell.el

(autoload 'phunculist/open-shell-in-ansi-term "phunculist/commands/shell" "\
Opens a new shell inside an ansi-terminal.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/toggle-spelling phunculist/toggle-program-spelling)
;;;;;;  "phunculist/commands/spelling" "phunculist/commands/spelling.el"
;;;;;;  (21092 34551 0 0))
;;; Generated autoloads from phunculist/commands/spelling.el

(autoload 'phunculist/toggle-program-spelling "phunculist/commands/spelling" "\
Toggle the programmer's spelling mode on and off.

\(fn)" t nil)

(autoload 'phunculist/toggle-spelling "phunculist/commands/spelling" "\
Toggle the spelling mode on and off.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/toggle-string-type) "phunculist/commands/strings"
;;;;;;  "phunculist/commands/strings.el" (21092 34552 0 0))
;;; Generated autoloads from phunculist/commands/strings.el

(autoload 'phunculist/toggle-string-type "phunculist/commands/strings" "\
Toggle between double, single, and choose-your-own-quotes string types.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/fix-all-whitespace phunculist/trim-backwards-and-forwards
;;;;;;  phunculist/trim-forwards phunculist/trim-backwards) "phunculist/commands/whitespace"
;;;;;;  "phunculist/commands/whitespace.el" (21092 34552 0 0))
;;; Generated autoloads from phunculist/commands/whitespace.el

(autoload 'phunculist/trim-backwards "phunculist/commands/whitespace" "\
Removes all whitespace behind the point.

\(fn)" t nil)

(autoload 'phunculist/trim-forwards "phunculist/commands/whitespace" "\
Removes all whitespace in front of the point.

\(fn)" t nil)

(autoload 'phunculist/trim-backwards-and-forwards "phunculist/commands/whitespace" "\
Removes all whitespace behind and in front of the point.

\(fn)" t nil)

(autoload 'phunculist/fix-all-whitespace "phunculist/commands/whitespace" "\
Replace tabs and cleans up junk whitespace.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/toggle-wrap) "phunculist/commands/wrap"
;;;;;;  "phunculist/commands/wrap.el" (21092 34569 0 0))
;;; Generated autoloads from phunculist/commands/wrap.el

(autoload 'phunculist/toggle-wrap "phunculist/commands/wrap" "\
Toggle the wrap guide and soft wrapping on and off.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/delete-file-and-buffer phunculist/rename-file-and-buffer
;;;;;;  phunculist/cleanup-buffer phunculist/cleanup-buffer-safe
;;;;;;  phunculist/indent-buffer phunculist/untabify-buffer) "phunculist/functions/buffers"
;;;;;;  "phunculist/functions/buffers.el" (21095 22886 0 0))
;;; Generated autoloads from phunculist/functions/buffers.el

(autoload 'phunculist/untabify-buffer "phunculist/functions/buffers" "\


\(fn)" t nil)

(autoload 'phunculist/indent-buffer "phunculist/functions/buffers" "\


\(fn)" t nil)

(autoload 'phunculist/cleanup-buffer-safe "phunculist/functions/buffers" "\
Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad.

\(fn)" t nil)

(autoload 'phunculist/cleanup-buffer "phunculist/functions/buffers" "\
Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save.

\(fn)" t nil)

(autoload 'phunculist/rename-file-and-buffer "phunculist/functions/buffers" "\
Rename the current buffer and file it is visiting.

\(fn)" t nil)

(autoload 'phunculist/delete-file-and-buffer "phunculist/functions/buffers" "\
Kill the current buffer and deletes the file it is visiting.

\(fn)" t nil)

;;;***

;;;### (autoloads (phunculist/find-in-path phunculist/find-subpath-in-path
;;;;;;  phunculist/read-file-to-string) "phunculist/functions/files"
;;;;;;  "phunculist/functions/files.el" (21093 50497 0 0))
;;; Generated autoloads from phunculist/functions/files.el

(autoload 'phunculist/read-file-to-string "phunculist/functions/files" "\
Reads the contents of path into a string.

\(fn PATH)" nil nil)

(autoload 'phunculist/find-subpath-in-path "phunculist/functions/files" "\
Walks up the passed path hunting for subpath at each level.

\(fn SUBPATH PATH)" nil nil)

(autoload 'phunculist/find-in-path "phunculist/functions/files" "\
Walks up the current path hunting for subpath at each level.

\(fn SUBPATH)" nil nil)

;;;***

;;;### (autoloads (phunculist/flatten) "phunculist/functions/lists"
;;;;;;  "phunculist/functions/lists.el" (21092 34225 0 0))
;;; Generated autoloads from phunculist/functions/lists.el

(autoload 'phunculist/flatten "phunculist/functions/lists" "\
Flatten a list.

\(fn X)" nil nil)

;;;***

;;;### (autoloads (phunculist/read-rails-database-config) "phunculist/functions/rails"
;;;;;;  "phunculist/functions/rails.el" (21092 34249 0 0))
;;; Generated autoloads from phunculist/functions/rails.el

(autoload 'phunculist/read-rails-database-config "phunculist/functions/rails" "\
Loads the database config as:  adapter database username [password].

\(fn PATH)" nil nil)

;;;***

;;;### (autoloads (phunculist/regex-replace-all phunculist/regex-replace)
;;;;;;  "phunculist/functions/regexen" "phunculist/functions/regexen.el"
;;;;;;  (21092 34274 0 0))
;;; Generated autoloads from phunculist/functions/regexen.el

(autoload 'phunculist/regex-replace "phunculist/functions/regexen" "\
Replace a regular expression in the passed string, if it occurs.

\(fn STR REGEX REPLACEMENT &optional FIXEDCASE LITERAL)" nil nil)

(autoload 'phunculist/regex-replace-all "phunculist/functions/regexen" "\
Replace a regular expression everywhere it occurs in the passed string.

\(fn STR REGEX REPLACEMENT &optional FIXEDCASE LITERAL)" nil nil)

;;;***

;;;### (autoloads (phunculist/camelize phunculist/string-trim) "phunculist/functions/strings"
;;;;;;  "phunculist/functions/strings.el" (21092 34302 0 0))
;;; Generated autoloads from phunculist/functions/strings.el

(autoload 'phunculist/string-trim "phunculist/functions/strings" "\
Trim whitespace from both ends of the passed string.

\(fn STR)" nil nil)

(autoload 'phunculist/camelize "phunculist/functions/strings" "\
Forces a string into CamelCase.

\(fn STR)" nil nil)

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
