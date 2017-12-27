;; No primary server:
(setq gnus-select-method '(nnnil ""))

(setq gnus-secondary-select-methods
      '((nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port "imaps")
                (nnimap-stream ssl)
                ;; Move expired messages to Gmail's trash.
                (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                ;; Mails marked as expired can be processed immediately.
                (nnmail-expiry-wait immediate))
        (nntp "gmane"
              (nntp-address "news.gmane.org"))))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; Archive outgoing email in Sent folder on imap.gmail.com:
(setq gnus-message-archive-method '(nnimap "gmail")
      gnus-message-archive-group "[Gmail]/Sent Mail")

;; Don't render HTML-mails but show the text part if it's available.
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(setq message-required-mail-headers
      '(From Date Subject X-Mailer User-Agent Message-ID Organization))

(setq message-required-news-headers
      '(From Date Newsgroups Subject
             X-Newsreader User-Agent Message-ID Xref Organization))

(setq gnus-visible-headers
      '("To:" "Cc:" "Reply-To:" "From:" "Subject:" "Newsgroups:"
        "Followup-To:" "Date:" "Organization:" "Organisation:" "X-Mailer:"
        "X-Newsreader:" "User-Agent:"))

(setq gnus-ignored-headers "^References:\\|^Xref:")

(setq gnus-sum-thread-tree-single-indent   " "
      gnus-sum-thread-tree-root            "◆"
      gnus-sum-thread-tree-false-root      "◇"
      gnus-sum-thread-tree-vertical        "│ "
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-single-leaf     "└─►"
      gnus-sum-thread-tree-indent          "  ")

(setq gnus-summary-display-arrow t)

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today)           . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (604800                         . "%A %H:%M")
        ((gnus-seconds-month)           . "%A %d")
        ((gnus-seconds-year)            . "%B %d")
        (t                              . "%B %d '%y")))

(setq gnus-summary-line-format
      (concat "%U%R %~(pad-left 2)t%*  %B %~(max-right 30)~(pad-right 30)n  "
              "%~(max-right 90)~(pad-right 90)s %-135=%&user-date;\n"))

(setq gnus-article-date-headers '(local))

(setq gnus-parameters
      '(("nnimap+gmail:[Gmail]/goldencheetah"
         (auto-expire . t))
        ("nnimap+gmail:[Gmail]/wattage"
         (auto-expire . t))
        ("nnimap+gmail:*"
         (display . all))))
