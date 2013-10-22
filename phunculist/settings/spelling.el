(setq ispell-dictionary-base-alist
      '((nil                    ; default
         "[A-Za-z]" "[^A-Za-z]" "[']" t
         ("-d" "/Library/Spelling/en_GB" "-i" "utf-8") nil utf-8)
        ("british"              ; British English
         "[A-Za-z]" "[^A-Za-z]" "[']" t
         ("-d" "/Library/Spelling/en_GB" "-i" "utf-8") nil utf-8)
        ("australian"           ; Australian English
         "[A-Za-z]" "[^A-Za-z]" "[']" t
         ("-d" "/Library/Spelling/en_AU" "-i" "utf-8") nil utf-8)
        ("american"             ; American English
         "[A-Za-z]" "[^A-Za-z]" "[']" t
         ("-d" "/Library/Spelling/en_US" "-i" "utf-8") nil utf-8)
        ("dutch"                ; Dutch
         "[A-Za-z]" "[^A-Za-z]" "[']" t
         ("-d" "/Library/Spelling/nl_NL" "-i" "utf-8") nil utf-8)))

(setq ispell-program-name   "hunspell"
      ispell-dictionary     "british"
      ispell-extra-args     nil
      ispell-silently-savep t)
