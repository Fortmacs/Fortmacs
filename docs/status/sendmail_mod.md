# sendmail_mod

- Module: sendmail_mod
- Upstream: mail/sendmail.el (+ mule-cmds.el `select-message-coding-system`, message.el `message-narrow-to-headers` and `message-make-date`)
- State: partial

## Ported

`mail` (C-x m through `compose_mail`; NOERASE as MAIL_NOERASE_NO, _YES and _NEW, TO, SUBJECT, IN-REPLY-TO, CC, a REPLYBUFFER buffer),
`mail-other-window`,
`mail-mode` (derived from Text mode: `mail-mode-map` with C-c ?, C-c C-f C-t/C-b/C-f/C-c/C-s/C-r/C-a/C-l/C-d, C-c C-t, C-c C-y, C-c C-r, C-c C-w, C-c C-c, C-c C-s, C-c C-i; `mail-mode-syntax-table`, `mail-font-lock-keywords` with the citation keyword, `mail-mode-auto-fill`, `mail-mode-fill-paragraph`, `comment-start` and `comment-start-skip` from the paste prefix, `paragraph-separate`),
`mail-setup` (`set_up_mail`),
`mail-header-end`,
`mail-text-start`,
`mail-sendmail-delimit-header`,
`mail-sendmail-undelimit-header`,
`mail-send-and-exit` (`send_mail_and_exit`),
`mail-dont-send`,
`mail-bury` (`bury_mail`),
`mail-send` (`send_mail`: the resend question, combining To and Cc fields, Mail-Followup-To and Mail-Reply-To for `mail-mailing-lists`, `mail-send-nonascii` (`.true.`, `.false.`, MAIL_SEND_NONASCII_ASK, _MIME), the invalid header check, `mail-send-hook`),
`mail-insert-from-field` with `mail-from-style`,
`mail-combine-fields`,
`sendmail-send-it` (`send_mail_with_sendmail`: Resent- fields, blank Subject, From, the MIME header with the coding system's charset, Fcc, `sendmail-program` through `execute_process_region` with `mail-specify-envelope-from`, `mail-envelope-from`, `mail-alias-file`, the error reporting options, `mail-use-dsn`; exit values, signals and error output),
`mail-do-fcc` (mbox and Babyl files, buffers visiting them),
`sendmail-query-once` and `sendmail-query-user-about-smtp` (`query_sendmail_once`, the default `send_mail_function`),
`sendmail-user-agent` and `sendmail-user-agent-compose`,
the field commands (`mail-to`, `mail-subject`, `mail-cc`, `mail-bcc`, `mail-fcc`, `mail-reply-to`, `mail-mail-reply-to`, `mail-mail-followup-to`, `mail-position-on-field`),
`mail-text`,
`mail-insert-disposition-notification-to`,
`mail-signature` (the variable is `mail_signature_variable`, a choice setting: `.true.` the file, `.false.` none, `%text` a string, `%procedure` a procedure; R6-39, R6-37),
`mail-indent-citation`,
`mail-yank-original` (`paste_mail_original`, from a reply buffer),
`mail-yank-clear-headers`,
`mail-yank-region` (a no-op without Rmail's action, as upstream),
`mail-insert-file`,
the user options of sendmail.el

## Deferred

the actions (`mail-reply-action` as an action, `mail-send-actions`, `mail-return-action`, YANK-ACTION, SEND-ACTIONS and RETURN-ACTION and returning to Rmail in `mail-bury`; `mail-bury-selects-summary`) with Rmail;
mail aliases on sending (mailalias.el: `mail-aliases`, `mail-alias-modtime`, `sendmail-sync-aliases`, `build-mail-aliases`, `expand-mail-aliases`, `mail-completion-at-point-function`);
RFC 2047 encoding of the header (`mail-encode-header`, rfc2047.el);
checking for suspicious addresses (textsec.el);
MML (`mail-encode-mml`, `mail-add-attachment`, long lines);
`mail-recover`,
`mail-recover-1` and auto-saving drafts;
`mail-other-frame` (frames);
`mail-split-line` and its remap of `split-line` (not ported);
`mail-fill-yanked-message` (C-c C-q, needs `fill-individual-paragraphs`);
the `header` value of `mail-envelope-from` (mail-extr.el);
the mail client and SMTP choices of `sendmail-query-once` (mailclient.el, smtpmail.el, step 52b: choosing them is a `void_function` error) and saving the choice into init.f90 (`customize-save-variable`);
the bold choices in its help buffer;
the `category` property of the separator;
`adaptive-fill-regexp` and `adaptive-fill-first-line-regexp` as buffer-local values;
`buffer-offer-save`;
input methods in `mail-setup`;
binding `inhibit-read-only`;
`select-safe-coding-system` in `select-message-coding-system`;
the coding of a pasted original message;
deleting the windows of the original (`delete-windows-on`);
the menus;
DESTINATION 0 of `execute_process_region` (sending with `mail_interactive` false)

## Tests

test_sendmail_mod (own: setting up, the field commands, the From field, signatures, `compose_mail`, sending through test/sendmail-resources/fake-sendmail with Fcc, failures, error output and no recipients)
