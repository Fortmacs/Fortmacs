# message_mod

- Module: message_mod (+ submodule message_smtp_mod)
- Upstream: gnus/message.el (+ rfc2047.el `rfc2047-fold-field`, gnus-util.el `gnus-output-to-mail`, mailheader.el `mail-header-format`, and what mml.el's `mml-generate-mime` makes of a plain ASCII body)
- State: partial

## Ported

`message-user-agent` (the default of `mail_user_agent`, set by `initialize_message`),
`message-mail` (`mail_message`, the agent's COMPOSE: CONTINUE, SWITCH-FUNCTION, TO, SUBJECT),
`message-mail-other-window` (`mail_message_other_window`),
`message-mode` (derived from Text mode: `message-mode-map` with the keys of the commands below, `message-mode-syntax-table`, `message-setup-fill-variables` with `paragraph-start`, `paragraph-separate`, `fill-paragraph-function` and `normal-auto-fill-function`, `message-fill-column`, `comment-start` and `comment-start-skip` from the paste prefix, mail abbrevs, the drafts file name, `indent-tabs-mode` and `parse-sexp-ignore-comments`),
`message-setup` and `message-setup-1` (`set_up_message`: the To and Subject fields, `message-default-headers`, the separator, `message-default-mail-headers`, References and In-Reply-To generated first, the signature, the hooks, `message-position-point`),
`message-buffer-name` (`generate_message_buffer_name`, every value of `message-generate-new-buffers`),
`message-pop-to-buffer`,
`message-buffers`,
`message-set-auto-save-file-name` (the drafts file name and coding system),
`message-send-and-exit`,
`message-send` (the resend question, `message-confirm-send`, `message-allow-no-recipients`, Fcc, `message-sent-hook`, the housekeeping),
`message-fix-before-sending` (the final newline, sorting the headers, the illegible-text check with its choices, the bogus-recipient check),
`message-check-recipients` (the bogus addresses, `message-bogus-addresses`),
`message-send-mail` (the empty Mail-Followup-To removed, the generated headers, the continuation-line check, folding long headers, `message-header-hook`, the encoded copy, the Lines header, `message-ignored-mail-headers`, the final newline, the cleanup),
`message-multi-smtp-send-mail` (`send_message_mail_by_method`: the X-Message-SMTP-Method header with the methods "smtp" (server, service, user) and "sendmail"),
`message-send-mail-with-sendmail` (`message-sendmail-extra-arguments`, `message-sendmail-f-is-evil`, `message-interactive`, Resent-To, the exit value and the error output),
`message-sendmail-envelope-from` and `message--sendmail-envelope-from`,
`message-use-send-mail-function`,
`message-default-send-mail-function` and `message--default-send-mail-function` (the default `message-send-mail-function`),
`message-smtpmail-send-it` (in the "smtp" method),
`message-do-fcc` (files and pipes, with the encoded copy),
`message-output` (`append_message_to_mail_file`, the default `message-fcc-handler-function`) with gnus-util.el's `gnus-output-to-mail`,
`message-do-send-housekeeping` (`message-max-buffers`),
`message-default-send-rename-function` (`rename_sent_message_buffer`),
`message-bury`,
`message-kill-buffer` (`close_message_buffer`),
`message-dont-send`,
`message-generate-headers` (the deletable headers with their text property, optional headers, values verbatim or from a procedure, asking for an empty header, Sender, `message-syntax-checks` "sender"),
`message-make-date`,
`message-make-message-id` (with the reply headers and Supersedes),
`message-unique-id`,
`message-number-base36`,
`message-make-fqdn` (with `message-user-fqdn`),
`message-make-domain`,
`message-make-address`,
`message-make-sender`,
`message-make-from` (every `message-from-style`),
`message-user-mail-address`,
`message-make-organization`,
`message-make-lines`,
`message-make-references`,
`message-make-in-reply-to` (with `message-header-use-obsolete-in-reply-to`),
`message-make-expires`,
`message-make-expires-date`,
`message-make-path`,
`message-fetch-reply-field`,
`message-shorten-references` and `message-shorten-1` (for mail),
`message-fill-field`,
`message-fill-field-address`,
`message-fill-field-general`,
`message-skip-to-next-address`,
`message--fold-long-headers` (with rfc2047.el's `rfc2047-fold-field`, `rfc2047-fold-region` and `rfc2047--break-line`),
`message--check-continuation-headers`,
`message-cleanup-headers`,
`message-encode-message-body` (for a body of ASCII text with no MML tags, no line over 1000 characters and no Content-Type field),
`message-fetch-field` (`get_message_field`),
`message-field-value`,
`message-remove-header`,
`message-remove-first-header`,
`message-add-header` (one header),
`message-replace-header`,
`message-position-on-field`,
`message-all-recipients`,
`message-narrow-to-headers`,
`message-narrow-to-headers-or-head`,
`message-narrow-to-field`,
`message-next-header`,
`message-sort-headers` and `message-sort-headers-1`,
`message-news-p` and `message-mail-p`,
`message-check-element`,
`message-strip-subject-re`,
`message-strip-subject-trailing-was` (`message-subject-trailing-was-query` .true. or .false.),
`message-point-in-header-p`,
`message-in-body-p`,
`message-do-auto-fill`,
`message-fill-paragraph`,
`message-newline-and-reformat` (M-RET),
`message-insert-signature` (C-c C-w),
the field commands (`message-goto-to`, `-from`, `-subject`, `-cc`, `-bcc`, `-fcc`, `-reply-to`, `-newsgroups`, `-distribution`, `-followup-to`, `-mail-followup-to`, `-keywords`, `-summary`, `-body`, `-eoh`, `-signature`),
`message-insert-or-toggle-importance`, `message-insert-importance-high`, `message-insert-importance-low`,
`message-insert-disposition-notification-to`,
`message-insert-expires`,
`message-insert-headers` (for mail),
`message-reduce-to-to-cc`,
`message-add-archive-header`,
`message-mark-inserted-region`, `message-mark-insert-file`,
`message-change-subject`,
`message-elide-region`,
`message-delete-not-region`,
`message-kill-to-signature` (`cut_message_to_signature`),
`message-kill-address` (`cut_message_address`),
`message-rename-buffer`,
`message-beginning-of-line` and `message-beginning-of-header`,
`message-widen-and-recenter`,
`message-syntax-checks` (`message_syntax_checks`, one logical component per check, `%sender` .false. by default),
the user options of these (`message-user-path` but its value t; the defaults upstream copies from the mail settings when message.el is loaded, such as `message-yank-prefix`, `message-signature-file` and `message-default-mail-headers`, are copied the first time Message mode or `mail_message` is used, after the init file, unless the configuration set them; `message_interactive` and `message_signature` start at the defaults of the mail settings they copy);
mail-utils.el's `mail-re-regexps` and `mail--wrap-re-regexp` (mail_utilities_mod), for `message-subject-re-regexp`

## Deferred

MML (mml.el, mm-*.el): MML mode in Message mode and its keys (C-c C-a, C-c C-m ...);
encoding any other body (non-ASCII text, MML tags, lines over 1000 characters, a Content-Type field: `message-encode-message-body` and Fcc fail with a `void_function` error condition instead of sending);
`mml-secure-bcc-is-safe` and the secure-tag checks;
`message-options`;
RFC 2047 encoding of the header (`mail-encode-encoded-word-buffer`): a header with non-ASCII text is a `void_function` error condition;
IDNA (puny.el, `message-idna-to-ascii-rhs`, `message-use-idna`): a non-ASCII domain is a `void_function` error condition;
checking for suspicious addresses (textsec.el) and the syntactic check of addresses (mail-parse.el's `mail-header-parse-addresses`);
news (`message-news`, `message-send-news`, the news headers and syntax checks, the courtesy copy, cancel and supersede, cross-posts, `message-insert-newsgroups`): a message with Newsgroups is a `void_function` error condition;
replies, followups and forwarding (`message-reply`, `message-wide-reply`, `message-followup`, `message-forward`, `message-resend`, `message-bounce`, `message-get-reply-headers`, `message-insert-to`, `message-insert-wide-reply`), and so citing (`message-yank-original`, `message-yank-buffer`, `message-cite-original`, `message-indent-citation`, `message-fill-yanked-message`, the citation line); the reply buffer and headers can be set (`set_message_reply_buffer`, `set_message_reply_headers`);
Mail-Followup-To for subscribed lists (`message-subscribed-*`, `message-make-mail-followup-to`, `message-to-list-only`, `message-generate-unsubscribed-mail-followup-to`);
OTHER-HEADERS, YANK-ACTION, SEND-ACTIONS and RETURN-ACTION of `message-mail` and the actions (`message-add-action`, `message-do-actions`, `message-*-actions`), with Rmail and Reporter;
`message-mail-user-agent`;
`message-server-alist` (`message-update-smtp-method-header`), and the TLS stream types of the "smtp" method (with smtpmail's TLS: they fail as a TLS connection cannot be negotiated);
`message-send-mail-with-qmail`, `message-send-mail-with-mh`, `message-send-mail-with-mailclient` (`void_function` in the method header);
sending in parts (`message-send-mail-partially-limit`, `message-send-mail-partially`);
Babyl Fcc files (gnus-rmail.el's `gnus-output-to-rmail`) and a null `message-fcc-handler-function` (rmailout.el);
auto-saving drafts (`buffer-auto-save-file-name`, deleting it after sending or closing), Gnus drafts (nndraft), `message-recover`;
the fontification (`message-font-lock-keywords` and the message faces), the syntax properties of citations and smileys (`message--syntax-propertize`, so the test message-mode-propertize), the forbidden text properties (`message-strip-forbidden-properties`), the invisible-text check, the tool bar, the menus, `buffer-offer-save`, the yank-media handler, `image-crop-buffer-text-function`;
`adaptive-fill-regexp` and `adaptive-fill-first-line-regexp` as buffer-local values;
the message-rank text property kept between sorts of the headers (the rank is computed again);
the commands that need unported packages: `message-tab`, `message-display-abbrev` and ecomplete, `message-split-line` (split-line), `gnus-delay-article`, `mml-attach-file`, `message-insert-screenshot`, `message-info`, `message-caesar-buffer-body` and `message-caesar-region`, `message-idna-to-ascii-rhs`, `message-insert-newsgroups`, `message-cross-post-followup-to`, `message-mailto`;
the value `dont-check-for-anything-just-trust-me` of `message-syntax-checks`, the asking form of `message-subject-trailing-was-query`, the value t of `message-user-path`, `message-distribution-function`, `message-alternative-emails`, `message-hidden-headers` (`message-hide-headers`), `message-generate-headers-first`, Visual Line mode in `message-beginning-of-line`, the warnings of `compose-mail` about Mail mode settings (`compose-mail-user-agent-warnings`, warnings.el);
the obsolete `message-send-mail-function` (the function), `message-delete-line`, `message-mark-active-p`

## Tests

test_message_mod (ports message-tests.el's message-all-recipients and message-replace-header; own: C-x m (`compose-mail`) with the default agent, sending through test/sendmail-resources/fake-sendmail with the generated headers and the MIME fields, Fcc to an mbox file, the X-Message-SMTP-Method header, a non-ASCII body refused, the syntax checks, the From styles, Message-ID with Supersedes and reply headers, In-Reply-To and References, sorting the headers, folding a long field, `position_message_on_field`)
