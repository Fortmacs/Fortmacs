# smtpmail_mod

- Module: smtpmail_mod (+ system_socket_mod in src/sys)
- Upstream: mail/smtpmail.el (+ network-stream.el `open-network-stream` and `network-stream-open-starttls` without TLS, `network-stream-get-response`; rfc2104.el `rfc2104-hash` with MD5; message.el `message-make-message-id`, `message-unique-id`, `message-number-base36`, `message-make-fqdn`, `message-user-mail-address`, `message-narrow-to-headers`; process.c's blocking connect, `send_process` and the default sentinel's messages for this connection)
- State: partial

## Ported

`smtpmail-send-it` (`send_mail_with_smtp`: the envelope address from `mail_envelope_from` (send_mail_mod's `get_mail_envelope_from`), the From field (mail_extract_mod) or `smtpmail_user_mail_address`, the blank header lines, blank Subject, the From field in the `angles`, `parens` and plain styles, Message-Id and Date fields, the MIME header for `mail_send_nonascii` MIME, Fcc with the message's coding system, the recipients, Bcc removed, sending or queueing; the `smtpmail_mail_address` and `smtpmail_code_conv_from` bindings),
`smtpmail-send-queued-mail` (`send_queued_mail_with_smtp`; the variables of a queued message are kept in a namelist file, FILE.nml, where upstream writes FILE.el),
`smtpmail-via-smtp` (the trace buffer "*trace of SMTP session to HOST*", the greeting, EHLO and HELO, the extensions, ONEX, VERB, XUSR, MAIL FROM with SIZE, RCPT TO, DATA, QUIT; retries after transient 4xx errors up to `smtpmail_retries`; asking for credentials after 530 or 550-554; `smtpmail_servers_requiring_authorization`),
`smtpmail-try-auth-methods` (the mechanisms of `smtpmail_auth_supported` the server offers),
`smtpmail-try-auth-method` (cram-md5, plain, login, xoauth2),
`smtpmail-command-or-throw` (`send_command_or_fail`),
`smtpmail-read-response`,
`smtpmail-send-command` (AUTH arguments omitted from the trace),
`smtpmail-send-data-1` (dot-stuffing, the data in the trace with `smtpmail_debug_info`),
`smtpmail-send-data` (with its progress reporter),
`smtpmail-process-filter`,
`smtpmail-deduce-address-list`,
`smtpmail-do-bcc`,
`smtpmail-response-code`,
`smtpmail-ok-p`,
`smtpmail-response-text`,
`smtpmail-query-smtp-server` (sets the server and port),
`smtpmail-user-mail-address`,
`smtpmail-fqdn`,
`smtpmail-maybe-append-domain`,
`smtpmail--sanitize-error-message`,
the user options of smtpmail.el but `smtpmail-stream-type` (`smtpmail_smtp_service` takes a port number, `%port`, or a service name, `%service_name`),
`smtpmail-auth-supported`,
`smtpmail-mail-address`;
the "smtp" choice of `sendmail-query-user-about-smtp` (send_mail_mod's submodule send_mail_query_mod);
TCP connections in src/sys/system_socket_mod (getaddrinfo, socket, connect, send without SIGPIPE)

## Deferred

TLS and STARTTLS (no C libraries: GnuTLS is not bound; the connection stays unencrypted as in an Emacs without TLS support, so a server's STARTTLS is not used) and with them `smtpmail-stream-type` and its stored queue value, client certificates and `nsm-verify-connection`;
the sources of credentials (auth-source.el: `auth-source-search`, the authinfo and netrc files, `:smtp-auth`, saving new credentials, `auth-source-forget+`): without them nothing is found, and when credentials are needed the user name and password are asked for as the netrc source's creation does;
expanding mail aliases (mailalias.el) and RFC 2047 encoding of the header (`mail-encode-header`);
saving the server and port found by `smtpmail-query-smtp-server` into init.f90 (`customize-save-variable`);
`message-user-fqdn` and the reply headers of `message-make-message-id` (message.el, step 52c);
C-g while waiting for the server;
the obsolete `smtpmail-intersection` and the unused `smtpmail-find-credentials` and `smtpmail-cred-*` accessors

## Tests

test_smtpmail_mod (own; upstream has no smtpmail-tests.el: a fake server forked by the test on 127.0.0.1 answers from a list and records what it received: the dialogue, dot-stuffing, recipients and Bcc, HELO, AUTH PLAIN with the trace buffer, CRAM-MD5 with RFC 2195's example, a failed AUTH sanitized, refused recipient, refused greeting, a transient error retried, a refused connection, no recipients, queueing and sending the queue, the envelope address from the From field and from `mail_envelope_from` set to MAIL_ENVELOPE_FROM_HEADER, a service name, an unknown service name)
