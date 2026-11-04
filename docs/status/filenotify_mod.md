# filenotify_mod

- Module: filenotify_mod
- Upstream: filenotify.el
- State: partial

## Ported

`file-notify-add-watch` (flags `FILE_NOTIFY_CHANGE` and `FILE_NOTIFY_ATTRIBUTE_CHANGE`, the inotify backend),
`file-notify-rm-watch`,
`file-notify-rm-all-watches`,
`file-notify-valid-p`,
`file-notify-descriptors`,
`file-notify--handle-event` (actions as `FILE_NOTIFY_*` constants; pairing `moved-from`/`moved-to` into `renamed`, an unpaired half as `deleted` or `created`; the watch removed when its file is deleted or renamed, except to a backup file),
the `stopped` event of `file-notify--rm-descriptor` (a special event),
`file-notify--callback-inotify`

## Deferred

file name handlers (remote files),
`file-notify-debug`,
the other backends (kqueue, gfilenotify, w32notify: not applicable)

## Tests

test_filenotify_mod (ports file-notify-test00-availability, -test01-add-watch, -test02-rm-watch, -test03-events, -test05-file-validity, -test06-dir-validity, local files; the inotify expectations)
