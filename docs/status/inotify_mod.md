# inotify_mod

- Module: inotify_mod (with system_dependent_mod's inotify calls)
- Upstream: inotify.c
- State: done

## Ported

`inotify-add-watch` (aspects as `INOTIFY_*` masks; one inotify instance watched while waiting for input, its events stored as special events; IDs sharing a watch descriptor),
`inotify-rm-watch`,
`inotify-valid-p`,
IN_IGNORED dropping the descriptor,
`file-notify-error`s as report_file_notify_error makes them

## Deferred

`inotify-watch-list` and `inotify-allocated-p` (INOTIFY_DEBUG),
the Android special directories

## Tests

test_inotify_mod (ports inotify-valid-p-simple, inotify-file-watch-simple, inotify-file-watch-stop-delivery)
