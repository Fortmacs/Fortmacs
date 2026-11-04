# timer_mod

- Module: timer_mod
- Upstream: emacs-lisp/timer.el (+ keyboard.c `timer_check`, `timer_check_2`, `timer_start_idle`, `timer_stop_idle`, `timer_resume_idle`, `current-idle-time`)
- State: done

## Ported

timers as registry objects (`timer-create`, `is_timer`, `timer-set-time`, `timer-set-idle-time`, `timer-inc-time`, `timer-set-function`, `timer-activate`, `timer-activate-when-idle`, `cancel-timer`, `cancel-timer-internal`, `cancel-function-timers`, `timer-event-handler` with repeats and `timer-max-repeats`, `run-at-time` with a number of seconds, `run-with-timer`, `run-with-idle-timer`, `internal-timer-start-idle`);
`destroy_timer` (ours: no GC);
the command loop runs them while waiting for terminal input,
with the wait's timeout the time to the next timer (no busy waiting),
and `sit-for` runs ordinary timers

## Deferred

timer functions with arguments,
string/encoded/`t` times of `run-at-time`,
`with-timeout`,
`y-or-n-p-with-timeout`,
`timer-duration`,
`add-timeout`,
pending_funcalls,
the timer-event-last variables,
the timer commands;
the clock is monotonic (upstream: wall clock)

## Tests

test_timer_mod (own: idle timers on a pty, repeats once per idle period, timers in `sit-for`, canceling, errors)
