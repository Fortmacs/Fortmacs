# calendar_mod

- Module: calendar_mod, calendar_movement_mod (submodule)
- Upstream: calendar/calendar.el, calendar/cal-move.el
- State: partial

## Ported

`calendar` (M-x calendar, C-u for the month: `calendar-basic-setup`, the window splitting of a wide or full-height frame),
`calendar-mode` and `calendar-mode-map` (the keys of calendar.el and cal-move.el),
the three-month display as `calendar-generate`,
`calendar-generate-month` and `calendar-generate-window` draw it (month headers in `calendar-month-header`, day headers in `calendar-weekday-header`/`calendar-weekend-header`, `date`, `mouse-face` and `help-echo` on the dates, `calendar-week-start-day`, `calendar-weekend-days`, the layout options and `calendar-recompute-layout-variables`, `calendar-total-months` per buffer),
`calendar-redraw` (C-c C-l),
`calendar-exit` (q, C-u q),
`calendar-update-mode-line` with the default mode line,
the Gregorian and absolute date arithmetic (`calendar-absolute-from-gregorian`, `calendar-gregorian-from-absolute`, `calendar-day-number`, `calendar-leap-year-p`, `calendar-last-day-of-month`, `calendar-day-of-week`, `calendar-increment-month`, `calendar-interval`, `calendar-date-from-day-of-year`, `calendar-dayname-on-or-before`, `calendar-nth-named-absday`/`-day`, `calendar-current-date`),
`calendar-date-string` in the three date styles and `calendar-set-date-style`,
day and month names and abbreviations,
`calendar-day-of-year-string` and `calendar-print-day-of-year` (p d),
`calendar-cursor-to-date`,
`calendar-date-is-visible-p`,
`calendar-get-month-range`,
`calendar-read-date`,
`calendar-read-sexp` (integers),
`calendar-other-month` (o),
the date mark ring (C-SPC, C-x C-x, M-=),
`calendar-not-implemented`,
`calendar-unmark` (u),
`calendar-mark-visible-date`,
`calendar-star-date`,
`calendar-mark-today` (with the face),
`calendar-goto-info-node` (?),
`calendar-mark-holidays-flag`,
`calendar-view-holidays-initially-flag`,
`calendar-christian-all-holidays-flag`,
`calendar-islamic-all-holidays-flag`,
`calendar-mark-holidays` (the variable, `calendar_mark_holidays_variable`),
`calendar-in-read-only-buffer`,
`calendar-set-mode-line` (a buffer visiting no file),
`calendar-month-visible-p` (N omitted),
`calendar--month-overlap-p`,
`calendar-nongregorian-date-visible-p`,
`calendar-date-compare` (the sort of holidays_mod),
the hooks (`calendar-initial-window-hook`, `calendar-today-visible-hook`, `calendar-today-invisible-hook`, `calendar-move-hook`, `calendar-mode-hook`),
the faces;
cal-move.el: `calendar-cursor-to-nearest-date`,
`calendar-cursor-to-visible-date`,
`calendar-goto-today` (.),
day,
week,
month and year motion (C-f, C-b, C-n, C-p, M-}, M-{, C-x ],
C-x [),
C-a,
C-e,
M-a,
M-e,
M-<,
M->,
`calendar-goto-date` (g d),
`calendar-goto-day-of-year` (g D),
scrolling (<, >, C-x <, C-x >, C-v, M-v),
`calendar-recenter` (C-l),
`calendar-show-more-months`,
`calendar-show-fewer-months`

## Deferred

holidays.el (see holidays_mod: `calendar-holiday-marker`, the other calendars' holidays),
diary-lib.el,
the other calendars (cal-iso.el, cal-hebrew.el and the rest, and cal-julian.el and cal-islam.el but their conversions: g and p keys but g d, g D and p d),
`calendar-get-date-range`,
the years of `calendar-month-visible-p` for N months,
the obsolete `calendar-nongregorian-visible-p`,
`calendar-set-mode-line` for a buffer visiting a file,
lunar.el,
solar.el,
appt.el,
cal-html.el,
cal-tex.el,
cal-x.el (`calendar-setup` frames),
cal-menu.el (menus, the mouse and EVENT arguments),
the computed forms of `calendar-date-display-form`,
`calendar-month-header`,
`calendar-intermonth-text`,
`calendar-intermonth-header`,
`calendar-date-echo-text` and `calendar-mode-line-format`,
the :set functions of the options,
the string forms of `calendar-today-marker`,
`scroll-margin` (see the module header)

## Tests

test_calendar_mod (ports calendar-test-validity-predicate and calendar-test-date-in-calendar-mode-line with the default mode line; own display tests for fixed months, date arithmetic, strings, movement and scrolling, the mark ring)
