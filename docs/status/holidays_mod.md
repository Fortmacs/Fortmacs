# holidays_mod

- Module: holidays_mod, calendar_julian_mod (submodule), calendar_islamic_mod (submodule)
- Upstream: calendar/holidays.el, calendar/cal-julian.el (in part), calendar/cal-islam.el (in part)
- State: partial

## Ported

the holiday lists as `holidays_type` lists of `holiday_entry_type` records (keyword constructors, `%append`, `%clear`): `holiday-general-holidays`,
`holiday-local-holidays`,
`holiday-other-holidays`,
`holiday-christian-holidays`,
`holiday-islamic-holidays`,
`calendar-holidays` (made from them when first used);
the entry kinds `holiday-fixed`,
`holiday-float`,
`holiday-sexp` (`holiday_expression`, a procedure pointer),
`holiday-easter-etc` (with the standard list),
`holiday-advent`,
`holiday-greek-orthodox-easter`,
`holiday-julian`,
`holiday-islamic`,
and computed entries (a procedure pointer: `holiday-islamic-new-year`, the conditional groups of the default lists);
`holiday-after`,
`holiday-filter-visible-calendar`,
`holiday-easter-etc-abs`,
`holiday-greek-orthodox-easter-abs`,
`calendar-holiday-list`,
`calendar-list-holidays` (a),
`holidays` (M-x holidays, C-u for the month),
`list-holidays` and its alias `holiday-list` (the choices of `holiday-available-holiday-lists` for the ported lists, and "Ask"),
`calendar-check-holidays`,
`holiday-in-range`,
`calendar-cursor-holidays` (h),
`calendar-mark-holidays` (x; u unmarks);
cal-julian.el: `calendar-julian-to-absolute`,
`calendar-julian-from-absolute`;
cal-islam.el: `calendar-islamic-leap-year-p`,
`calendar-islamic-last-day-of-month`,
`calendar-islamic-day-number`,
`calendar-islamic-to-absolute`,
`calendar-islamic-from-absolute`,
`calendar-islamic-epoch`

## Deferred

`holiday-hebrew-holidays` (waits for cal-hebrew.el, a later part of step 46, the other calendars),
`holiday-bahai-holidays` (cal-bahai.el, which needs solar.el and lunar.el),
`holiday-oriental-holidays` (cal-china.el, which needs solar.el and lunar.el),
`holiday-solar-holidays` and `solar-equinoxes-solstices` (solar.el, cal-dst.el),
the choices "Equinoxes/Solstices", "Hebrew", "Bahá’í", "Oriental" and "Solar" of `list-holidays`,
the warning for a failing entry (warnings.el's display-warning; the entry gives no holidays) and `calendar-debug-sexp`,
the EVENT arguments and the menu of `calendar-cursor-holidays` (cal-menu.el, the mouse),
`calendar-holiday-marker` (a face or a single-character string, with `calendar-today-marker`; dates are marked with the `holiday` face),
the :set function of the lists (`holidays--set-calendar-holidays`),
the rest of cal-julian.el (`calendar-julian-date-string`, `calendar-julian-print-date`, `calendar-julian-goto-date`, the astronomical day numbers, the diary entries),
the rest of cal-islam.el (`calendar-islamic-date-string`, `calendar-islamic-print-date`, `calendar-islamic-goto-date`, the diary functions);
Not applicable: customizing `holiday-available-holiday-lists` with add-function

## Tests

test_holidays_mod (ports holidays-test-holiday-easter-etc and holidays-test-more-months; own tests of Easter and Pascha dates, the Julian and Islamic conversions, the holidays of a calendar window, M-x list_holidays for 2026 and, with all Christian and Islamic holidays, 2024 (expected text made by GNU Emacs from upstream's 31.1 sources), an expression entry, `calendar_list_holidays` and its mode line, `calendar_check_holidays` and h, x and u)
