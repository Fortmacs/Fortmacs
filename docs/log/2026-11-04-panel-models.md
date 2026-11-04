# 2026-11-04: panel-models

Owner decisions on the decision panel (CLAUDE.md):
- The panel is two agents on Opus and one on Fable, so that the three
  answers do not all come from one model.
- A fresh agent writes each question neutrally from the facts, with no
  recommendation; each panelist sees the options in its own shuffled order.
- Claude seals its own answer in the decision log, with the time, before
  reading any panelist's answer; the panelists never see it. A unanimous
  panel decides; otherwise the question goes to the owner on issue #261
  with each agent's pick and reason and Claude's sealed pick.

The questions that were open on #261 under the old rules were withdrawn and
asked again this way.

Follow-up (panel-followup): the sealed pick and the #272 routing, which
landed on the branch after #270 merged; a clash check before the panel
(where a newer owner rule covers a question, the newer rule wins and the
older decision is removed, as R6-36's window_state_get/_put fell to verb
first); #272's title counts the answered decisions; tools/fortran-names
gets window_state_get -> get_window_state and window_state_put ->
set_window_state.
