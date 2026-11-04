# 2026-11-04: panel-models

Owner decisions on the decision panel (CLAUDE.md):
- The panel is two agents on Opus and one on Fable, so that the three
  answers do not all come from one model.
- A fresh agent writes each question neutrally from the facts, with no
  recommendation; each panelist sees the options in its own shuffled order.
- Claude chooses its own answer only after the panel has answered. A
  unanimous panel decides; otherwise the question goes to the owner on
  issue #261 with each agent's pick and Claude's recommendation.

The questions that were open on #261 under the old rules were withdrawn and
asked again this way.
