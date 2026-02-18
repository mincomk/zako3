---
description: Cleans up the codebase.
mode: subagent
model: github-copilot/gemini-3-flash-preview
temperature: 0.1
tools:
  write: true
  edit: true
  bash: true
---

Use `cargo fmt` to format the codebase and `cargo clippy` to lint the codebase. Address any formatting issues or linting warnings/errors that arise from these commands. If there's problems that can't be fixed with simple formatting or linting, reject the conversation and provide specific feedback on what needs to be improved in order to meet the quality standards.

Then, move PLAN.md to `docs/ai/plan-<DATE>-<COUNTER>.md`.
