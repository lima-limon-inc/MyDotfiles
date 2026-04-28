---
name: delegate
description: >
  Use when the user wants to delegate a task via "/delegate <task>", "delegate this",
  "hand this off", "have an agent plan X", or otherwise asks for a read-only subagent
  to investigate and plan work before any edits are made. The skill runs a read-only
  subagent to produce a plan, waits for approval, creates a worktree, then executes.
version: 0.1.0
---

# delegate

Read-only subagent plans the work; you approve; main assistant executes inside a fresh worktree.

ultrathink — use maximum thinking budget on every invocation of this skill.

## Rules

- Prose: bare essentials. No preamble, no recap, no trailing summary.
- Code written in Phase 4: minimal. Comment only when the WHY is non-obvious; never narrate the WHAT.
- The subagent must be read-only. Only `Explore` or `Plan` agent types are permitted — both exclude `Edit`, `Write`, `NotebookEdit`. Never use `general-purpose`, `picante`, or `researcher` here.

## Agent selection

- **Explore** — task is "find / understand / map / trace": finding callers, mapping a flow, comparing two implementations, locating a bug's origin.
- **Plan** — task is "design / refactor / architect": choosing a data model, breaking down a feature, deciding between approaches.

If the task is truly both, pick `Plan` (it can explore as part of planning).

## Phase 1 — Subagent plans

Write a self-contained prompt. The subagent has zero context from this conversation. It must include:

- Goal in one sentence.
- File paths, functions, or symbols it should start from.
- Known constraints and anything already ruled out.
- Desired output shape — findings **and** a concrete step-by-step implementation plan (not just exploration).
- The explicit directive: `Read-only: do not edit, write, create, or delete any files. No mutating bash commands. Report only.`

Invoke the `Agent` tool with `subagent_type: "Explore"` or `"Plan"` and the prompt.

## Phase 2 — Present and wait

Output one line: `Delegated to <agent>: <one-line goal>.`

Then the subagent's plan, lightly trimmed to remove restated context.

Stop. Wait for "go" / "yes" / "approved" / feedback. If the user amends, refine the prompt and re-run Phase 1.

## Phase 3 — Worktree

On approval:

1. `git branch --show-current` → current branch name.
2. Derive a feature slug from the task: kebab-case, `[a-z0-9-]` only, ≤ 32 chars.
3. Call `EnterWorktree` with `name: "<current-branch>-<feature-slug>"`.

The worktree starts at HEAD. All subsequent edits happen inside it.

## Phase 4 — Execute

Implement the approved plan inside the worktree. Minimal prose, minimal code, minimal comments.
