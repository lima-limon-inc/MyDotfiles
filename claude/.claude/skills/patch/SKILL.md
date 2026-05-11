---
name: patch
description: >
  Use when the user wants code changes delivered as a .patch file instead of applied
  directly to the working tree. Triggers on "/patch <instruction>", "give me a patch
  for X", "make a .patch file", "diff this change for me". The skill edits inside a
  throwaway worktree, writes a .patch to patches/, then destroys the worktree.
version: 0.1.0
allowed-tools:
  - Bash(make:*)
  - Bash(mkdir:*)
  - Bash(ls:*)
  - Bash(cp:*)
  - Bash(echo:*)
  - Bash(wc:*)
  - Bash(date:*)
  - Bash(git diff:*)
  - Bash(git diff --cached:*)
  - Bash(git diff --cached --stat:*)
  - Bash(git -C * diff:*)
  - Bash(git -C * status:*)
  - Bash(git -C * rev-parse:*)
  - Bash(git add -A:*)
  - Bash(git add:*)
  - Bash(git checkout:*)
  - Bash(git fetch:*)
  - Bash(git log:*)
  - Bash(git status:*)
  - Bash(git rev-parse:*)
  - Bash(git branch:*)
  - Bash(git show:*)
  - Bash(diff:*)
  - Read
  - Edit
  - Edit(**)
  - Edit(**/.claude/worktrees/**)
  - Write
  - Write(**)
  - Write(**/.claude/worktrees/**)
  - Glob
  - Grep
  - EnterWorktree
  - ExitWorktree
---

# patch

Produce a `.patch` file for the requested change. The user's working tree is never touched.

## Rules

- Keep prose terse. No preamble, no recap.
- Edit inside a throwaway worktree — never the main tree.
- **Edits inside the worktree (paths under `**/.claude/worktrees/**`) must not prompt for permission.** The worktree is throwaway and removed at the end of the skill — there is no risk to the user's tree, so do not ask. The `allowed-tools` list grants `Edit`, `Write`, `mkdir`, `git checkout`, `git fetch`, etc. for exactly this reason. If the harness still prompts, treat that as a bug to fix in the skill, not a checkpoint to bother the user with.
- Never `git push` from the worktree (the throwaway branch isn't meant to be published).
- `git commit` inside the throwaway worktree is allowed but unnecessary — the stage-mirror flow below produces a clean diff without one.
- Do not leave the worktree behind.
- Patch file lands in the main repo's `patches/` — absolute path, written before worktree cleanup.

## Flow

1. Capture the repo root and current branch:
   ```
   REPO=$(git rev-parse --show-toplevel)
   BRANCH=$(git branch --show-current)
   ```
2. **Sanity-check with the user before editing.** Print one line: `Operating in: <REPO> (branch: <BRANCH>).` In multi-worktree setups, the session's cwd can differ from where the user actually works. If the printed path is wrong, the user will abort here — cheaper than discovering the mismatch after the edits.
3. Ensure the output dir exists: `mkdir -p "$REPO/patches"`.
4. Derive a slug from the instruction: kebab-case, `[a-z0-9-]`, ≤ 40 chars. Patch filename: `YYYY-MM-DD-HH:MM:SS-<slug>.patch` (local time, e.g. `2026-04-30-14:30:22-fix-foo.patch`). If the file exists, append `-2`, `-3`, …
5. `EnterWorktree` (name optional — the worktree is ephemeral). If the task targets a specific branch (e.g. addressing review comments on a PR), `git checkout <that-branch>` inside the worktree before editing — fetch first if needed. This is preferred over producing a patch against the wrong base.
6. **Mirror the user's uncommitted state into the worktree, then stage it.** This makes the index = "user's current working tree", so a later plain `git diff` will show only the *new* edits.
   - For each modified file in the main tree (`git -C <REPO> status --short` lists them), copy the main-tree version into the worktree at the same relative path: `cp "<REPO>/<path>" "<path>"`.
   - Stage everything: `git add -A`.
   - The index now matches the user's working tree. **Do not commit.**
7. Make the new edits in the worktree (working tree only — not staged).
8. Build the patch (absolute path so it writes to the main repo, not the worktree):
   ```
   git diff > "$REPO/patches/<filename>.patch"
   ```
   Plain `git diff` (no `--cached`) compares working tree vs index. Since the index is the user's pre-edit state, the diff is exactly the new change.
9. `ExitWorktree` with `action: "remove", discard_changes: true`.
10. Report with the **absolute** path (not relative — avoids confusion when the user's terminal cwd differs from the skill's repo root). Include `--stat` summary on one line. Apply hint: `git apply <absolute-path>`.

## Notes

- No `--binary`. Binary changes won't round-trip; warn if any binary file was touched.
- If the edit produces zero diff, say so and skip writing the patch (do not create an empty file).
- The skill does not manage `.gitignore`. `patches/` lives at the repo root, so the user may want to gitignore it themselves.
- If `EnterWorktree` creates the worktree from a different HEAD than the session's cwd (common in multi-worktree repos — the tool may use the primary checkout's HEAD), don't exit-and-retry with `git worktree add` by hand. Either `git checkout` the right branch inside the worktree (preferred when the task targets a specific branch like a PR), or proceed against the worktree's HEAD and mention the base commit in the final report so the user can resolve conflicts.
