---
name: patch
description: >
  Use when the user wants code changes delivered as a .patch file instead of applied
  directly to the working tree. Triggers on "/patch <instruction>", "give me a patch
  for X", "make a .patch file", "diff this change for me". The skill edits inside a
  throwaway worktree, writes a .patch to .claude/patches/, then destroys the worktree.
version: 0.1.0
---

# patch

Produce a `.patch` file for the requested change. The user's working tree is never touched.

## Rules

- Keep prose terse. No preamble, no recap.
- Edit inside a throwaway worktree — never the main tree.
- Do not commit. Do not leave the worktree behind.
- Patch file lands in the main repo's `.claude/patches/` — absolute path, written before worktree cleanup.

## Flow

1. Capture the repo root and current branch:
   ```
   REPO=$(git rev-parse --show-toplevel)
   BRANCH=$(git branch --show-current)
   ```
2. **Sanity-check with the user before editing.** Print one line: `Operating in: <REPO> (branch: <BRANCH>).` In multi-worktree setups, the session's cwd can differ from where the user actually works. If the printed path is wrong, the user will abort here — cheaper than discovering the mismatch after the edits.
3. Ensure the output dir exists: `mkdir -p "$REPO/.claude/patches"`.
4. Derive a slug from the instruction: kebab-case, `[a-z0-9-]`, ≤ 40 chars. Patch filename: `YYYY-MM-DD-<slug>.patch`. If the file exists, append `-2`, `-3`, …
5. `EnterWorktree` (name optional — the worktree is ephemeral).
6. Make the edits inside the worktree, per the user's instruction.
7. Build the patch (absolute path so it writes to the main repo, not the worktree):
   ```
   git add -A
   git diff --cached > "$REPO/.claude/patches/<filename>.patch"
   ```
   `add -A` catches untracked files; `diff --cached` then captures everything vs HEAD.
8. `ExitWorktree` with `action: "remove", discard_changes: true`.
9. Report with the **absolute** path (not relative — avoids confusion when the user's terminal cwd differs from the skill's repo root). Include `--stat` summary on one line. Apply hint: `git apply <absolute-path>`.

## Notes

- No `--binary`. Binary changes won't round-trip; warn if any binary file was touched.
- If the edit produces zero diff, say so and skip writing the patch (do not create an empty file).
- `.claude/` is assumed to be gitignored (globally or per-repo); the skill does not manage `.gitignore`.
- If `EnterWorktree` creates the worktree from a different HEAD than the session's cwd (common in multi-worktree repos — the tool may use the primary checkout's HEAD), **proceed anyway**. Don't exit-and-retry with `git worktree add` by hand. Just mention the base commit in the final report so the user knows what the patch applies against; they will resolve any conflicts themselves.
