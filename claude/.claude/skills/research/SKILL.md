---
name: research
description: >
  Produce a thoroughly-sourced research report on any topic and save it as an
  org-mode file optimized for HTML export under `research/` in the current
  working directory. Use this skill when the user asks to "research X",
  "investigate X", "look into X", "/research X", or otherwise wants a
  written, citation-backed deep dive on a topic. The output is a single
  self-contained `.org` file ready to export to HTML via
  `org-html-export-to-html`.
---

# research

Generate a thorough, source-backed research report as an org-mode document optimized for HTML export.

## Workflow

### 1. Delegate the research

Use the Agent tool with `subagent_type: "researcher"` to gather material. Do **not** do the web searches yourself in the main conversation — the researcher subagent is purpose-built for sourced findings and keeps the main context clean.

When briefing the researcher:

- State the research question precisely.
- Enumerate the specific sub-questions you want answered (be exhaustive — the subagent only gets one shot with the prompt you give it).
- Ask for *direct quotes* on any load-bearing claim.
- Ask for primary sources (specs, standards, official docs, RFCs, kernel/compiler source, peer-reviewed papers) over Stack Overflow / blogs.
- Ask for a structured response with a summary table at the top if the topic admits one.
- Set a target length (typically 1500–3000 words depending on topic depth).

If the user's request is broad, you may launch multiple researcher subagents in parallel for independent sub-topics, then merge their findings.

**Never ask the user for permission** to run web searches or fetches — proceed.

### 2. Write the org file

Save to `{pwd}/research/{slug}.org`, where `{slug}` is a short kebab-case identifier derived from the topic (e.g. `rename_syscall_atomicity.org`, `tcp_congestion_control.org`).

If the `research/` directory does not exist, create it with `mkdir -p` — do not ask the user.

Writing under `research/` is pre-authorized in `~/.claude/settings.json` (via `Write(research/**)` and `Write(**/research/**)`). Just call `Write` directly — do not ask the user for permission and do not narrate "I'll write this to disk now" beforehand.

### 3. Required org-mode header

Every report starts with this header block (adapt title/subtitle/date/slug). The HTML configuration is what makes the file export cleanly to a self-contained HTML page — do not strip these headers down.

```org
#+TITLE: {Title}
#+SUBTITLE: {One-line subtitle clarifying scope}
#+AUTHOR: Tomas Fabrizio Orsi
#+EMAIL: tomas.orsi@lambdaclass.com
#+DATE: {YYYY-MM-DD generation date}
#+LANGUAGE: en

#+OPTIONS: toc:2 num:t H:4 ^:{} todo:nil tags:nil
#+OPTIONS: html-style:t html-postamble:auto html5-fancy:t
#+STARTUP: showall

#+HTML_DOCTYPE: html5
#+HTML_HEAD: <meta charset="utf-8">
#+HTML_HEAD: <meta name="viewport" content="width=device-width, initial-scale=1">
#+HTML_HEAD: <style>
#+HTML_HEAD:   :root { --fg: #222; --muted: #555; --bg: #fff; --code-bg: #f4f4f4; --border: #ddd; --accent: #0366d6; --quote: #888; --callout-bg: #fffbe6; --callout-border: #d4a017; }
#+HTML_HEAD:   body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", system-ui, sans-serif; max-width: 820px; margin: 2em auto; padding: 0 1em; line-height: 1.6; color: var(--fg); background: var(--bg); }
#+HTML_HEAD:   h1, h2, h3, h4 { color: #111; line-height: 1.25; }
#+HTML_HEAD:   h1 { font-size: 1.9em; border-bottom: 1px solid var(--border); padding-bottom: 0.3em; }
#+HTML_HEAD:   h2 { font-size: 1.5em; margin-top: 1.6em; }
#+HTML_HEAD:   h3 { font-size: 1.2em; }
#+HTML_HEAD:   a { color: var(--accent); text-decoration: none; }
#+HTML_HEAD:   a:hover { text-decoration: underline; }
#+HTML_HEAD:   code { background: var(--code-bg); padding: 1px 5px; border-radius: 3px; font-size: 0.92em; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
#+HTML_HEAD:   pre, pre.src { background: #f8f8f8; padding: 0.9em 1em; overflow-x: auto; border-radius: 4px; border: 1px solid var(--border); font-size: 0.9em; }
#+HTML_HEAD:   table { border-collapse: collapse; margin: 1em 0; width: 100%; }
#+HTML_HEAD:   th, td { border: 1px solid var(--border); padding: 0.5em 0.8em; text-align: left; vertical-align: top; }
#+HTML_HEAD:   th { background: var(--code-bg); }
#+HTML_HEAD:   blockquote { border-left: 3px solid var(--quote); margin: 1em 0; padding: 0.2em 1em; color: var(--muted); }
#+HTML_HEAD:   .takeaway { background: var(--callout-bg); border: 1px solid var(--callout-border); border-radius: 4px; padding: 0.8em 1em; margin: 1.2em 0; }
#+HTML_HEAD:   .takeaway p:first-child { margin-top: 0; }
#+HTML_HEAD:   .takeaway p:last-child { margin-bottom: 0; }
#+HTML_HEAD:   #footnotes { font-size: 0.9em; }
#+HTML_HEAD:   .footnum a, .footref { text-decoration: none; }
#+HTML_HEAD:   #table-of-contents { background: #fafafa; border: 1px solid var(--border); padding: 0.5em 1em; border-radius: 4px; }
#+HTML_HEAD: </style>
```

### 4. Body structure

Adapt the structure to the topic, but the following sections are recommended for most reports:

```org
* Executive summary
:PROPERTIES:
:UNNUMBERED: t
:END:
{2–4 paragraph TL;DR — what the answer is, with the most important caveats.
Each load-bearing claim should already carry a footnote here.}

* Quick-reference table
{If the topic admits one. Plain org tables render cleanly in HTML — no
special attributes needed for long tables.}

| Header A | Header B | Header C | Header D |
|----------+----------+----------+----------|
| ...      | ...      | ...      | ...      |

* {Topic-specific section 1}
* {Topic-specific section 2}
...

* Practical takeaway
{Concrete recommendations, ideally numbered. Highlight the most important one
in a callout:}

#+begin_takeaway
*Practical guidance:* ...
#+end_takeaway

* Source quality assessment
{Tier the sources: primary specs, expert commentary, blog/SO inferences. Make
clear which claims rest on weaker sources. Cite the footnote numbers, e.g.
"Tier 1 (primary specs): [fn:1], [fn:3], [fn:7]."}

* Footnotes
{All sources go here as numbered footnotes. This section IS the bibliography
— do not also add a separate "Sources" list.}
```

### 5. Org / HTML conventions

- **Inline code:** `~code~` (renders monospace as `<code>` in HTML).
- **Emphasis:** `/italic/`, `*bold*`.
- **Block quotes** for direct quotes from sources: `#+begin_quote` / `#+end_quote` (the footnote citing the source goes immediately after the closing line of the quote).
- **Highlighted callouts:** wrap with `#+begin_takeaway` / `#+end_takeaway`. Org exports these as `<div class="takeaway">…</div>`, styled by the `.takeaway` CSS class in the header.
- **HTML-only escapes:** wrap with `@@html:...@@` if you need raw HTML inside prose (e.g. `@@html:<kbd>Ctrl-C</kbd>@@`).
- **Long URLs in footnotes:** use `[[url][label]]` form so the rendered footnote shows a readable label rather than the bare URL.
- **Diagrams:** raw HTML export blocks are allowed for inline diagrams or embeds:
  ```org
  #+begin_export html
  <svg ...>...</svg>
  #+end_export
  ```
  Prefer prose + tables for most content; reserve raw HTML for diagrams that genuinely need it.

### 6. Citations — use footnotes, not a trailing list

**Every factual claim must carry an inline org-mode footnote.** Do not dump
sources in a closing list — the `* Footnotes` section at the bottom *is* the
bibliography, populated automatically by the inline references.

**Format:**

```org
POSIX guarantees that an observer of the destination path will see either the
old inode or the new one, never a missing entry[fn:1].

#+begin_quote
"a link named /new/ shall remain visible to other threads throughout the
renaming operation and refer either to the file referred to by /new/ or
/old/ before the operation began."
#+end_quote
[fn:1]

The Linux man page is even more emphatic[fn:2].

* Footnotes
[fn:1] POSIX.1-2017 ~rename~ — The Open Group, [[https://pubs.opengroup.org/onlinepubs/9699919799/functions/rename.html][pubs.opengroup.org]].
[fn:2] Linux ~rename(2)~ man page, [[https://man7.org/linux/man-pages/man2/rename.2.html][man7.org]].
```

Rules:

- One footnote per source occurrence; if the same source is cited twice,
  reuse the same `[fn:N]` number — do not allocate a new one.
- Place the `[fn:N]` immediately after the punctuation of the cited claim,
  with no space before it.
- For a block quote, place the `[fn:N]` on its own line directly after the
  `#+end_quote`.
- Footnote references inside table cells render fine in HTML (org generates
  the appropriate `<sup>` links), so use them freely if a row needs a
  citation.
- Footnote definitions live in a single `* Footnotes` section at the very
  bottom of the file. They should be numbered in order of first appearance.

### 7. HTML export

After writing the file, tell the user how to export it:

```
emacs research/{slug}.org --batch -f org-html-export-to-html --kill
```

Do **not** run the export yourself unless the user explicitly asks. Producing a stray HTML file without being asked is noise.

## Checklist before delivering

- [ ] Report saved to `{pwd}/research/{slug}.org`
- [ ] `#+TITLE`, `#+DATE`, and the full HTML header block (with embedded CSS) present
- [ ] Executive summary at the top
- [ ] Quick-reference table if the topic admits one
- [ ] Direct quotes used for load-bearing claims
- [ ] Every factual claim has an inline `[fn:N]` footnote
- [ ] Single `* Footnotes` section at the bottom — no separate "Sources" list
- [ ] Repeated sources reuse the same footnote number
- [ ] Primary sources cited; weak sources flagged in the source-quality section
- [ ] Raw HTML export blocks used only where a diagram or embed genuinely needs it
- [ ] Researcher subagent did the searches, not the main agent
- [ ] User informed of the export command
