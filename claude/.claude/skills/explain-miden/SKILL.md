---
name: explain-miden
description: >
  Generate thorough visual explanations of Miden ecosystem concepts as an org-mode file
  optimized for Emacs HTML export. Use this skill whenever the user asks for an explanation,
  comparison, deep-dive, or reference page about any Miden topic — VM instructions,
  execution model, memory layout, stack behavior, MAST, contexts, protocol, notes, accounts,
  transactions, client, compiler, or any other concept. Also trigger when the user says
  "explain", "compare", "how does X work in Miden", or "make a page about X".
---

# explain-miden

Generate rich, visual technical explanations of Miden ecosystem concepts as org-mode documents.

## Workflow

### 1. Research the codebases

Before writing anything, deeply understand the topic by reading primary sources. The user has local clones of the Miden repositories listed in `/Users/fabri/Repositories/shell-scripts/repos.txt`. As of this writing, they are:

- `/Users/fabri/Repositories/miden-client/miden-client-original`
- `/Users/fabri/Repositories/miden-protocol/protocol-original`
- `/Users/fabri/Repositories/miden-compiler/miden-compiler-original`
- `/Users/fabri/Repositories/miden-vm`

Always read `repos.txt` at the start of a run to get the current paths — they may change.

**Research order:**

1. **Documentation first** — search `docs/` directories for relevant markdown files. These contain the canonical explanations.
2. **Source code** — read the implementation files that back up the documented behavior.
3. **Tests** — check test modules for concrete usage examples that illustrate the behavior.

Use the Agent tool with `subagent_type: "Explore"` for broad research across the codebases. Launch multiple explore agents in parallel if the topic spans several repositories or subsystems.

Record every file you reference — you will need these for footnotes.

### 2. Generate the org-mode file

Write a single `.org` file to `/Users/fabri/docs/{topic}.org`.

Structure:

```org
#+TITLE: {Topic Title}
#+DATE: {generation date in YYYY-MM-DD format}
#+OPTIONS: toc:2 num:nil

{Introduction paragraph with footnotes}

* Overview
{Overview subsections for each concept being explained}

* {Comparison / Feature Table}
{Org-mode table with | delimiters}

* {Visual Sections — e.g., Stack Behavior, Memory Layout}
{Prose explanation followed by SVG diagrams}

* Annotated Example
#+BEGIN_SRC masm
{real example from docs or tests}
#+END_SRC
{Step-by-step walkthrough}

* When to Use Which / Guidance
{Practical recommendations}

* Footnotes
[fn:1] ...
[fn:2] ...
```

The exact sections will vary by topic — adapt the structure to what makes sense. Always include at least: an overview, a comparison or feature table if applicable, visual diagrams, and a worked example.

### 3. SVG diagrams

Use `#+BEGIN_EXPORT html` / `#+END_EXPORT` blocks containing inline SVG. This is the only way to get proper diagrams through org-mode's HTML export. Never use ASCII art.

**SVG style rules:**
- Font: `font-family: monospace`
- Primary strokes/text: `#222`
- Secondary/dimmed: `#999`, `#ccc`
- Fills: `#e8e8e8`, `#eee`, `#f0f0f0`, `#ddd` — all grayscale
- Stroke widths: 1-1.5px for boxes, 2px for emphasis lines
- Dashed lines for hidden/inactive elements: `stroke-dasharray="4,3"` or `"5,3"`
- No color — everything grayscale
- Center diagrams: `style="display: block; margin: 1em auto;"`

### 4. Footnotes and sourcing

Every factual claim must have a footnote pointing to its source.

**Format:**
```org
some claim[fn:1]
...
[fn:1] [[https://github.com/0xPolygonMiden/miden-vm/blob/next/path/to/file_name.rs][path/to/file_name.rs]]
```

Map the local repo paths to their GitHub counterparts:
- `miden-vm` -> `https://github.com/0xPolygonMiden/miden-vm/blob/next/`
- `miden-client-original` -> `https://github.com/0xPolygonMiden/miden-client/blob/main/`
- `protocol-original` -> `https://github.com/0xPolygonMiden/miden-node/blob/main/`
- `miden-compiler-original` -> `https://github.com/0xPolygonMiden/compiler/blob/main/`

Use the actual file paths you read during research.

### 5. Org-mode conventions

- Inline code: `=code=`
- Emphasis: `/italic/`, `*bold*`
- Superscripts: `2^{31}` (org renders this correctly)
- Footnotes: `[fn:N]` inline, with definitions at the bottom
- Source blocks: `#+BEGIN_SRC masm` for Miden assembly, `#+BEGIN_SRC rust` for Rust, etc.
- Do NOT put `[fn:N]` references inside org-mode table cells — Emacs HTML export tries to resolve them as links and fails with "Unable to resolve link". Cite table rows in the surrounding prose instead.

### 6. Cross-link related articles

After writing the new `.org` file, check whether any other `.org` file already in `/Users/fabri/docs/` covers a related Miden topic. For each related article found:

1. **In the new article**, append a `* Related articles` heading (place it just before `* Footnotes`) that lists the related pages as org links:
   ```org
   * Related articles
   - [[file:other-topic.org][Other Topic Title]] — one-line description of how it relates
   ```

2. **In each related article**, insert (or extend) its own `* Related articles` heading to reference the new article, using the same format.

**How to decide what's related:** read the `#+TITLE` and the first paragraph of each existing `.org` file. Two articles are related when they share a major subsystem, build on the same primitive, or when a reader of one would plausibly want to read the other. Skip tenuous connections — err on the side of fewer, higher-quality links.

Use relative filename links (`file:foo.org`) so the references work both in Emacs and after HTML export.

## Checklist before delivering

- [ ] `.org` file written to `/Users/fabri/docs/`
- [ ] `#+DATE:` header present with generation date
- [ ] All factual claims have footnotes
- [ ] All footnote links point to real files on the correct GitHub branch
- [ ] SVG diagrams via `#+BEGIN_EXPORT html` (no ASCII art)
- [ ] No colors in diagrams — everything grayscale
- [ ] At least one annotated code example with walkthrough
- [ ] No `[fn:N]` references inside table cells
- [ ] Related articles in `/Users/fabri/docs/` cross-linked both ways under a `* Related articles` heading
