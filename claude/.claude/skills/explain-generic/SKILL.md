---
name: explain-generic
description: >
  Generate thorough visual explanations of any technical concept as an org-mode file
  optimized for Emacs HTML export. Use this skill whenever the user asks for an explanation,
  comparison, deep-dive, or reference page about any topic — language features, runtime
  behavior, memory layout, data structures, protocols, APIs, architectures, algorithms,
  tools, or any other concept. Also trigger when the user says "explain", "compare",
  "how does X work", or "make a page about X".
---

# explain-generic

Generate rich, visual technical explanations of arbitrary concepts as org-mode documents.

## Workflow

### 1. Research the source material

Before writing anything, deeply understand the topic by reading primary sources. The set of sources depends on the topic:

- **Code-related topics**: read the relevant repositories. Start in the current working directory, and ask the user for any additional repo paths if the topic spans multiple codebases.
- **Standards / protocols**: read the canonical specification (RFC, W3C, language standard, etc.) — use `WebFetch` if it lives on the web.
- **Library / framework concepts**: read the official documentation first, then dive into the implementation.

**Research order:**

1. **Documentation first** — search `docs/`, `README.md`, and any `*.md` files for canonical explanations.
2. **Source code** — read the implementation files that back up the documented behavior.
3. **Tests** — check test modules for concrete usage examples that illustrate the behavior.

Use the Agent tool with `subagent_type: "Explore"` for broad research across a codebase. Launch multiple explore agents in parallel if the topic spans several repositories or subsystems.

Record every file or URL you reference — you will need these for footnotes.

### 2. Generate the org-mode file

Write a single `.org` file under `docs/{project-or-type}/{topic}.org` in the current working directory. The subdirectory should be:

- **The project name** for code-related topics (e.g. `docs/postgres/`, `docs/react/`, `docs/linux-kernel/`). Derive it from the basename of the repo root, or from `git remote get-url origin`.
- **A type/category name** when no single project applies (e.g. `docs/protocols/`, `docs/algorithms/`, `docs/languages/`, `docs/tools/`).

Create the directory if it does not exist. If the user specifies a different output location, honor that instead.

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

* {Visual Sections — e.g., Memory Layout, Control Flow, Data Structures}
{Prose explanation followed by SVG diagrams}

* Annotated Example
#+BEGIN_SRC {language}
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

**Format for code citations:**
```org
some claim[fn:1]
...
[fn:1] [[https://github.com/{owner}/{repo}/blob/{branch}/path/to/file_name.rs][path/to/file_name.rs]]
```

**Format for web citations:**
```org
some claim[fn:2]
...
[fn:2] [[https://example.com/spec#section-3.2][Spec §3.2 — Section Title]]
```

To build a GitHub URL for a file you read locally, run `git -C {repo} remote get-url origin` and `git -C {repo} rev-parse --abbrev-ref HEAD` to derive the owner, repo, and default branch. Use the actual file paths you read during research — do not invent paths.

### 5. Org-mode conventions

- Inline code: `~code~` (tilde delimiters — this is the org-mode convention)
- Emphasis: `/italic/`, `*bold*`
- Superscripts: `2^{31}` (org renders this correctly)
- Footnotes: `[fn:N]` inline, with definitions at the bottom
- Source blocks: `#+BEGIN_SRC rust`, `#+BEGIN_SRC python`, `#+BEGIN_SRC c`, etc. — use the actual language of the example
- Do NOT put `[fn:N]` references inside org-mode table cells — Emacs HTML export tries to resolve them as links and fails with "Unable to resolve link". Cite table rows in the surrounding prose instead.

### 6. Cross-link related articles

After writing the new `.org` file, check whether any other `.org` file already in the same output directory covers a related topic. For each related article found:

1. **In the new article**, append a `* Related articles` heading (place it just before `* Footnotes`) that lists the related pages as org links:
   ```org
   * Related articles
   - [[file:other-topic.org][Other Topic Title]] — one-line description of how it relates
   ```

2. **In each related article**, insert (or extend) its own `* Related articles` heading to reference the new article, using the same format.

**How to decide what's related:** read the `#+TITLE` and the first paragraph of each existing `.org` file. Two articles are related when they share a major subsystem, build on the same primitive, or when a reader of one would plausibly want to read the other. Skip tenuous connections — err on the side of fewer, higher-quality links.

Use relative filename links (`file:foo.org`) so the references work both in Emacs and after HTML export.

## Checklist before delivering

- [ ] `.org` file written under `docs/{project-or-type}/`
- [ ] `#+DATE:` header present with generation date
- [ ] All factual claims have footnotes
- [ ] All footnote links point to real files on the correct branch, or to real, reachable URLs
- [ ] SVG diagrams via `#+BEGIN_EXPORT html` (no ASCII art)
- [ ] No colors in diagrams — everything grayscale
- [ ] At least one annotated code example with walkthrough
- [ ] No `[fn:N]` references inside table cells
- [ ] Related articles in the same output directory cross-linked both ways under a `* Related articles` heading
