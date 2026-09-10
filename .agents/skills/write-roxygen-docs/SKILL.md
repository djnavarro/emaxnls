---
name: write-roxygen-docs
description: Guidance for writing and reviewing roxygen2 documentation comments in the emaxnls R package. Use whenever adding a new exported function, editing an existing @param/@returns/@details/@examples block, or reviewing a roxygen comment before running devtools::document().
---

# Writing roxygen2 Documentation

Roxygen comments become the content of `?function` and the pkgdown reference
site — they are read by a human user deciding whether and how to call a
function, not by a future contributor or an agent. Getting the mechanics
right (tags, `@export`, blank lines) is the easy part; agents reliably get
that right already. The failure modes worth guarding against are about
*content*: putting the wrong thing in a section, writing at the wrong level
of detail, or leaking information that shouldn't be there at all.

This skill assumes familiarity with roxygen2 basics. For deeper background on
any topic below, see [R Packages (2e), ch. 16](https://r-pkgs.org/man.html).

## What goes where

Each part of the introduction has a distinct job. Don't let content drift
into the wrong one:

- **Title** (first sentence, sentence case, no full stop): what the function
  does, distinguishing it from sibling functions. For emaxnls's paired
  continuous/binary families (`emax_nls()` vs `emax_logistic()`,
  `emax_nls_init()` vs `emax_logistic_init()`, `emax_nls_options()` vs
  `emax_logistic_options()`), the title should name what the function does
  *and* which response type it applies to (e.g. "Estimate parameters for an
  Emax regression model" for the continuous fit, "Estimate parameters for a
  logistic Emax regression model" for the binary one), not a generic verb
  that could describe either.
- **Description** (next paragraph): one paragraph on *this* function's
  purpose, in different words than the title — not a restatement of it, and
  not a template copied from a sibling function. It's easy to start the
  logistic counterpart of an existing continuous-response function by
  copying its docs and forget to re-target the description; the tell is a
  description that reads correctly for *either* member of the pair but never
  actually says what makes this one distinct (e.g. IRLS vs Gauss-Newton/Port/
  Levenberg-Marquardt, or a probability-scale vs response-scale return value)
  — that's a sign the real description got left in `@details` instead. It's
  just as easy to skip the description paragraph entirely — roxygen2 doesn't
  warn you, it silently reuses the title as the description, which is
  *always* a restatement by construction. If a topic documents several
  functions/methods together (via `@name`/`@rdname`, e.g. a shared
  `print()`/`summary()` topic covering both `emaxnls` and `emaxlogistic`
  objects), write a description that names what the group has in common, not
  just a copy of the shared title.
- **`@details`**: everything else — default behaviour, edge cases, how an
  argument being `NULL` is treated (e.g. `covariate_model = NULL` fitting an
  intercept-only hyperbolic model), interactions with other parts of the
  fitting/SCM pipeline. It's fine for this to be a few sentences to a short
  paragraph. Details render *after* arguments and return value on the help
  page, so don't put anything here that a reader needs before they can parse
  `@param`.
- **`@param`**: a succinct summary of what the argument controls and, if it
  has a fixed set of values (like `optim_method`), what they are. State the
  default inline (e.g. "The default is `optim_method = \"gauss\"`") since the
  usage block and the argument description are far apart on the rendered
  page. When the default is a sentinel like `NULL` whose *effect* isn't
  obvious from the value itself, say what it does rather than just naming it
  (e.g. "When `NULL` (the default), an intercept-only hyperbolic Emax model
  is fitted" reads better than "the default is `covariate_model = NULL`",
  which tells the reader nothing until they go read `@details` too).
- **`@returns`**: the shape of the return value — for the fitting functions,
  "An object of class `emaxnls`" or "An object of class `emaxlogistic`"; for
  SCM functions, the class of the returned history/model object; for S3
  methods, the concrete type returned (e.g. a `data.frame`, a named numeric
  vector). Every exported function must have this tag.
- **`@examples`**: runnable code showing typical usage. Not a place to
  re-explain arguments already covered in `@param`. Fitting examples should
  pass `opts = emax_nls_options(max_time = 10)` (or the logistic equivalent)
  so example runs cannot hang.

## Calibrating detail

Match documentation density to how novel the content actually is:

- Across a paired family — `emax_nls()`/`emax_logistic()`,
  `emax_nls_init()`/`emax_logistic_init()`,
  `emax_nls_options()`/`emax_logistic_options()` — the argument shape is
  largely identical (`structural_model`, `covariate_model`, `data`, `init`/
  `opts`). That similarity belongs in the title/description pattern, not
  restated at length in every `@details`. Spend the words on what's
  actually distinctive: IRLS vs the three NLS algorithms, or predictions on
  the probability scale vs the response scale.
- A dense wall of text is harder to scan than the same information broken
  into a sentence or two per idea, or a short bullet list (roxygen2 markdown
  supports `* item` lists in any prose section). Prefer that when an
  argument has more than two or three possible values or behaviours (e.g.
  the three `optim_method` choices for `emax_nls_options()`).
- Don't pad a short, genuinely simple function's documentation just to make
  it look thorough. If the description already says everything, an empty or
  one-line `@details` — or omitting the tag — is correct.
- Once `@details` covers more than three or four distinct sub-topics (e.g.
  `summary()` documenting `suppress_nonsensical`, `p_adjust`, and
  `simultaneous` together), break it into markdown headings or `@section`
  blocks, one per sub-topic, instead of one long unbroken block of
  paragraphs. A reader looking for one specific fact shouldn't have to read
  the whole section serially to find it.
- `@section` titles must be capitalized (R Core's own
  [Rd file guidelines](https://developer.r-project.org/Rds.html) state this
  explicitly for both `\title` and `\section` titles). Don't just reuse a
  lowercase identifier (e.g. a returned list element's name) verbatim as a
  heading -- prefer a short, readable capitalized phrase (e.g. "Library
  paths" rather than "libpaths"), and refer to the actual identifier in the
  body text instead, in backticks.

## Keep it user-facing

Roxygen documentation ships to end users via `?function` and pkgdown. It is
governed by the same boundary as `NEWS.md` (see the `write-news-entries`
skill), and a couple of emaxnls-specific ones:

- **Never reference agent- or contributor-facing material.** No mentions of
  skills, `AGENTS.md`, internal helper naming conventions, or "how this is
  implemented internally." A user reading `?emax_nls` has no use for the
  fact that it delegates to `.emax_nls()`, or that `emax_nls_options()`
  validation happens in `.validate_*()` helpers.
- **Don't describe internal implementation details that could change.**
  Explain behaviour in terms of what the function does and what the user
  observes (inputs accepted, outputs produced, what the fitted object
  contains), not the private helper functions or data structures used to get
  there — e.g. document that `simulate()` degrades gracefully when `mvtnorm`
  is unavailable at runtime, but not the specific Cholesky fallback code
  path unless a user genuinely needs to know which sampling method produced
  their draws. Naming a dot-prefixed internal function in documentation also
  invites users to reach for it with `:::`, which is best avoided. If you
  find yourself writing "internally, this calls..." or "`.nls_call()` is
  used to...", cut it unless the behavioural consequence (not the function
  name) is genuinely useful to a user.
- **Write for a reader who has never seen the source.** Avoid phrasing that
  only makes sense with the R script open (e.g. "as shown above", "the
  parameter vector described earlier" referring to code, not prose already
  in the same doc).
- **Cross-reference with square brackets, not just backticks.** Writing
  `` `emax_nls_options()` `` renders as code but produces no link;
  `[emax_nls_options()]` (or `[erplots::er_plot_add_model()]` for another
  package) is what roxygen2/pkgdown turn into an actual hyperlink. A backtick-only
  mention anywhere a function is referenced — in `@details`, `@seealso`, or
  prose — is a broken cross-reference, not a stylistic choice. When the
  natural link text is a function call but the useful target is a
  different topic (e.g. mentioning `as.data.frame()` where the real
  explanation lives on the `coercion_methods` topic), use
  `` [`as.data.frame()`][coercion_methods] `` for custom link text rather
  than linking to the (less useful) base generic's own page.
- **Check the compiled `.Rd` for stray aliases, not just the prose.** A
  `@name`/`@rdname` block (shared docs for several functions/methods)
  attaches to whichever R object immediately follows it in the file. If an
  internal, unrelated object — e.g. a private lookup table — sits between
  the block and its intended first function, roxygen2 silently attaches the
  block to that object instead and gives it a public `\alias`, which can
  surface as nonsense in the rendered `\usage{}`. Reading the roxygen
  comments won't reveal this; open the generated `man/*.Rd` and check that
  every `\alias{}` is something the topic is actually meant to document.

## Checklist before finishing a roxygen block

- [ ] Title names what's distinctive about this function; description
      says the same thing in different words, not a restatement of the title.
- [ ] Description actually describes *this* function, not a template
      inherited from a sibling function that describes the family in
      general instead.
- [ ] An explicit `@description` paragraph was actually written, distinct
      from the title — not left to roxygen2's default of silently reusing
      the title verbatim.
- [ ] Everything in `@details` is genuinely additional to the description,
      not filler to make the section non-empty.
- [ ] If `@details` covers more than three or four sub-topics, it's broken
      into headings/`@section`s rather than left as one long block.
- [ ] Every `@section` title is capitalized, and reads as a short phrase
      rather than a bare lowercase identifier copied from the code.
- [ ] Every `@param` states the default where one exists, and enumerates
      fixed value sets.
- [ ] `@returns` is present and names the concrete class returned.
- [ ] Every reference to another function (in `@details`, `@seealso`, or
      prose) uses square brackets so it actually renders as a link, not
      backticks alone.
- [ ] No mention of skills, `AGENTS.md`, internal dot-prefixed functions, or
      other agent/contributor-facing material.
- [ ] No description of implementation details a user would need `:::` to
      verify — only observable inputs/outputs/behavior.
- [ ] For `@name`/`@rdname` topics, every `\alias{}` in the compiled
      `man/*.Rd` is something the topic is actually meant to document — no
      internal object picked up by accident.
- [ ] Ran `devtools::document()` and skimmed the rendered `man/*.Rd` (or
      `?function` output) rather than just the roxygen comment source. If
      the fix touched code (not just comments) — e.g. reordering
      definitions to fix an alias leak — also ran `devtools::test()`.
