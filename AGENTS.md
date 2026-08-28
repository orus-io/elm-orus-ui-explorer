# AGENTS.md

Elm package library (`elm.json` type `package`), `orus-io/elm-orus-ui-explorer`
— an extendable UI Explorer / showcase framework built on top of
[`elm-orus-ui`](https://github.com/orus-io/elm-orus-ui). Exposes `OUI.Explorer`
and `OUI.Showcase`.

## Toolchain

All tools pinned via [mise](https://mise.jdx.dev) in `mise.toml` (elm 0.19.2,
elm-format 0.8.8, elm-review 2.13.5, elm-go 5.0.20). Use `mise run`:

| Task | Runs | Scope |
|------|------|-------|
| `mise run build` | `elm make` | compile-check the package (`src/`) |
| `mise run build-showcase` | `elm make src/Main.elm --optimize` (in `showcase/`) | build the showcase app |
| `mise run showcase` | `elm-go` dev server (in `showcase/`) | live showcase at `public/` |
| `mise run review` | `elm-review` | static analysis — **`src/` only** |
| `mise run format-validate` | `elm-format --validate src` | formatting check — **`src/` only** |
| `mise run lint` | `format-validate` + `review` | full pre-commit check — **`src/` only** |

- `mise run lint` is the canonical verification step for the package. Run it before considering package work done.
- **`showcase/` is not covered by any lint/format task.** If you edit showcase code, there is no automated check; verify by running `mise run build-showcase` or `mise run showcase`.
- `format-validate` does **not** auto-fix. To fix formatting, run `elm-format src` directly (no `--validate`).
- No test suite: `elm.json` has empty `test-dependencies`, no `tests/` dir.
- `.opencode/opencode.jsonc` pre-allows `mise run *`

## Two-project structure

This repo contains two separate Elm projects:

1. **Package library** (`src/`) — `elm.json` type `package`, exposes
   `OUI.Explorer` and `OUI.Showcase`. `OUI.Explorer` is the explorer framework;
   `OUI.Showcase` provides ready-made showcase pages (`OUI.Showcase.Buttons`,
   `.Colors`, `.Tabs`, …) that plug into an explorer.
2. **Showcase app** (`showcase/`) — `showcase/elm.json` type `application`,
   entry point `showcase/src/Main.elm`. This is the live demo deployed at
   elm.orus.io.

The showcase app's `source-directories` pulls from **four** locations (see
`showcase/elm.json`):
- `src` — showcase app code (`Main.elm`, `ColorThemes.elm`)
- `../src` — the explorer package library (this repo)
- `../../elm-orus-ui/src` — **local source of the sibling `elm-orus-ui` repo,
  not the published package**
- `generated` — codegen output (Material Icons books)

**`elm-orus-ui` must be checked out as a sibling directory** (`../elm-orus-ui/`
relative to this repo root) for the showcase to compile. Changes to
`elm-orus-ui` source are picked up immediately by the showcase — no install
step.

## Codegen (Material Icons)

`showcase/codegen/` is an
[elm-codegen](https://github.com/mdgriffith/elm-codegen) project (uses its own
`elm.json`). It generates icon "book" modules:

- `Generate.elm` — codegen entry point; generates three files into
  `showcase/generated/IcidassetMaterialIcons/{Regular,Outlined,Round}.elm`.
- `MatIcons.elm` — the raw icon name list (categories → icon names), sourced
  from `icidasset/elm-material-icons`.
- `showcase/generated/` — **committed output**; don't hand-edit. The generated
  modules wrap Material Icons into `OUI.Explorer.Book` pages.
- No mise task for running codegen. Requires the `elm-codegen` CLI (not pinned
  in `mise.toml`); run manually from `showcase/codegen/` if icon lists change.

## elm-review constraints

`review/src/ReviewConfig.elm` is the same strict config as `elm-orus-ui`. Rules
most likely to trip on:

- **No `exposing (..)`** in module declarations or imports
  (`NoExposingEverything`, `NoImportingEverything`). List names explicitly.
- **Type annotations required** on all top-level and `let/in` bindings.
- **Exposed modules must be documented**: module docs with `@docs` for every
  exposed item (`Docs.NoMissing`). README/doc links validated.
- **No `Debug`** — `Debug.log`, `Debug.todo`, `toString` all banned.
- **Eta-reduce lambdas**: write `f`, not `\x -> f x`.
- **No unused** parameters, patterns, variables, constructors, or dependencies.
- `VariablesBetweenCaseOf.AccessInCases` forbids a specific case-of
  variable-access pattern.

## Misc

- Version bumps are manual in `elm.json` (`"version"` field).
- No CI workflows in-repo (`.github/` absent); `mise run lint` is the local
  source of truth.
- `showcase/docs.json` is an older copy of `elm-orus-ui`'s package docs;
  committed, don't hand-edit.
- `showcase/public/main.js` is a committed build artifact of the showcase.
