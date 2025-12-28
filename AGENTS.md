# Agent Style Guide

## Types and annotations

- Prefer plain record access.
- Put type annotations at module boundaries or function signatures when the compiler requires it for
  type-inference.
- When designing types and modules, prefer to give a module a single type named
  `t` (e.g.,`Game.t`in `src/game.ml`). Then all logic related to that [t] should
  live in that file.
- Avoid extra module-qualification noise or repeating annotations inside bodies.
- Use short, direct matches with as few module qualifiers as required by the compiler

## Naming and organization

- Use snake_case for all functions and variables
- Keep code concise; refactor only when it improves clarity or reduces noise.
- Remove obsolete files/modules after migrations.
- Avoid leaving dead code or sprawling boilerplate.

## Module boundaries

- Keep All Js_of_ocaml code, such as DOM, storage, timers, and side effects in `Bindings`.
- Keep core game logic pure and data-only (e.g., `src/game.ml`, `src/render.ml`).
- Avoid leaking side effects into game logic modules.

## Imports and opens

- Use `open Bindings` at the top of each file that uses bindings.
- Prefer explicit module qualifiers elsewhere (e.g., `Game.step`, `Canvas.fill_rect`).

## Comments and documentation

- Use comments to explain why, not what; keep them short and focused.
- Prefer self-explanatory names over comments that restate the code.
- Avoid large comment blocks or redundant inline commentary.

## Checking correctness

- Always run `dune build` to ensure the build passes.
