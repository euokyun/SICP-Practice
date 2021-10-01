# SICP-Practice
personal SCIP practice - on progress

## VSCode Settings

Extension : [Magic Racket](https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket)

```bash
brew install minimal-racket
raco pkg install sicp
raco pkg install racket-langserver
```
and follow Magic Racket's instruction.

[Notion link](https://euokyun.notion.site/SICP-4c0c533dcfbc447a9707bb01b7eb2e6c)(Korean)

## Emacs Settings

### Packages
- `geiser`
- `racket-mode`
<!-- - `geiser-racket` -->
- `lsp-mode` - make sure add :hook 'racket-mode
- `company`
- `company-box`
- `flycheck`

install Racket and [racket-langserver](https://github.com/jeapostrophe/racket-langserver)

<!-- you can't set `#lang` inside REPL, so you need to start racket REPL with `geiser-mode-switch-to-repl-and-enter` (`C-c C-a`, <normal-state> `g Z`) -->
you can't set `#lang` inside REPL, so first you need to write `#lang sicp` in file's first line, and start racket REPL with `racket-repl` (`C-c C-z`), and `racket-run-module-at-point` (`C-c C-k`)



## note
* unlike [mit-scheme](https://www.gnu.org/software/mit-scheme/), racket don't allow re-define identifier. so some of my code has problems.

* paint/painters.ss from [nosyu](https://nosyu.pe.kr/1354)
