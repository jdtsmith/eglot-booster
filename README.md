# eglot-booster: Boost eglot using lsp-booster

The [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster) project provides a Rust-based wrapper program which substantially speeds up Emacs' interactions with LSP servers. This small package enables [eglot](https://github.com/joaotavora/eglot) to use it.

## Install/Usage

Install directly from this repo via `M-x package-vc-install` (pasting in this URL), or using `straight`. Then, in your init:

```elisp
(use-package eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))
````

### Using `straight`:

```elisp
(use-package eglot-booster
  :straight (eglot-booster
             :type git
             :host nil
             :repo "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))
```

### Using `use-package` with the built-in `:vc`

```elisp
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))
```

Then just use Eglot as normal. You should notice no differences other than speedier performance and less I/O blocking.

To verify that the wrapper is functioning, run:

```
M-x eglot-events-buffer
```

and look near the beginning for `emacs_lsp_booster::app` notices. If you'd like to avoid boosting remote servers (those run over TRAMP), set:

```elisp
(setq eglot-booster-no-remote-boost t)
```

> [!IMPORTANT]
> At present only local or TRAMP-based LSP servers communicating over stdin/stdout can be wrapped—not LSP servers communicating over network ports (local or remote). Using remote servers over TRAMP requires installing `emacs-lsp-booster` on the remote machine.

## Testing

You can disable boosting at any time with:

```
M-x eglot-booster
```

Then run `M-x eglot-shutdown-all`, restart Eglot (`M-x eglot`), and compare performance on a large/heavy file before/after.

## I/O Only Mode

> [!NOTE]
> Emacs v30 introduced a much faster JSON parser. Testing shows it decodes faster than bytecode, while bytecode still gives faster I/O. With bytecode disabled, small messages are slightly slower but large ones are faster. Worth testing for your workflow.

`emacs-lsp-booster` provides the flag `--disable-bytecode`.
Setting:

```elisp
(setq eglot-booster-io-only t)
```

or in `use-package`:

```elisp
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :custom
  (eglot-booster-io-only t)   ;; Enable IO-only mode
  :config
  (eglot-booster-mode))
```

enables IO-only mode. This retains fast buffered I/O while using Emacs 30’s much faster native JSON parser.
