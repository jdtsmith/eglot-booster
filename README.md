# eglot-booster: Boost eglot using lsp-booster

The [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster) project provides a rust-based wrapper program which substantially speeds up emacs' interactions with lsp servers.   This small package enables [eglot](https://github.com/joaotavora/eglot) to use it.

## Install/Usage

Install directly from this repo via `package-vc-install`, or using e.g. `straight`.  Then, in your init, simply 

```elisp
(use-package eglot-booster
	:after eglot
	:config	(eglot-booster-mode))
```

Then just use eglot as normal.  You should notice no differences other than speedier performance and less I/O blocking.

To verify that the wrapper is functioning, `M-x eglot-events-buffer` and look at the beginning for `emacs_lsp_booster::app` notices.

> [!IMPORTANT]
> At present only local lsp server programs which communicate by standard input/output can be wrapped, not lsp servers communicating over network ports (local or remote).

## Testing

Maybe you don't even need this.  You can `M-x eglot-booster` to disable the booster at any time.  Then `M-x eglot-shutdown-all`, restart eglot (`M-x eglot` is usually enough) in a large/heavy-weight file, and compare.
