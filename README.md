[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/eglot-grammarly.svg)](https://jcs-emacs.github.io/jcs-elpa/#/eglot-grammarly)

# eglot-grammarly

[![CI](https://github.com/emacs-grammarly/eglot-grammarly/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-grammarly/eglot-grammarly/actions/workflows/test.yml)

`eglot` client leveraging [grammarly-language-server](https://github.com/znck/grammarly).

![](./etc/screenshot.png)

## 💾 Quickstart

### 🔍 Step 1. Install `Grammarly` language server through `NPM`

First, install the language server using NPM.

```sh
npm install -g @emacs-grammarly/grammarly-languageserver
```

### 🔍 Step 2. Configure your Emacs configuration

Consider adding this to your configuration.

```el
(use-package eglot-grammarly
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'eglot-grammarly)
                       (call-interactively #'eglot))))
```

If you use straight.el, you can also get this package directly from github.

```el
(use-package eglot-grammarly
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
  :defer t  ; defer package loading
  :hook ((text-mode markdown-mode). (lambda ()
                                      (require 'eglot-grammarly)
                                      (eglot-ensure))))
```

## 🔧 Configuration

Create `.dir-locals.el` file in the the project root directory.

```el
((nil
  (eglot-workspace-configuration
   . ((@emacs-grammarly/grammarly-languageserver
       . ((audience . "knowledgeable")))))))
```

*P.S. See all possible configuration [here](https://github.com/znck/grammarly#extension-settings).*

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
