;;; eglot-grammarly.el --- Eglot Clients for Grammarly  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Shen, Jen-Chieh
;; Created date 2021-03-04 11:34:54

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-grammarly/eglot-grammarly
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (eglot "1.4"))
;; Keywords: convenience eglot grammarly checker

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Eglot server implementation for Grammarly
;;

;;; Code:

(require 'eglot)
(require 'org-protocol)

(defgroup eglot-grammarly nil
  "Settings for the Grammarly Language Server.

Link: https://github.com/znck/grammarly"
  :group 'eglot
  :link '(url-link "https://github.com/emacs-grammarly/eglot-grammarly"))

(defcustom eglot-grammarly-active-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of major mode that work with Grammarly."
  :type 'list
  :group 'eglot-grammarly)

(defun eglot-grammarly--server-command ()
  "Generate startup command for Grammarly language server."
  (list 'eglot-grammarly-server "grammarly-languageserver" "--stdio"))

(add-to-list 'eglot-server-programs
             `(,eglot-grammarly-active-modes . ,(eglot-grammarly--server-command)))

(defclass eglot-grammarly-server (eglot-lsp-server) ()
  :documentation "A custom class for grammarly langserver.")

;; desktop
(defconst eglot-grammarly-client-id "client_MAFL6NoqazFobEJb9SxMib"
  "Client ID is required for language server's activation.")
;; web
;; (defconst eglot-grammarly-client-id "client_2tUGvu1E2Dfu2tWjwgSHFd"
;;    "Client ID is required for language server's activation.")

(cl-defmethod eglot-initialization-options ((server eglot-grammarly-server))
  "Passes through required grammarly initialization options"
    (list :clientId eglot-grammarly-client-id))

(defun eglot-grammarly-logout ()
  "Logout the grammarly account."
  (jsonrpc-request (eglot-current-server) :$/logout))

(defun eglot-grammarly--login-ok (server)
  "Whether user account is connected in SERVER."
  (jsonrpc-request server :$/isUserAccountConnected nil))

(defun eglot-grammarly-login-ok ()
  "Whether user account is connected."
  (interactive)
  (eglot-grammarly--login-ok (eglot-current-server)))

(defun eglot-grammarly-message ()
    (message (if (eglot-grammarly-login-ok) "Login successfully!" "Login failed!")))

(defvar eglot-grammarly--last-server nil "Last seen server")

(defun eglot-grammarly-login ()
  "Login to the grammarly account."
  (interactive)
  (let* ((window-id (format "%s" (emacs-pid))))
    (setq eglot-grammarly--last-server (eglot-current-server))
    (browse-url (jsonrpc-request eglot-grammarly--last-server :$/getOAuthUrl "org-protocol://eglot-grammarly/auth/callback"))))

(defun eglot-grammarly-on-oauth-redirected (data)
  "Handle oauth code in DATA sent from org-protocol."
  (let* ((params (org-protocol-parse-parameters data nil '(:code :state)))
         (code (plist-get params :code))
         (state (plist-get params :state)))
    (setq code (concat "org-protocol://?code=" code "&state=" state))  ; This is a non-documented magic...
    (jsonrpc-request eglot-grammarly--last-server :$/handleOAuthCallbackUri code)
    (message (if (eglot-grammarly--login-ok eglot-grammarly--last-server) "Login successfully!" "Login failed!")))
  nil)

(add-to-list 'org-protocol-protocol-alist
             '("eglot-grammarly"
               :protocol "eglot-grammarly/auth/callback"
               :function eglot-grammarly-on-oauth-redirected))

(provide 'eglot-grammarly)
;;; eglot-grammarly.el ends here
