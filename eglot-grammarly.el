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


;;
;; (@* "Options" )
;;
;; From https://github.com/emacs-grammarly/lsp-grammarly/blob/master/lsp-grammarly.el

(defcustom eglot-grammarly-selectors
  []
  "Filter documents to be checked with Grammarly."
  :type 'vector
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-audience "knowledgeable"
  "Sets the default audience for every document."
  :type '(choice (const "general")
                 (const "knowledgeable")
                 (const "expert"))
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-dialect "american"
  "Sets the default dialect for every document."
  :type '(choice (const "american")
                 (const "australian")
                 (const "british")
                 (const "canadian")
                 (const "auto-text"))
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-domain "general"
  "Sets the default domain for every document."
  :type '(choice (const "academic")
                 (const "business")
                 (const "general")
                 (const "mail")
                 (const "casual")
                 (const "creative"))
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-conjunction-at-start-of-sentence
  nil
  "Flags use of conjunctions such as 'but' and 'and' at the beginning of
sentences."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-fluency
  t
  "Suggests ways to sound more natural and fluent."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-informal-pronouns-academic
  nil
  "Flags use of personal pronouns such as 'I' and 'you' in academic writing."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-missing-spaces
  t
  "Suggests adding missing spacing after a numeral when writing times."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-noun-strings
  t
  "Flags a series of nouns that modify a final noun."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-numbers-beginning-sentences
  t
  "Suggests spelling out numbers at the beginning of sentences."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-numbers-zero-through-ten
  t
  "Suggests spelling out numbers zero through ten."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-oxford-comma
  nil
  "Suggests adding the Oxford comma after the second-to-last item in a list of
things."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-passive-voice
  nil
  "Flags use of passive voice."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-person-first-language
  t
  "Suggests using person-first language to refer respectfully to an individual
with a disability."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-biased-language-age-related
  t
  "Suggests alternatives to potentially biased language related to older adults."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-biased-language-disability-related
  t
  "Suggests alternatives to potentially ableist language."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-biased-language-family-related
  t
  "Suggests alternatives to potentially biased language related to parenting and
family systems."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-biased-language-gender-related
  t
  "Suggests alternatives to potentially gender-biased and non-inclusive phrasing."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-biased-language-human-rights
  t
  "Suggests alternatives to language related to human slavery."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-biased-language-human-rights-related
  t
  "Suggests alternatives to terms with origins in the institution of slavery."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-biased-language-lgbtqia-related
  t
  "Flags LGBTQIA+-related terms that may be seen as biased, outdated, or
disrespectful in some contexts."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-biased-language-race-ethnicity-related
  t
  "Suggests alternatives to potentially biased language related to race and
ethnicity."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-possibly-politically-incorrect-language
  t
  "Suggests alternatives to language that may be considered politically
incorrect."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-preposition-at-the-end-of-sentence
  nil
  "Flags use of prepositions such as 'with' and 'in' at the end of sentences."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-punctuation-with-quotation
  t
  "Suggests placing punctuation before closing quotation marks."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-readability-fillerwords
  t
  "Flags long, complicated sentences that could potentially confuse your reader."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-readability-transforms
  t
  "Suggests splitting long, complicated sentences that could potentially confuse
your reader."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-sentence-variety
  t
  "Flags series of sentences that follow the same pattern."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-spaces-surrounding-slash
  t
  "Suggests removing extra spaces surrounding a slash."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-split-infinitive
  t
  "Suggests rewriting split infinitives so that an adverb doesn't come between
'to' and the verb."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-stylistic-fragments
  nil
  "Suggests completing all incomplete sentences, including stylistic sentence
fragments that may be intentional."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-unnecessary-ellipses
  nil
  "Flags unnecessary use of ellipses (...)."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-variety
  t
  "Suggests alternatives to words that occur frequently in the same paragraph."
  :type 'boolean
  :group 'eglot-grammarly)

(defcustom eglot-grammarly-suggestions-vocabulary
  t
  "Suggests alternatives to bland and overused words such as 'good' and 'nice'."
  :type 'boolean
  :group 'eglot-grammarly)







(cl-defmethod eglot-initialization-options ((server eglot-grammarly-server))
  "Passes through required grammarly initialization options"
  (list :clientId eglot-grammarly-client-id
        :grammarly.selectors eglot-grammarly-selectors
        :grammarly.config.documentDialect eglot-grammarly-dialect
        :grammarly.config.documentDomain eglot-grammarly-domain
        :grammarly.config.suggestions.ConjunctionAtStartOfSentence eglot-grammarly-suggestions-conjunction-at-start-of-sentence
        :grammarly.config.suggestions.Fluency eglot-grammarly-suggestions-fluency
        :grammarly.config.suggestions.InformalPronounsAcademic eglot-grammarly-suggestions-informal-pronouns-academic
        :grammarly.config.suggestions.MissingSpaces eglot-grammarly-suggestions-missing-spaces
        :grammarly.config.suggestions.NounStrings eglot-grammarly-suggestions-noun-strings
        :grammarly.config.suggestions.NumbersBeginningSentences eglot-grammarly-suggestions-numbers-beginning-sentences
        :grammarly.config.suggestions.NumbersZeroThroughTen eglot-grammarly-suggestions-numbers-zero-through-ten
        :grammarly.config.suggestions.OxfordComma eglot-grammarly-suggestions-oxford-comma
        :grammarly.config.suggestions.PassiveVoice eglot-grammarly-suggestions-passive-voice
        :grammarly.config.suggestions.PersonFirstLanguage eglot-grammarly-suggestions-person-first-language
        :grammarly.config.suggestions.PossiblyBiasedLanguageAgeRelated eglot-grammarly-suggestions-possibly-biased-language-age-related
        :grammarly.config.suggestions.PossiblyBiasedLanguageDisabilityRelated eglot-grammarly-suggestions-possibly-biased-language-disability-related
        :grammarly.config.suggestions.PossiblyBiasedLanguageFamilyRelated eglot-grammarly-suggestions-possibly-biased-language-family-related
        :grammarly.config.suggestions.PossiblyBiasedLanguageGenderRelated eglot-grammarly-suggestions-possibly-biased-language-gender-related
        :grammarly.config.suggestions.PossiblyBiasedLanguageHumanRights eglot-grammarly-suggestions-possibly-biased-language-human-rights
        :grammarly.config.suggestions.PossiblyBiasedLanguageHumanRightsRelated eglot-grammarly-suggestions-possibly-biased-language-human-rights-related
        :grammarly.config.suggestions.PossiblyBiasedLanguageLgbtqiaRelated eglot-grammarly-suggestions-possibly-biased-language-lgbtqia-related
        :grammarly.config.suggestions.PossiblyBiasedLanguageRaceEthnicityRelated eglot-grammarly-suggestions-possibly-biased-language-race-ethnicity-related
        :grammarly.config.suggestions.PossiblyPoliticallyIncorrectLanguage eglot-grammarly-suggestions-possibly-politically-incorrect-language
        :grammarly.config.suggestions.PrepositionAtTheEndOfSentence eglot-grammarly-suggestions-preposition-at-the-end-of-sentence
        :grammarly.config.suggestions.PunctuationWithQuotation eglot-grammarly-suggestions-punctuation-with-quotation
        :grammarly.config.suggestions.ReadabilityFillerwords eglot-grammarly-suggestions-readability-fillerwords
        :grammarly.config.suggestions.ReadabilityTransforms eglot-grammarly-suggestions-readability-transforms
        :grammarly.config.suggestions.SentenceVariety eglot-grammarly-suggestions-sentence-variety
        :grammarly.config.suggestions.SpacesSurroundingSlash eglot-grammarly-suggestions-spaces-surrounding-slash
        :grammarly.config.suggestions.SplitInfinitive eglot-grammarly-suggestions-split-infinitive
        :grammarly.config.suggestions.StylisticFragments eglot-grammarly-suggestions-stylistic-fragments
        :grammarly.config.suggestions.UnnecessaryEllipses eglot-grammarly-suggestions-unnecessary-ellipses
        :grammarly.config.suggestions.Variety eglot-grammarly-suggestions-variety
        :grammarly.config.suggestions.Vocabulary eglot-grammarly-suggestions-vocabulary
        ))


(defun eglot-grammarly-logout ()
  "Logout the grammarly account."
  (jsonrpc-request (eglot-current-server) :$/logout))

(defun eglot-grammarly--login-ok (server)
  "Whether user account is connected in SERVER."
  (jsonrpc-request server :$/isUserAccountConnected nil))

(defun eglot-grammarly-login-ok ()
  "Whether user account is connected."
  (interactive)
  (if (eglot-grammarly--login-ok (eglot-current-server))
      (message "Login is okay.")
    (message "Not login.")
    ))

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
