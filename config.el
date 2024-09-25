;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(use-package! hl-todo
  ;; if you omit :defer, :hook, :commands, or :after, then the package is loaded
  ;; immediately. By using :hook here, the `hl-todo` package won't be loaded
  ;; until prog-mode-hook is triggered (by activating a major mode derived from
  ;; it, e.g. python-mode)
  :hook (prog-mode . hl-todo-mode)
  :init
  ;; code here will run immediately
  :config
  ;; code here will run after the package is loaded
  (setq hl-todo-highlight-punctuation ":"))

(use-package! python-black)

(use-package! flycheck)

(use-package! realgud)

(use-package! lsp-mode
  :commands lsp
  :config
  (setq lsp-signature-auto-activate t
        lsp-signature-render-documentation t))  ;; Show parameter info and documentation

;; (use-package codeium
;;     :init
;;     ;; use globally
;;     (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

;;     :config
;;     (setq use-dialog-box nil) ;; do not use popup boxes

;;     ;; if you don't want to use customize to save the api-key
;;     ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;     ;; get codeium status in the modeline
;;     (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;     (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;     ;; alternatively for a more extensive mode-line
;;     ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;     ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;     (setq codeium-api-enabled
;;         (lambda (api)
;;             (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

;;     (defun my-codeium/document/text ()
;;         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))

;;     ;; warning: this is measured by UTF-8 encoded bytes
;;     (defun my-codeium/document/cursor_offset ()
;;         (codeium-utf8-byte-length
;;             (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;     (setq codeium/document/text 'my-codeium/document/text)
;;     (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; (use-package! corfu
;;   :custom
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-cycle t)                ;; Allows cycling through candidates
;;   (corfu-quit-no-match 'separator) ;; Don't quit when no match is found
;;   :config
;;   (setq-default
;;    company-frontends '(company-preview-frontend))
;;   :bind
;;   (:map corfu-map
;;         ("TAB" . corfu-next)
;;         ([tab] . corfu-next)
;;         ("S-TAB" . corfu-previous)
;;         ([backtab] . corfu-previous))
;;   :init
;;   (global-corfu-mode))

;; (use-package! treemacs
;;   :defer t
;;   :config
;;   ;; Enable treemacs' file preview
;;   (setq treemacs-follow-after-init t)
;;   (treemacs-follow-mode t))


(use-package! company
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :hook (doom-first-input . global-company-mode)
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-idle-delay 0.26
        company-global-modes
        '(not erc-mode
              circe-mode
              message-mode
              help-mode
              gud-mode
              vterm-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '(company-capf)

        ;; These auto-complete the current selection when
        ;; `company-auto-commit-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-commit nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  (when (modulep! +tng)
    (add-hook 'global-company-mode-hook #'company-tng-mode))

  :config
  (when (modulep! :editor evil)
    (add-hook 'company-mode-hook #'evil-normalize-keymaps)
    (add-hook! 'evil-normal-state-entry-hook
      (defun +company-abort-h ()
        ;; HACK `company-abort' doesn't no-op if company isn't active; causing
        ;;      unwanted side-effects, like the suppression of messages in the
        ;;      echo-area.
        ;; REVIEW Revisit this to refactor; shouldn't be necessary!
        (when company-candidates
          (company-abort))))
    ;; Allow users to switch between backends on the fly. E.g. C-x C-s followed
    ;; by C-x C-n, will switch from `company-yasnippet' to
    ;; `company-dabbrev-code'.
    (defadvice! +company--abort-previous-a (&rest _)
      :before #'company-begin-backend
      (company-abort)))

  (add-hook 'company-mode-hook #'+company-init-backends-h 'append)

  ;; NOTE Fix #1335: ensure `company-emulation-alist' is the first item of
  ;;      `emulation-mode-map-alists', thus higher priority than keymaps of
  ;;      evil-mode. We raise the priority of company-mode keymaps
  ;;      unconditionally even when completion is not activated. This should not
  ;;      cause problems, because when completion is activated, the value of
  ;;      `company-emulation-alist' is ((t . company-my-keymap)), when
  ;;      completion is not activated, the value is ((t . nil)).
  (add-hook! 'evil-local-mode-hook
    (when (memq 'company-emulation-alist emulation-mode-map-alists)
      (company-ensure-emulation-alist)))

  ;; Fix #4355: allow eldoc to trigger after completions.
  (after! eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort)))


;;
;;; Packages

(after! company-files
  ;; Fix `company-files' completion for org file:* links
  (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))


(use-package! company-box
  :when (modulep! +childframe)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-tooltip-limit 50
        company-box-icons-alist 'company-box-icons-nerd-icons
        ;; Move company-box-icons--elisp to the end, because it has a catch-all
        ;; clause that ruins icons from other backends in elisp buffers.
        company-box-icons-functions
        (cons #'+company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions))
        company-box-icons-nerd-icons
        `((Unknown        . ,(nerd-icons-codicon  "nf-cod-code"                :face  'font-lock-warning-face))
          (Text           . ,(nerd-icons-codicon  "nf-cod-text_size"           :face  'font-lock-doc-face))
          (Method         . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (Function       . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (Constructor    . ,(nerd-icons-codicon  "nf-cod-triangle_right"      :face  'font-lock-function-name-face))
          (Field          . ,(nerd-icons-codicon  "nf-cod-symbol_field"        :face  'font-lock-variable-name-face))
          (Variable       . ,(nerd-icons-codicon  "nf-cod-symbol_variable"     :face  'font-lock-variable-name-face))
          (Class          . ,(nerd-icons-codicon  "nf-cod-symbol_class"        :face  'font-lock-type-face))
          (Interface      . ,(nerd-icons-codicon  "nf-cod-symbol_interface"    :face  'font-lock-type-face))
          (Module         . ,(nerd-icons-codicon  "nf-cod-file_submodule"      :face  'font-lock-preprocessor-face))
          (Property       . ,(nerd-icons-codicon  "nf-cod-symbol_property"     :face  'font-lock-variable-name-face))
          (Unit           . ,(nerd-icons-codicon  "nf-cod-symbol_ruler"        :face  'font-lock-constant-face))
          (Value          . ,(nerd-icons-codicon  "nf-cod-symbol_field"        :face  'font-lock-builtin-face))
          (Enum           . ,(nerd-icons-codicon  "nf-cod-symbol_enum"         :face  'font-lock-builtin-face))
          (Keyword        . ,(nerd-icons-codicon  "nf-cod-symbol_keyword"      :face  'font-lock-keyword-face))
          (Snippet        . ,(nerd-icons-codicon  "nf-cod-symbol_snippet"      :face  'font-lock-string-face))
          (Color          . ,(nerd-icons-codicon  "nf-cod-symbol_color"        :face  'success))
          (File           . ,(nerd-icons-codicon  "nf-cod-symbol_file"         :face  'font-lock-string-face))
          (Reference      . ,(nerd-icons-codicon  "nf-cod-references"          :face  'font-lock-variable-name-face))
          (Folder         . ,(nerd-icons-codicon  "nf-cod-folder"              :face  'font-lock-variable-name-face))
          (EnumMember     . ,(nerd-icons-codicon  "nf-cod-symbol_enum_member"  :face  'font-lock-builtin-face))
          (Constant       . ,(nerd-icons-codicon  "nf-cod-symbol_constant"     :face  'font-lock-constant-face))
          (Struct         . ,(nerd-icons-codicon  "nf-cod-symbol_structure"    :face  'font-lock-variable-name-face))
          (Event          . ,(nerd-icons-codicon  "nf-cod-symbol_event"        :face  'font-lock-warning-face))
          (Operator       . ,(nerd-icons-codicon  "nf-cod-symbol_operator"     :face  'font-lock-comment-delimiter-face))
          (TypeParameter  . ,(nerd-icons-codicon  "nf-cod-list_unordered"      :face  'font-lock-type-face))
          (Template       . ,(nerd-icons-codicon  "nf-cod-symbol_snippet"      :face  'font-lock-string-face))
          (ElispFunction  . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (ElispVariable  . ,(nerd-icons-codicon  "nf-cod-symbol_variable"     :face  'font-lock-variable-name-face))
          (ElispFeature   . ,(nerd-icons-codicon  "nf-cod-globe"               :face  'font-lock-builtin-face))
          (ElispFace      . ,(nerd-icons-codicon  "nf-cod-symbol_color"        :face  'success))))

  ;; HACK Fix oversized scrollbar in some odd cases
  ;; REVIEW `resize-mode' is deprecated and may stop working in the future.
  ;; TODO PR me upstream?
  (setq x-gtk-resize-child-frames 'resize-mode)

  ;; Disable tab-bar in company-box child frames
  ;; TODO PR me upstream!
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))

  ;; Don't show documentation in echo area, because company-box displays its own
  ;; in a child frame.
  (delq! 'company-echo-metadata-frontend company-frontends)

  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace)))))

  ;; `company-box' performs insufficient frame-live-p checks. Any command that
  ;; "cleans up the session" will break company-box.
  ;; TODO Fix this upstream.
  (defadvice! +company-box-detect-deleted-frame-a (frame)
    :filter-return #'company-box--get-frame
    (if (frame-live-p frame) frame))
  (defadvice! +company-box-detect-deleted-doc-frame-a (_selection frame)
    :before #'company-box-doc
    (and company-box-doc-enable
         (frame-local-getq company-box-doc-frame frame)
         (not (frame-live-p (frame-local-getq company-box-doc-frame frame)))
         (frame-local-setq company-box-doc-frame nil frame))))



;; ;; Enable pyvenv for virtualenv management with Elpy
;; (use-package! pyvenv
;;   :config
;;   (pyvenv-mode 1))

;; ;; Automatically activate virtualenv if .env exists in the project root
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (let ((venv-path (expand-file-name ".env" (project-root (project-current)))))
;;               (when (file-exists-p venv-path)
;;                 (pyvenv-activate venv-path)))))

(after! lsp-mode
  (setq lsp-completion-provider :capf))  ;; Use company-capf for completion

(after! company
  (setq company-idle-delay 0.2          ;; Short delay before suggestions pop up
        company-minimum-prefix-length 1  ;; Show suggestions after 1 character
        company-tooltip-align-annotations t))  ;; Align annotations to the right


(use-package! python-black
  :demand t
  :after python
  :config
  (add-hook! 'python-mode-hook #'python-black-on-save-mode)
  (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
  (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
  (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement))
