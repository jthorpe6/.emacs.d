;;; init.el --- summary -*- lexical-binding: t; outline-regexp: ";.*---"; -*-

;; Author: Joe Thorpe
;; Maintainer: Joe Thorpe
;; Version: 1
;; Package-Requires: (emacs)
;; Homepage: https://github.com/jthorpe6/.emacs.d
;; Keywords: emacs


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; This file is my Emacs configuration.
;; I have three zsh aliases to run Emacs and the Emacs client, they are:
;;    alias ec='emacsclient -nw -c -s ~/.emacs.d/server/server' # Primarily used in the terminal
;;    alias ecc='emacsclient -n -c -s ~/.emacs.d/server/server' # Primarily used to call the gui
;;    alias emacs='emacs --daemon' # Used to start the Emacs daemon

;;; Code:

(setq gc-cons-threshold 50000000)

;; set location of custom file, so emacs can put the gunk in there and keep this clean
(setq custom-file (locate-user-emacs-file ".custom-init.el"))
(load custom-file 'noerror 'nomessage)

(setq gnutls-min-prime-bits 4096)

;;; packages ------------------------------------------------------------------------------------------
(require 'package)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")))
;;(package-initialize)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; use-package ---------------------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; (setq use-package-always-ensure t)

;;; keep packages up to date --------------------------------------------------------------------------
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;; Emacs ----------------------------------------------------------------------------------------------
(use-package emacs
  :config
  ;; frame settings
  (toggle-frame-maximized)
  (defun toggle-fullscreen ()
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (if (not (frame-parameter nil 'fullscreen))
         'fullscreen
       nil)))
  (global-set-key (kbd "s-<return>") 'toggle-fullscreen)

  ;; character encoding
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; misc settings
  (setq ring-bell-function 'ignore
        menu-bar-mode -1
        initial-scratch-message ""
        make-pointer-invisible t
        inhibit-startup-message t
        standard-indent 2
        column-number-mode t
        create-lockfiles nil
        make-backup-files nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	ad-redefinition-action 'accept
        global-visual-line-mode t
        global-font-lock-mode 1
        global-auto-revert-mode t
	compilation-read-command nil
	server-socket-dir "~/.emacs.d/server"
	tramp-default-method "ssh"
	explicit-shell-file-name "/bin/bash"
	shell-file-name "/bin/bash")

  ;; hooks
  (add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
  
  ;; account for symlinks
  (setq-default find-file-visit-truename t)
  
  ;; Buffer and window management
  (set-window-margins nil 15)
  (delete-selection-mode 1)
  (setq kill-whole-line t)
  (add-to-list 'display-buffer-alist '("\\*Ibuffer\\*" . (jt/display-ibuffer-window)))

  ;; key bindings
  (global-set-key (kbd "C-x k") 'jt/kill-current-buffer)
  (global-set-key (kbd "C-x 2") 'jt/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'jt/split-and-follow-vertically)
  (global-set-key (kbd "s-/") 'jt/comment-region-or-line)
  (global-set-key (kbd "C-x C-r") 'compile)
  (global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)
  (global-set-key (kbd "C-x .") 'next-buffer)
  (global-set-key (kbd "C-x ,") 'previous-buffer)
  (global-unset-key (kbd "C-r")) ;; unset I-search
  
  ;; custom functions
  (defun jt/markdown-convert-buffer-to-org ()
    "Convert the current buffer's content from markdown to org-mode format using pandoc"
    (interactive)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f markdown -t org -o %s"
                                     (concat (file-name-sans-extension (buffer-file-name)) ".org"))))
  
   (defadvice jt/mouse-save-then-kill (around mouse2-copy-region activate)
     (when (region-active-p)
       (copy-region-as-kill (region-beginning) (region-end)))
     ad-do-it)
  
  (defun jt/kill-current-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))
  
  (defun jt/split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))

  (defun jt/split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))

  (defun jt/display-ibuffer-window (buffer alist)
    "Display the Ibuffer buffer in a new window and move cursor to it."
    (let ((window (display-buffer-pop-up-window buffer alist)))
      (select-window window)))
  
  (defun jt/comment-region-or-line ()
    "Comments out the current region or line."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-line 1)))

  (defun jt/xnu-dir ()
    "Open xnu in Dired mode."
    (interactive)
    (let ((directory "~/Documents/opt/xnu"))
      (if (file-exists-p directory)
          (dired directory)
        (message "Directory does not exist: %s" directory)))))

;;; eshell --------------------------------------------------------------------------------------------
(use-package eshell
  :config
  (setq eshell-banner-message ""
	eshell-prefer-lisp-functions t
	eshell-history-size 10000
        eshell-hist-ignoredups t
        eshell-cmpl-ignore-case t)

  (defun jt/eshell-clear ()
    "Clear the Eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  
  (defun jt/eshell-clear-screen ()
    "Clear the screen in Eshell."
    (interactive)
    (eshell-send-input)
    (jt/eshell-clear))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") #'jt/eshell-clear-screen)))

  (defun jt/eshell-here ()
    "Open a new Eshell buffer in the current directory."
    (interactive)
    (eshell "new"))

  (global-set-key (kbd "C-c t") 'jt/eshell-here))

(use-package em-tramp)

(use-package eshell-syntax-highlighting
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode t))

(use-package eshell-vterm
  :ensure t
  :after (eshell vterm)
  :config
  (eshell-vterm-mode))

(use-package load-bash-alias
  :ensure t
  :config
  (setq load-bash-alias-additional-aliases-files '("~/.aliases")))

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-run-command nil)
  :bind
  ("C-`" . eshell-toggle))

;;; vterm ---------------------------------------------------------------------------------------------
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "zsh")
  :init
  (add-hook 'vterm-exit-functions
     (lambda (_ _)
       (let* ((buffer (current-buffer))
              (window (get-buffer-window buffer)))
         (when (not (one-window-p))
           (delete-window window))
         (kill-buffer buffer))))
  :bind ("C-!" . 'vterm-other-window))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;; diminish -------------------------------------------------------------------------------------------
;; diminish those minor-mode indicators
(use-package diminish
  :ensure t)

(diminish 'visual-line-mode)
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)

;;; theming -------------------------------------------------------------------------------------------
(defun jt/set-fonts ()
  "Set the specific fonts that Emacs should use."
  ;; set default font
  (set-face-attribute 'default nil
		      :font "JetBrains Mono-14"
		      :weight 'light)

  ;; Set monospace font
  (set-face-attribute 'fixed-pitch nil
                      :font "JetBrains Mono-14"
                      :weight 'light)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka Curly-14"
                      :weight 'light))

;; set the border width
(add-to-list 'default-frame-alist '(internal-border-width . 7))

;; change the cursor to a bar
(setq-default cursor-type 'bar)

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 't '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                               "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                               "--" "---" "-->" "->" "->>" "-<" "-<<" "-~" "#{" "#["
                               "##" "###" "####" "#(" "#?" "#_" "#_(" ".-" ".=" ".."
                               "..<" "..." "?=" "??" ";;" "/*" "/**" "/=" "/=="
                               "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                               "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>"
                               "<=" "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>"
                               "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->"
                               "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<"
                               "<<-" "<<=" "<<<" "<~" "<~~" "</" "</>" "~@" "~-" "~>"
                               "~~" "~~>" "%%"))
  (global-ligature-mode 't))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("INFO"   . "#3498DB")
        ("HACK"   . "#FFFFFF")))

;; (use-package nord-theme
;;   :ensure t
;;   :config (load-theme 'nord t))

(use-package catppuccin-theme
  :ensure t
  :config (load-theme 'catppuccin t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-time-icon nil)
  (setq doom-modeline-buffer-encoding nil))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character
		highlight-indent-guides-responsive 'top))

(use-package color-identifiers-mode
  :ensure t
  :config (add-hook 'after-init-hook 'global-color-identifiers-mode))

(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-s-n" . mc/mark-next-like-this)
	 ("C-s-p" . mc/mark-previous-like-this)))

;; nerd-icons everywhere ------------------------------------------------------------------------------
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure t
  :after dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; macOS custom settings -----------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#")))

  (setq dired-use-ls-dired nil)
  
  (use-package ns-auto-titlebar
    :ensure t
    :config
    (ns-auto-titlebar-mode))
  
  (use-package dwim-shell-command
    :ensure t
    :bind (([remap shell-command] . dwim-shell-command)
           :map dired-mode-map
           ([remap dired-do-async-shell-command] . dwim-shell-command)
           ([remap dired-do-shell-command] . dwim-shell-command)
           ([remap dired-smart-shell-command] . dwim-shell-command)))

  (use-package dwim-shell-commands))

;;; packages ------------------------------------------------------------------------------------------

;; spelling -------------------------------------------------------------------------------------------
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :diminish jinx-mode
  :bind (("C-;" . jinx-correct)
         ("C-M-;" . jinx-languages)))

;;; completion and other nice things ------------------------------------------------------------------
;; package: consult settings
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
	 ("C-s" . consult-line) ;; need to make C-s feel nice
	 ;; ("C-r" . consult-recent-file)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("s-l" . consult-goto-line)
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
	 ("M-." . consult-xref))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (setq consult-focus-lines t)

  (setq consult-narrow-key "<"))

;; package: vertigo settings
(use-package vertico
  :ensure t
  :demand
  :config
  (setq vertico-cycle t)
  (setq vertico-preselect 'directory)
  :init
  (vertico-mode)
  (defun jt/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  :bind (:map vertico-map
              ("/" . #'jt/vertico-insert)))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :demand
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; package: marginalia settings
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  (marginalia-mode))

;; package: orderless settings
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; package: cape settings
(use-package cape
  :ensure t
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; package: corfu settings
;; important here is the `corfu-face' variable to make it fit with the theme
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 3)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  :config
  (setq corfu-auto t
      corfu-quit-no-match 'separator)
  (custom-set-faces
   '(corfu-default ((t (:background "#1e1e2e"))))))

;;; grep settings -------------------------------------------------------------------------------------
(setq grep-command "rg --color=always --no-heading --line-number --smart-case --follow ")
(setq grep-find-command "rg --color=always --no-heading --line-number --smart-case --follow --context=5 ")
(if (require 'consult nil 'noerror)
    (global-set-key (kbd "C-c r") 'consult-ripgrep)
  (global-set-key (kbd "C-c r") 'grep-find))

(use-package rg
  :ensure t)

;;; dired ---------------------------------------------------------------------------------------------
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-ahl")
  (setq dired-auto-revert-buffer t)
  (setq dired-do-revert-buffer t))

(use-package dired-subtree
  :ensure t
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package async
  :ensure t
  :init (dired-async-mode)
  :config (dired-async-mode 1))

(use-package consult-dir
  :ensure t
  :after (consult vertigo)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; rss ------------------------------------------------------------------------------------------------
(use-package elfeed
  :ensure t
  :init
  (setq elfeed-feeds
        '(("https://news.ycombinator.com/rss" hacker-news))))

(use-package elfeed-goodies
  :ensure t)

;; chatgpt --------------------------------------------------------------------------------------------
;; M-x set-variable chatgpt-shell-openai-key
(use-package chatgpt-shell
  :ensure t
  :custom
  (chatgpt-shell-api-url-base "https://api.openai.com"))

;; which key ------------------------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :diminish 'which-key-mode)

;;; programming ---------------------------------------------------------------------------------------

;; yasnippet ------------------------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; flycheck ------------------------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t)
  :diminish flycheck-mode)

;; git -----------------------------------------------------------------------------------------------
(use-package magit
  :ensure t
  :commands magit-status
  :config
    (magit-auto-revert-mode 1)
  :init
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  :bind)

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh)))

;; lsp ------------------------------------------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((
         objc-mode
         cc-mode
         c-mode
         c++-mode
         json-mode
         dockerfile-mode
         sql-mode
         yamal-mode
         sh-mode
         python-mode
         go-mode
         swift-mode
         js-mode
         web-mode
         js-jsx-mode
         typescript-mode
         ) . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t
	lsp-log-io nil
	lsp-restart 'auto-restart
	lsp-keep-workspace-alive nil
	lsp-diagnostics-provider :flycheck
	lsp-eldoc-hook nil
	lsp-modeline-diagnostics-enable nil
	lsp-headerline-breadcrumb-enable nil
	lsp-semantic-tokens-enable nil
	lsp-enable-folding nil
	lsp-enable-imenu nil
	lsp-enable-snippet nil
	read-process-output-max (* 1024 1024) ;; 1MB
	lsp-idle-delay 0.5
	lsp-enable-links nil
	lsp-lens-enable nil
	lsp-completion-provider :none) ;; we use Corfu!
  :custom
  (lsp-enable-snippet t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
	lsp-ui-doc-header t
	lsp-ui-doc-include-signature t
	lsp-ui-doc-border (face-foreground 'default)
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-delay 0.05))

;; debugging ------------------------------------------------------------------------------------------
(use-package dap-mode
  :ensure t
  :config (setq dap-auto-configure-features '(sessions locals controls tooltip)))

;; docker ---------------------------------------------------------------------------------------------
(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  :config
  (setq dockerfile-build-progress 'plain))

;; c --------------------------------------------------------------------------------------------------
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp)))
  :config
  ;; (setq ccls-sem-highlight-method nil)
  (setq ccls-enable-skipped-ranges nil))

;; python ---------------------------------------------------------------------------------------------
(defun jt/run-python-doctest ()
  "Run \='python -m doctest -v\=' on the current buffer's file."
  (interactive)
  (let ((output-buffer (generate-new-buffer "*Python Doctest Output*"))
        (file (buffer-file-name)))
    (when (and file (file-exists-p file))
      (call-process "python3" nil output-buffer nil "-m" "doctest" "-v" file))
    (with-current-buffer output-buffer
      (pop-to-buffer output-buffer))))

(use-package lsp-pyright
  :ensure t
  :after (python lsp-mode)
  :hook
  (python-mode-hook . (lambda ()
            (require 'lsp-pyright) (lsp-deferred))))

(use-package python
  :ensure t
  :hook((python-mode-hook . lsp)))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells))

;; for modeline
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
;; (setq mode-line-format (cons '(:exec venv-current-name) mode-line-format))

(use-package pyenv-mode
  :ensure t
  :config
  (setq pyenv-mode-mode-line-format nil)
  (pyenv-mode))

;; golang ---------------------------------------------------------------------------------------------
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode)))

(use-package dap-dlv-go
  :after dap-mode)

(use-package go-dlv
  :ensure t)

;; swift ----------------------------------------------------------------------------------------------
(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :ensure t
  :hook (swift-mode . (lambda () (lsp))))

;; typescript -----------------------------------------------------------------------------------------
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode))

;; javascript -----------------------------------------------------------------------------------------
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode))

;; general web ----------------------------------------------------------------------------------------
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"
         "\\.css\\'")
  :config
  (progn
    (setq web-mode-code-indent-offset 2
	  web-mode-enable-auto-quoting nil)))

(use-package web-beautify
    :ensure t
    :bind (:map web-mode-map
           ("C-c b" . web-beautify-html)
           :map js2-mode-map
           ("C-c b" . web-beautify-js)))

;; json -----------------------------------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

;; yaml -----------------------------------------------------------------------------------------------
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-hook 'yaml-mode-hook
            #'(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; markdown ------------------------------------------------------------------------------------------
(use-package markdown-mode
  :mode "\\.md\\'"
  :ensure t)

;;; org-mode -----------------------------------------------------------------------------------------
(defun jt/org-mode-fonts ()
  "Set the fonts for `org-mode' to feel like a word processor."
  (set-face-attribute 'org-document-title nil :font "Iosevka Curly" :weight 'bold :height 1.3)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Curly" :weight 'medium :height (cdr face)))

  ;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'org-code))

(defvar jt/org-agenda-update-timer nil
  "Timer for updating org-agenda-files.")

(defun jt/update-org-agenda-files ()
  "Update org-agenda-files when a new org file is added to ~/org/journal."
  (setq org-agenda-files
        (append '("~/org/agenda.org")
                (directory-files-recursively "~/org/journal" "\\.org$"))))

(defun jt/start-org-agenda-update-timer ()
  "Start a timer to periodically update org-agenda-files."
  (setq jt/org-agenda-update-timer
        (run-with-timer 0 300 'jt/update-org-agenda-files)))

(defun jt/org-mode-setup ()
  "Initialise custom `org-mode' setup."
  (jt/org-mode-fonts)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

(use-package org
  :pin gnu
  :hook (org-mode . jt/org-mode-setup)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (org-indent-mode 1)
  
  (setq org-startup-folded t
	org-startup-with-inline-images t
	org-export-with-section-numbers nil
	org-hide-emphasis-markers t ;; Just the Italics
	org-hide-leading-stars t
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-html-postamble nil
	org-babel-python-command "python3"
	org-confirm-babel-evaluate nil
	;; agenda files
	org-agenda-files (append '("~/org/agenda.org")
                                 (directory-files-recursively "~/org/journal" "\\.org$"))))

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
	(add-to-list 'org-babel-load-languages (cons (intern language) t))
	(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))

;; git submodule org apple notes
(use-package org-apple-notes
  :ensure nil
  :load-path "~/.emacs.d/org-apple-notes/")

(use-package org-modern
  :ensure t
  :config (with-eval-after-load 'org (global-org-modern-mode)))

(use-package org-tempo
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell :results output"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("js" . "src js :results output"))
  (add-to-list 'org-structure-template-alist '("go" . "src go :results output")))

(use-package htmlize
  :ensure t)

(use-package ob-go
  :ensure t)

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

(use-package org-download
  :ensure t
  :bind ("C-c o d" . org-download-clipboard)
  :custom
  (org-download-image-org-width 50)
  (org-download-screenshot-method "/usr/bin/import %s")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  :config
  (defun org-download--dir-2 ()
    "Return the 'dir-2' of the current org file.
    This is the directory in which the file is located."
    (file-name-directory (directory-file-name (buffer-file-name (buffer-base-buffer)))))
  (setq-default org-download-image-dir #'org-download--dir-2)
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-web-tools
  :ensure t)

;; note taking with denote ----------------------------------------------------------------------------
(use-package denote
  :ensure t
  :custom (denote-directory "~/org/")
  :config
  (denote-rename-buffer-mode)
  (require 'denote-org-dblock))

(use-package consult-notes
  :ensure t
  :after consult
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind ("C-c n" . consult-notes)
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files)))
  
(defun jt/denote-journal ()
  "Create an entry tagged `journal' with the date as its title."
  (interactive)
  (denote
   (format-time-string "%A %e %B %Y")
   '("journal")
   nil
   "~/org/journal/"))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
	       '("n" "New note" plain
		 (file denote-last-path)
		 #'denote-org-capture
		 :no-save t
		 :immediate-finish nil
		 :kill-buffer t
		 :jump-to-captured t)))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("j" "Journal entry" plain (nil)
		 #'jt/denote-journal
		 :immediate-finish t
		 :kill-buffer t
		 :jump-to-captured t)))

;;; Emacs client settings -----------------------------------------------------------------------------
(defun jt/emacs-client-settings (frame)
  "Settings that need to be set in the Emacs client this takes the FRAME argument."
  (with-selected-frame frame
    (jt/set-fonts)
    (load-theme 'catppuccin t)
    (set-window-margins nil 15)
    (setq column-number-mode t)
    (menu-bar-mode -1)
    (if (display-graphic-p)
        (progn
        (tool-bar-mode -1)
        (scroll-bar-mode -1)))
    (global-visual-line-mode t)
    (jt/start-org-agenda-update-timer)))

(add-hook 'after-make-frame-functions #'jt/emacs-client-settings)

;; backup notes but only on some systems --------------------------------------------------------------
(defun jt/load-backup-notes ()
  "Load the backup-notes.el file if it exists."
  (let ((backup-notes-file "~/.emacs.d/backup-notes.el"))
    (when (file-exists-p backup-notes-file)
      (load backup-notes-file))))

;;; init.el ends here
