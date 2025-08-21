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

;;; Code:

(setq gc-cons-threshold 50000000)

;; set location of custom file, so emacs can put the gunk in there and keep this clean
(setq custom-file (locate-user-emacs-file ".custom-init.el"))
(load custom-file 'noerror 'nomessage)

(setq gnutls-min-prime-bits 4096)

;;; package repos --------------------------------------------------------------------------------------
(require 'package)
(setq package-archives '(("melpa"     . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("elpa" . "http://elpa.gnu.org/packages/")))

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
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;; Emacs ----------------------------------------------------------------------------------------------
(use-package emacs
  :config
  ;; main window settings
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  
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
	use-short-answers t ;; assume Emacs >=28
        column-number-mode t
        create-lockfiles nil
        make-backup-files nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	ad-redefinition-action 'accept
	scroll-step 3
        global-visual-line-mode t
        global-font-lock-mode 1
	compilation-scroll-output t
	compilation-read-command nil
	server-socket-dir "~/.emacs.d/server"
	tramp-default-method "ssh"
	explicit-shell-file-name "/bin/bash"
	shell-file-name "/bin/bash")

  (setq-default standard-indent 2
		find-file-visit-truename t ;; account for symlinks
		sentence-end-double-space nil ;; sentences end with 1 space not two
		display-fill-column-indicator-column 120)
  
  ;; hooks
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)
  ;; (add-hook 'emacs-startup-hook #'jt/start-org-agenda-update-timer)

  ;; Buffer and window management
  (global-auto-revert-mode t)
  (set-window-margins nil 15)
  (delete-selection-mode 1)
  (setq kill-whole-line t)
  (add-to-list 'display-buffer-alist '("\\*Ibuffer\\*" . (jt/display-ibuffer-window)))

  ;; key bindings
  (global-set-key (kbd "s-<return>") 'jt/toggle-fullscreen)
  (global-set-key (kbd "C-x k") 'jt/kill-current-buffer)
  (global-set-key (kbd "s-/") 'jt/comment-region-or-line)
  (global-set-key (kbd "C-x C-x") 'compile) ;; orig. exchange-point-and-mark
  (global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)
  (global-set-key (kbd "C-x .") 'next-buffer)
  (global-set-key (kbd "C-x ,") 'previous-buffer)
  (global-set-key (kbd "M-j") 'avy-goto-char)
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

  (defun jt/toggle-fullscreen ()
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (if (not (frame-parameter nil 'fullscreen))
         'fullscreen
       nil)))
  
  (defun jt/kill-current-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))

  (defun jt/display-ibuffer-window (buffer alist)
    "Display the Ibuffer buffer in a new window and move cursor to it."
    (let ((window (display-buffer-pop-up-window buffer alist)))
      (select-window window)))
  
  (defun jt/comment-region-or-line ()
    "Comments out the current region or line."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-line 1))))

;;; savehist ------------------------------------------------------------------------------------------
(use-package savehist
  :hook (after-init . savehist-mode))

;;; server --------------------------------------------------------------------------------------------
(use-package server
  :config
  (load "server")
  (unless (server-running-p) (server-start)))

;;; avy-zap -------------------------------------------------------------------------------------------
(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-to-char-dwim)
	 ("M-Z" . avy-zap-up-to-char-dwim)))

;;; recentf -------------------------------------------------------------------------------------------
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25
	recentf-max-saved-items 25)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;;; eshell --------------------------------------------------------------------------------------------
(use-package eshell
  :config
  (setq eshell-banner-message ""
	eshell-prefer-lisp-functions t
	eshell-history-size 10000
        eshell-hist-ignoredups t
        eshell-cmpl-ignore-case t
	eshell-scroll-to-bottom-on-input t
	eshell-prefer-lisp-functions nil)

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
    (let ((eshell-buffer-name (generate-new-buffer-name "*eshell*")))
      (if (= (length (window-list)) 1)
	  (split-window-horizontally))
      (other-window 1)
      (eshell "new")
      (add-hook 'kill-buffer-hook #'jt/eshell-kill-hook)))

  (defun jt/eshell-kill-hook ()
    "Kill hook function to handle Eshell buffer kill."
    (when (and (string-prefix-p "*eshell*" (buffer-name))
               (window-parent))
      (delete-window)
      (remove-hook 'kill-buffer-hook #'jt/eshell-kill-hook)))
  
  (global-set-key (kbd "C-c t") 'jt/eshell-here)

  ;; prompt settings ------------------------------------------------------------------------------------
  ;; The majority of this has been modified from here
  ;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org#special-prompt
  (defun jt/curr-dir-git-branch-string (pwd)
    "Returns current git branch as a string, or the empty string if
     PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (when (and (not (file-remote-p pwd))
               (eshell-search-path "git")
               (locate-dominating-file pwd ".git"))
      (let* ((git-url    (shell-command-to-string "git config --get remote.origin.url"))
             (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
             (git-branch (s-trim git-output)))
	(concat " " git-branch))))

  (defun jt/pwd-replace-home (pwd)
    "Replace home in PWD with tilde (~) character."
    (interactive)
    (let* ((home (expand-file-name (getenv "HOME")))
           (home-len (length home)))
      (if (and
           (>= (length pwd) home-len)
           (equal home (substring pwd 0 home-len)))
          (concat "~" (substring pwd home-len))
	pwd)))

  (defun jt/pwd-shorten-dirs (pwd)
    "Shorten all directory names in PWD except the last two."
    (let ((p-lst (split-string pwd "/")))
      (if (> (length p-lst) 2)
          (concat
           (mapconcat (lambda (elm) (if (zerop (length elm)) ""
				      (substring elm 0 1)))
                      (butlast p-lst 2)
                      "/")
           "/"
           (mapconcat (lambda (elm) elm)
                      (last p-lst 2)
                      "/"))
	pwd)))  ;; Otherwise, we return the PWD

  (defun jt/split-directory-prompt (directory)
    (if (string-match-p ".*/.*" directory)
	(list (file-name-directory directory) (file-name-base directory))
      (list "" directory)))

  (defun jt/python-prompt ()
    "Returns a string (may be empty) based on the current Python
   Virtual Environment. Assuming I've called the M-x command:
   `pyenv-mode-set'."
    (when (fboundp #'pyenv-mode-version)
      (let ((venv (pyenv-mode-version)))
	(when venv
          (concat
           (propertize " \xe928 " 'face `(:family "nerdicons"))
           (pyenv-mode-version))))))

  (defun jt/eshell-local-prompt-function ()
    "A more useful prompt for eshell."
    (interactive)
    (let* ((pwd        (eshell/pwd))
           (directory (jt/split-directory-prompt
                       (jt/pwd-shorten-dirs
			(jt/pwd-replace-home pwd))))
           (parent (car directory))
           (name   (cadr directory))
           (branch (jt/curr-dir-git-branch-string pwd))
           (python (when (not (file-remote-p pwd)) (jt/python-prompt)))
           (for-git `(:foreground "green"))
           (for-python `(:foreground "green")))

      (concat
       (propertize parent)
       (propertize name)
       (when branch
	 (concat
          (propertize branch 'face for-git)))
       (when python
	 (concat (propertize python 'face for-python)))
       (propertize " λ")
       (propertize " "))))
  (setq eshell-prompt-regexp "^[^λ]+ λ ")
  (custom-set-faces
   '(eshell-prompt ((t (:foreground "white" :background nil)))))
  (setq-default eshell-prompt-function #'jt/eshell-local-prompt-function))

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

;;; vterm ---------------------------------------------------------------------------------------------
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "zsh"))

(use-package multi-vterm
  :ensure t
  :config
  (defun jt/multi-vterm-here ()
    "Open a new multi-vterm buffer in the current directory."
    (interactive)
    (if (= (length (window-list)) 1)
        (split-window-horizontally))
    (other-window 1)
    (multi-vterm)
    (add-hook 'kill-buffer-hook #'jt/multi-vterm-kill-hook))

  (defun jt/multi-vterm-kill-hook ()
    "Kill hook function to handle multi-vterm buffer kill."
    (when (and (string-match-p "^\\*vterminal<[0-9]+>\\*$" (buffer-name))
               (window-parent))
      (delete-window)
      (remove-hook 'kill-buffer-hook #'jt/multi-vterm-kill-hook)))

  :bind
  (("C-!" . jt/multi-vterm-here)))

(defun jt/set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL -i -c 'echo $PATH'"
					  ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(jt/set-exec-path-from-shell-PATH)

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :init (exec-path-from-shell-initialize)
;;   :config (setq exec-path-from-shell-arguments '("-i" "-c")))

;; diminish -------------------------------------------------------------------------------------------
;; diminish those minor-mode indicators
(use-package diminish :ensure t)
(diminish 'visual-line-mode)
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)

;;; theming -------------------------------------------------------------------------------------------
(defun jt/set-fonts ()
  "Set the specific fonts that Emacs should use."
  ;; set default font
  (set-face-attribute 'default nil
		      :font "Menlo-12"
		      :weight 'light)

  ;; Set monospace font
  (set-face-attribute 'fixed-pitch nil
                      :font "Menlo-12"
                      :weight 'light)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "Helvetica-12"
                      :weight 'light))
(jt/set-fonts)
;; line numbers
(setq display-line-numbers-type 'relative)

;; Enable line numbers and highlight for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1)(hl-line-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(vterm-mode-hook
		org-mode-hook
		org-agenda-mode-hook
		markdown-mode-hook
		dired-mode-hook
		help-mode-hook
		man-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)(hl-line-mode 0))))

;; set the border width
(add-to-list 'default-frame-alist '(internal-border-width . 7))

;; turn on fill-colum by default for prog-mode
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-xcode t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-time-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-lsp nil))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character
		highlight-indent-guides-responsive 'top))

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
(use-package nerd-icons :ensure t)

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

  ;; need gnu-sed for man pages
  ;; (let ((gnu-sed-path "/opt/homebrew/opt/gnu-sed/libexec/gnubin"))
  ;;   (when (file-exists-p gnu-sed-path)
  ;;     (setenv "PATH" (concat gnu-sed-path ":" (getenv "PATH")))
  ;;     (add-to-list 'exec-path gnu-sed-path)))
  
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

;; visual-replace -------------------------------------------------------------------------------------
(use-package visual-replace
  :ensure t
  :bind (("M-%" . visual-replace)))
         
;;; completion and other nice things ------------------------------------------------------------------
;; consult --------------------------------------------------------------------------------------------
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
	 ("C-r" . consult-recent-file)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; register bindings
         ;; ("M-#" . consult-register-load)
         ("C-x r SPC" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-x r j" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
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
         ("M-s" . consult-history))                 ;; orig. next-matching-history-element

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
  (setq consult-line-start-from-top t)
  (setq consult-narrow-key "<"))

;; vertigo --------------------------------------------------------------------------------------------
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

;; marginalia -----------------------------------------------------------------------------------------
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; orderless ------------------------------------------------------------------------------------------
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; cape -----------------------------------------------------------------------------------------------
(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; tempel ---------------------------------------------------------------------------------------------
(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode))

(use-package tempel-collection :ensure t)

(use-package eglot-tempel
  :ensure t
  :after eglot
  :init
  (eglot-tempel-mode t))

;; corfu ---------------------------------------------------------------------------------------------
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  :config
  (setq corfu-auto t
	corfu-quit-no-match 'separator))

;;; grep settings -------------------------------------------------------------------------------------
(setq grep-command "rg --color=always --no-heading --line-number --smart-case --follow ")
(setq grep-find-command "rg --color=always --no-heading --line-number --smart-case --follow --context=5 ")

(use-package rg :ensure t)

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

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package dired-preview
  :ensure t
  :config
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
	(concat "\\."
		"\\(gz\\|"
		"zst\\|"
		"tar\\|"
		"xz\\|"
		"rar\\|"
		"zip\\|"
		"iso\\|"
		"epub\\|"
		"ipa\\|"
		"\\)")))

(use-package async
  :ensure t
  :init (dired-async-mode)
  :config (dired-async-mode 1))

(use-package consult-dir
  :ensure t
  :after consult ;; orig. (consult vertigo)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; pdf tools ------------------------------------------------------------------------------------------
(use-package pdf-tools :ensure t)

;; which key ------------------------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :diminish 'which-key-mode)

;; rss ------------------------------------------------------------------------------------------------
(use-package elfeed
  :ensure t
  :commands (elfeed)
  :init
  (run-at-time nil (* 30 60) #'elfeed-update)
  :config
  (setq elfeed-feeds
        '(
	  ("https://hnrss.org/frontpage" hackernews)
	  ("https://developer.apple.com/news/releases/rss/releases.rss" apple)
	  ("https://goodguysgogrind.wordpress.com/feed/" ggg)
	  ("https://www.angrymetalguy.com/tag/goregrind/feed/" goregrind)
          )))

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

;;; programming ---------------------------------------------------------------------------------------

;; diffing --------------------------------------------------------------------------------------------
(use-package difftastic
  :ensure-system-package (difft)
  :ensure t)

;; git ------------------------------------------------------------------------------------------------
(use-package magit
  :ensure t
  :commands magit-status
  :config
  (magit-auto-revert-mode 1)
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  ;; (magit-git-executable (executable-find "git"))
  (magit-git-executable "/opt/homebrew/bin/git")
  :init
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  :bind)

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))

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

(use-package blamer
  :ensure t
  :bind (("s-b" . global-blamer-mode))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :italic t)))
  :config
  (setq blamer-prettify-time-p t))

(use-package consult-ls-git
  :ensure t
  :bind
  (("C-c g f" . #'consult-ls-git)
   ("C-c g F" . #'consult-ls-git-other-window)))

(use-package consult-vc-modified-files
  :ensure t
  :bind
  (("C-x v /" . consult-vc-modified-files)
   ("C-x v ." . consult-vc-log-select-files)))

;; tree-sitter ---------------------------------------------------------------------------------------
(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :ensure t
  :after treesit-auto
  :bind (("s-f" . treesit-fold-toggle))
  :config
  (global-treesit-fold-indicators-mode 1))

;; eglot ----------------------------------------------------------------------------------------------
(use-package eglot
  :ensure t
  :config
  (defun jt/custom-eglot-hover (cb)
    "Same as `eglot-hover-eldoc-function`, but throw away its short :echo cookie."
    (eglot-hover-eldoc-function (lambda (info &rest _ignore)
                                  (funcall cb info))))

  ;; Custom function for completion at point
  (defun jt/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local eldoc-documentation-functions 
                          (cl-substitute #'jt/custom-eglot-hover
                                         'eglot-hover-eldoc-function 
                                         eldoc-documentation-functions))
              (jt/eglot-capf))))

(use-package consult-eglot :ensure t)

;; debugging ------------------------------------------------------------------------------------------
(use-package realgud :ensure t)
(use-package realgud-lldb
  :ensure t
  :after realgud
  :config
  (let ((cask-dir (expand-file-name "~/.cask")))
    (unless (file-directory-p cask-dir)
      (shell-command
       (format "git clone https://github.com/cask/cask %s" cask-dir)))))

;; just -----------------------------------------------------------------------------------------------
(use-package just-mode :ensure t)
(use-package justl :ensure t)

;; docker ---------------------------------------------------------------------------------------------
(use-package docker :ensure t)

(use-package dockerfile-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  :hook (dockerfile-ts-mode . eglot-ensure)
  (dockerfile-mode . eglot-ensure)
  :config
  (setq dockerfile-build-progress 'plain))

;; aggressive indent ----------------------------------------------------------------------------------
(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode 1))

;; shell ----------------------------------------------------------------------------------------------
(use-package sh-mode
  :hook (bash-ts-mode . eglot-ensure)
  (bash-mode . eglot-ensure)
  (sh-mode . eglot-ensure))

;; python ---------------------------------------------------------------------------------------------
(use-package python
  :ensure t
  :hook
  (python-base-mode . jt/choose-python-lsp)
  :config
  (defvar jt/python-lsp-per-project (make-hash-table :test 'equal))

  (defun jt/get-project-root ()
    (if (fboundp 'project-current)
        (when-let ((project (project-current)))
          (expand-file-name (project-root project)))
      default-directory))

  (defun jt/choose-python-lsp ()
    (let* ((project-root (or (jt/get-project-root) default-directory))
           (cached (gethash project-root jt/python-lsp-per-project)))
      (unless cached
        (let* ((choice (completing-read
                        (format "Select Python LSP for project (%s): " project-root)
                        '("pyright" "basedpyright" "ruff") nil t))
               (server-cmd (pcase choice
                             ("pyright"       '("pyright-langserver" "--stdio"))
                             ("basedpyright"  '("basedpyright-langserver" "--stdio"))
                             ("ruff"          '("ruff" "server")))))
          (puthash project-root server-cmd jt/python-lsp-per-project)
          (setq cached server-cmd)))
      (setq-local eglot-server-programs
                  `((python-base-mode . ,cached)))
      (eglot-ensure)))

  (defun jt/switch-python-lsp ()
    (interactive)
    (let ((project-root (or (jt/get-project-root) default-directory)))
      (remhash project-root jt/python-lsp-per-project)
      (when (eglot-current-server)
        (eglot-shutdown (eglot-current-server)))
      (jt/choose-python-lsp))))

(use-package ruff-format
  :ensure t
  :hook (python-base-mode . ruff-format-on-save-mode))

(use-package uv-mode
  :ensure-system-package (uv)
  :ensure t
  :hook (python-base-mode . uv-mode-auto-activate-hook))

(use-package pyenv-mode
  :ensure t
  :config
  (setq pyenv-mode-mode-line-format nil)
  (pyenv-mode))

(use-package pet
  :ensure t
  :ensure-system-package (dasel sqlite3)
  :config
  (add-hook 'python-base-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (pet-eglot-setup)
              (eglot-ensure)
              (when-let ((ruff-executable (pet-executable-find "ruff")))
                (setq-local ruff-format-command ruff-executable)
                (ruff-format-on-save-mode)))))

;; c/c++ ----------------------------------------------------------------------------------------------
(use-package c-mode
  :hook (c-ts-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (c++-mode . eglot-ensure))

;; objective-c ----------------------------------------------------------------------------------------
(use-package objc-mode
  :hook (objc-mode . eglot-ensure))

;; golang ---------------------------------------------------------------------------------------------
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(use-package go-mode
  :ensure t
  :hook (go-ts-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  :mode (("\\.go\\'" . go-mode)))

(use-package go-dlv :ensure t)

;; rust -----------------------------------------------------------------------------------------------
(use-package rust-mode
  :ensure t
  :init (setq rust-mode-treesitter-derive t)
  :hook
  (rust-ts-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  :mode (("\\.rs\\'" . rust-mode))
  :config (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode)
  (rust-mode . cargo-minor-mode))

;; swift ----------------------------------------------------------------------------------------------
(use-package swift-mode :ensure t)

;; typescript -----------------------------------------------------------------------------------------
(use-package typescript-mode
  :ensure t
  :hook (typescript-ts-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  :mode ("\\.ts\\'" . typescript-mode))

(use-package add-node-modules-path
  :ensure t
  :defer t
  :custom
  (eval-after-load 'typescript-ts-mode
    '(add-hook 'typescript-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
    '(add-hook 'tsx-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescriptreact-mode
    '(add-hook 'typescriptreact-mode-hook #'add-node-modules-path))
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))

;; javascript -----------------------------------------------------------------------------------------
(use-package js2-mode
  :ensure t
  :hook (javascript-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  :mode ("\\.js\\'" . js2-mode))

;; json -----------------------------------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :hook (json-ts-mode . eglot-ensure)
  (json-mode . eglot-ensure)
  :mode (("\\.json\\'" . json-mode)))

;; yaml -----------------------------------------------------------------------------------------------
(use-package yaml-mode
  :ensure t
  :hook (yaml-ts-mode . eglot-ensure)
  (yaml-mode . eglot-ensure)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-hook 'yaml-mode-hook
            #'(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; markdown ------------------------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-hide-urls nil)
  :hook (markdown-mode . eglot-ensure)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(defun jt/markdown-to-pdf ()
  "Export current markdown file to PDF using Pandoc."
  (interactive)
  (let ((output (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (shell-command (format "pandoc %s -o %s"
                           (shell-quote-argument (buffer-file-name))
                           (shell-quote-argument output)))
    (message "Exported to %s" output)))

;; Disable smartparens auto-pairing for backticks in markdown-mode
(with-eval-after-load 'smartparens
  (sp-local-pair 'markdown-mode "`" nil :actions nil))

;;; org-mode -----------------------------------------------------------------------------------------
(defun jt/org-mode-fonts ()
  "Set the fonts for `org-mode' to feel like a word processor."
  (set-face-attribute 'org-document-title nil :font "Helvetica" :weight 'bold :height 1.3)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Helvetica" :weight 'medium :height (cdr face)))

  ;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'org-code))

(defun jt/org-mode-setup ()
  "Initialise custom `org-mode' setup."
  (jt/org-mode-fonts)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

(use-package org
  :hook (org-mode . jt/org-mode-setup)
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  (:map org-mode-map
	(("C-c s" . org-schedule)
	 ("C-c d" . org-deadline)))
  :config
  (org-indent-mode 1)

  (setq org-agenda-prefix-format
	'((agenda . " %i %?-12t% s")
	  (todo . " %i %-12:c")
	  (tags . " %i %-12:c%t")
	  (search . " %i %-12:c")))

  ;; general org-mode config
  (setq org-startup-folded t
	org-startup-with-inline-images t
	org-export-with-section-numbers nil
	org-export-with-toc nil
	org-hide-emphasis-markers t ;; Just the Italics
	org-hide-leading-stars t
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-capture-bookmark nil ;; capture notes are not bookmarks
	org-html-postamble nil
	org-babel-python-command "python3"
	org-agenda-remove-tags t
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-deadline-if-done t
	org-auto-align-tags nil
	org-tags-column 0
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t
	org-pretty-entities t
	org-ellipsis "…"
	org-log-done 'time
	org-agenda-files '("~/org/agenda.org")
	;; agenda styling
	org-agenda-tags-column 0
	org-agenda-block-separator ?─
	org-agenda-time-grid
	'((daily today require-timed)
	  (800 1000 1200 1400 1600 1800 2000)
	  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	org-agenda-current-time-string
	"◀── now ┄┄┄┄┄┄┄"))

;; (defun load-org-babel-language (lang)
;;   "Load org-babel language module with case sensitivity for LANG."
;;   (let* ((lang-name (symbol-name lang))
;;          (capitalized-lang (if (member lang-name '("c")) "C" lang-name))
;;          (symbol-lang (intern capitalized-lang)))
;;     (unless (cdr (assoc symbol-lang org-babel-load-languages))
;;       (add-to-list 'org-babel-load-languages (cons symbol-lang t))
;;       (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))))

;; (advice-add 'org-babel-execute-src-block :before
;;             (lambda (&rest _)
;;               (load-org-babel-language (intern (org-element-property :language (org-element-at-point))))))

(use-package org-alert :ensure t)

(use-package ob
  :demand t
  :bind (:map org-mode-map
              ("C-c C-c" . org-ctrl-c-ctrl-c))
  :custom
  (org-export-babel-evaluate t)
  (org-confirm-babel-evaluate t)
  :config
  ;; Keep at the top and let other packages append to `org-babel-load-languages'.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (sh . nil)
     (python . t)
     (js . t)
     (C . t))))

(use-package org-apple-notes
  :if (file-directory-p "~/.emacs.d/org-apple-notes/")
  :ensure nil
  :after org
  :load-path "~/.emacs.d/org-apple-notes/")

(use-package ob-idapython
  :if (file-directory-p "~/.emacs.d/ob-idapython/")
  :ensure nil
  :load-path "~/.emacs.d/ob-idapython/"
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(idapython . t))
  (add-to-list 'org-src-lang-modes '("idapython" . python))
  (require 'ob-idapython))

(use-package org-modern
  :ensure t
  :config (with-eval-after-load 'org (global-org-modern-mode))
  (setq org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("⏵" . "⏷") ("▹" . "▿"))))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package org-transclusion
  :ensure t
  :after org
  :bind (:map org-mode-map ("<f12>" . org-transclusion-add)))

(use-package org-tempo
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell :results output"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("js" . "src js :results output"))
  (add-to-list 'org-structure-template-alist '("go" . "src go :results output"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("cc" . "src C :export results :flags \"\"")))

(use-package htmlize :ensure t)

(use-package ob-async :ensure t)

(use-package ob-go
  :ensure t
  :config
  (append org-babel-load-languages
          '((go . t))))

(use-package ob-rust
  :ensure t
  :config
  (append org-babel-load-languages
          '((rust . t))))


(use-package ox-gfm :ensure t)

(use-package org-download
  :ensure t
  :bind (:map org-mode-map ("C-c o d" . org-download-clipboard))
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-web-tools :ensure t)

;; note taking with denote ----------------------------------------------------------------------------
(use-package denote
  :ensure t
  :custom (denote-directory "~/org/")
  :config
  (denote-rename-buffer-mode t))
;; (require 'denote-org-extras))

(use-package consult-notes
  :ensure t
  :after consult
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind ("C-c n" . jt/consult-notes-search-in-all-notes)
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  (setq consult-notes-denote-files-function (lambda () (denote-directory-files nil t t)))

  (defun jt/consult-notes-search-in-all-notes ()
    (interactive)
    (if current-prefix-arg
	(consult-notes-search-in-all-notes)
      (consult-notes))))

;;; Emacs client settings -----------------------------------------------------------------------------
(defun jt/emacs-client-settings (frame)
  "Settings that need to be set in the Emacs client this takes the FRAME argument."
  (with-selected-frame frame
    (jt/set-fonts)
    (load-theme 'doom-xcode t)
    (set-window-margins nil 15)
    (setq column-number-mode t)
    (if (display-graphic-p)
        (progn
          (tool-bar-mode -1)
          (scroll-bar-mode -1)))
    (global-visual-line-mode t)))

(add-hook 'after-make-frame-functions #'jt/emacs-client-settings)

;;; init.el ends here
