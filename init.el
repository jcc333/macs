(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-milkbox" . "http://melpa.milkbox.net/packages/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package neotree :ensure t)
(use-package transpose-frame :ensure t)
(use-package smart-tab :ensure t)
(use-package evil :ensure t)
(use-package projectile :ensure t
             :init
             (projectile-mode))
(use-package fiplr
  :ensure t
  :config
  (setq fiplr-root-markers '(".git"))
  (setq fiplr-ignored-globs '((directories (".git"))
			      (files ("*.jpg" "*.png" "*.gz" "*.zip" "*~"))))
  :bind
   ((("C-c f" . fiplr-find-file))
   ("C-c C-f" . fiplr-find-file)))
(use-package flx :ensure t)
(use-package flx-ido :ensure t)

(use-package ponylang-mode
  :ensure t
  :config
  (progn
    (add-hook
     'ponylang-mode-hook
     (lambda ()
       (set-variable 'indent-tabs-mode nil)
       (set-variable 'tab-width 2)))))

(setq mac-command-modifier 'meta)

;; Font nonsense
(load "pretty-fonts")
(use-package pretty-mode
  :ensure t
  :init
  (global-pretty-mode t)
  (pretty-activate-groups
   '(:equality
     :ordering
     :ordering-double
     :ordering-triple
     :arrows
     :arrows-twoheaded
     :punctuation
     :logic
     :sets
     :sub-and-superscripts
     :greek
     :arithmetic-nary)))

(global-pretty-mode 1)
(global-prettify-symbols-mode 1)
(global-set-key (kbd "C-c C-p C-m") (lambda ()
				      (global-pretty-mode)
				      (global-prettify-symbols-mode)))

(pretty-fonts-set-kwds
 '((pretty-fonts-fira-font
    prog-mode-hook org-mode-hook)))

;; Colors
(load-theme 'nord t)
(setq nord-comment-brightness 15)
(setq highlight-indent-guides-method 'column)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(setq tab-always-indent 'complete)

;; ruby-crutches
(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Guardfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"
  :init
  (setq ruby-indent-level 2
	ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)
  :bind
  (([(meta down)] . ruby-forward-sexp)
   ([(meta up)]   . ruby-backward-sexp)
   (("C-c C-e"    . ruby-send-region))))

(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

(use-package yari
  :ensure t
  :init
  (add-hook 'ruby-mode-hook
	    (lambda ()
	      (local-set-key [f1] 'yari))))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  :bind
  ((("C-c C-r C-b" . inf-ruby))))

(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)

(use-package ruby-tools
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  :diminish ruby-tools-mode)

(add-hook
 'ruby-mode
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
           ("def" .        #x2131)
           ("not" .        #x2757)
           ("in" .         #x2208)
           ("not in" .     #x2209)
           ("return" .     #x27fc)
           ("yield" .      #x27fb)
           ("for" .        #x2200)
           ("int" .        #x2124)
           ("float" .      #x211d)
           ("str" .        #x1d54a)
           ("true" .       #x1d54b)
           ("false" .      #x1d53d)
           ("Hash" .       #x1d507)
           ("Array" .      #x2112)
           ("Set" .        #x2126)
           ("Enumerable" . #x1d50a)
           ("Any" .        #x2754)
           ("Union" .      #x22c3)))))

(setq-default indent-tabs-mode nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2)

;; modes
(electric-indent-mode 0)
(setq projectile-indexing-method 'native)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(when (not package-archive-contents)
(package-refresh-contents))

(global-set-key (kbd "M-1") 'neotree-toggle)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-x C-n") 'goto-line)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x g") 'beginning-of-buffer)

(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

(defalias 'qrr 'query-replace-regexp)

(setq-default indent-tabs-mode nil)

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))

(define-key read-expression-map [(shift tab)] 'hippie-unexpand)

;; Genuwine's "Pony"

(global-set-key (kbd "C-<U>") 'undo)
(global-set-key (kbd "C-<return>") 'newline)
(global-undo-tree-mode -1)

(eval-after-load
  'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

;; ghc-mod specific things
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;; Line numbers
(setq linum-format "%4d \u2502 ")
(global-linum-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'untabify)
(setq tab-stop-list '(number-sequence 2 120 2))

;; Delete region
(delete-selection-mode 1)

(global-linum-mode t)

(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-smart-tab-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq default-tab-width 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(package-selected-packages
   (quote
    (pretty-mode racket-mode ponylang-mode fiplr projectile-codesearch pony-mode projectile haskell-emacs smart-tabs-mode use-package transpose-frame tabkey2 spotify smart-tab slime seq scala-outline-popup scala-mode rainbow-identifiers rainbow-delimiters pkg-info neotree let-alist ghc evil-visual-mark-mode evil-tutor evil-textobj-anyblock evil-space evil-snipe evil-smartparens evil-search-highlight-persist evil-rsi evil-quickscope evil-paredit evil-org evil-numbers evil-nerd-commenter evil-matchit evil-mark-replace evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-escape evil-commentary evil-args evil-anzu auto-complete))))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
