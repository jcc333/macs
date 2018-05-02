;; package manager
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp/")

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

;; EVIL
(use-package evil :ensure t)

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

;; IDO
(use-package flx :ensure t)
(use-package flx-ido :ensure t)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(setq tab-always-indent 'complete)

;; Tabs are evil
(setq-default indent-tabs-mode nil)

;; Colors
(load-theme 'nord t)
(setq nord-comment-brightness 15)
(setq highlight-indent-guides-method 'column)

;; Fuzzy finding
(use-package fiplr
  :ensure t
  :config
  (setq fiplr-root-markers '(".git"))
  (setq fiplr-ignored-globs '((directories (".git"))
			      (files ("*.jpg" "*.png" "*.gz" "*.zip" "*~"))))
  :bind
   ((("C-c f" . fiplr-find-file))
   ("C-c C-f" . fiplr-find-file)))

;; Line numbers
(setq linum-format "%4d \u2502 ")
(global-linum-mode 1)
;; Delete region
(delete-selection-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'untabify)
(setq tab-stop-list '(number-sequence 2 120 2))

(use-package projectile
  :ensure t
  :init
  (projectile-mode))

(setq projectile-indexing-method 'native)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(defun delete-until-word ()
  "Delete all whitespace up until the next non-whitespace character"
  (interactive)
  (save-excursion
    (let* ((start-point (point))
           (number-to-skip (skip-chars-forward " \t"))
           (end-point (+ start-point number-to-skip)))
      (delete-region start-point end-point))))

(global-unset-key (kbd "C-d"))
(global-set-key (kbd "C-d") 'delete-until-word)

(global-set-key (kbd "C-<return>") 'newline)
(global-undo-tree-mode -1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" default)))
 '(package-selected-packages
   (quote
    (pretty-mode js-auto-format-mode electric-spacing electric-operator electric-case yaml-mode projectile-rails projectile ggtags color-theme-x color-theme-approximate color-theme-modern color-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow solarized-theme magit nord yari use-package smartparens rvm ruby-tools rubocop nord-theme inf-ruby evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
