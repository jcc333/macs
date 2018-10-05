;; package manager
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(set-frame-parameter nil 'fullscreen 'fullboth)
(setq shr-external-browser "firefox")

(use-package kotlin-mode :ensure t)

(use-package enh-ruby-mode
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
  (add-hook 'ruby-mode-hook 'superword-mode)
  :bind
  (([(meta down)] . enh-ruby-forward-sexp)
   ([(meta up)]   . enh-ruby-backward-sexp)
   (("C-c C-n"    . enh-ruby-find-error))))

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

(use-package neotree
  :ensure t
  :init
  (global-set-key (kbd "<f2>") 'neotree-toggle))
(setq projectile-switch-project-action 'neotree-projectile-action)

(use-package ruby-tools
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  :diminish ruby-tools-mode)

(use-package haskell-mode :ensure t)
(use-package yasnippet :ensure t)
(use-package haskell-snippets :ensure t)

(add-hook 'haskell-mode-hook
          (lambda ()
            (interactive)
            (haskell-indentation-mode)
            (interactive-haskell-mode)
            (yas-minor-mode)))

;; EVIL
(use-package evil
  :ensure t
  :init
  (global-set-key (kbd "<f4>") 'evil-mode))

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

(add-to-list 'load-path "/home/jclemer/kit-mode")
(autoload 'kit-mode "kit-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.kit\\'" . kit-mode))

;; Colors
(load-theme 'nord t)
(setq nord-comment-brightness 20)

(setq highlight-indent-guides-method 'column)

(defgroup highlight-indentation nil
  "Highlight Indentation"
  :prefix "highlight-indentation-"
  :group 'basic-faces)

(defface highlight-indentation-face
  ;; Fringe has non intrusive color in most color-themes
  '((t :inherit fringe))
  "Basic face for highlighting indentation guides."
  :group 'highlight-indentation)

(defcustom highlight-indentation-offset
  (if (and (boundp 'standard-indent) standard-indent) standard-indent 2)
  "Default indentation offset, used if no other can be found from
  major mode. This value is always used by
  `highlight-indentation-mode' if set buffer local. Set buffer
  local with `highlight-indentation-set-offset'"
  :group 'highlight-indentation)

(defvar highlight-indentation-overlay-priority 1)
(defvar highlight-indentation-current-column-overlay-priority 2)

(defconst highlight-indentation-hooks
  '((after-change-functions (lambda (start end length)
                              (highlight-indentation-redraw-region
                               start end
                               'highlight-indentation-overlay
                               'highlight-indentation-put-overlays-region))
                            t t)
    (window-scroll-functions (lambda (win start)
                               (highlight-indentation-redraw-window
                                win
                                'highlight-indentation-overlay
                                'highlight-indentation-put-overlays-region
                                start))
                             nil t)))

(defun highlight-indentation-get-buffer-windows (&optional all-frames)
  "Return a list of windows displaying the current buffer."
  (get-buffer-window-list (current-buffer) 'no-minibuf all-frames))

(defun highlight-indentation-delete-overlays-buffer (overlay)
  "Delete all overlays in the current buffer."
  (save-restriction
    (widen)
    (highlight-indentation-delete-overlays-region (point-min) (point-max) overlay)))

(defun highlight-indentation-delete-overlays-region (start end overlay)
  "Delete overlays between START and END."
  (mapc #'(lambda (o)
            (if (overlay-get o overlay) (delete-overlay o)))
        (overlays-in start end)))

(defun highlight-indentation-redraw-window (win overlay func &optional start)
  "Redraw win starting from START."
  (highlight-indentation-redraw-region (or start (window-start win)) (window-end win t) overlay func))

(defun highlight-indentation-redraw-region (start end overlay func)
  "Erease and read overlays between START and END."
  (save-match-data
    (save-excursion
      (let ((inhibit-point-motion-hooks t)
            (end (save-excursion (goto-char end) (line-beginning-position 2))))
        (highlight-indentation-delete-overlays-region start end overlay)
        (funcall func start end overlay)))))

(defun highlight-indentation-redraw-all-windows (overlay func &optional all-frames)
  "Redraw the all windows showing the current buffer."
  (dolist (win (highlight-indentation-get-buffer-windows all-frames))
    (highlight-indentation-redraw-window win overlay func)))

(defun highlight-indentation-put-overlays-region (start end overlay)
  "Place overlays between START and END."
  (goto-char start)
  (let (o ;; overlay
        (last-indent 0)
        (pos start))
    (while (< pos end)
      (beginning-of-line)
      (while (and (integerp (char-after))
                  (not (= 10 (char-after))) ;; newline
                  (= 32 (char-after))) ;; space
        (when (= 0 (% (current-column) highlight-indentation-offset))
          (setq pos (point)
                last-indent pos
                o (make-overlay pos (+ pos 1)))
          (overlay-put o overlay t)
          (overlay-put o 'priority highlight-indentation-overlay-priority)
          (overlay-put o 'face 'highlight-indentation-face))
        (forward-char))
      (forward-line) ;; Next line
      (setq pos (point)))))

(defun highlight-indentation-guess-offset ()
  "Get indentation offset of current buffer."
  (cond ((and (eq major-mode 'python-mode) (boundp 'python-indent))
         python-indent)
        ((and (eq major-mode 'python-mode) (boundp 'py-indent-offset))
         py-indent-offset)
        ((and (eq major-mode 'python-mode) (boundp 'python-indent-offset))
         python-indent-offset)
        ((and (eq major-mode 'ruby-mode) (boundp 'ruby-indent-level))
         ruby-indent-level)
        ((and (eq major-mode 'scala-mode) (boundp 'scala-indent:step))
         scala-indent:step)
        ((and (eq major-mode 'scala-mode) (boundp 'scala-mode-indent:step))
         scala-mode-indent:step)
        ((and (or (eq major-mode 'scss-mode) (eq major-mode 'css-mode)) (boundp 'css-indent-offset))
         css-indent-offset)
        ((and (eq major-mode 'nxml-mode) (boundp 'nxml-child-indent))
         nxml-child-indent)
        ((and (eq major-mode 'coffee-mode) (boundp 'coffee-tab-width))
         coffee-tab-width)
        ((and (eq major-mode 'js-mode) (boundp 'js-indent-level))
         js-indent-level)
        ((and (eq major-mode 'js2-mode) (boundp 'js2-basic-offset))
         js2-basic-offset)
        ((and (fboundp 'derived-mode-class) (eq (derived-mode-class major-mode) 'sws-mode) (boundp 'sws-tab-width))
         sws-tab-width)
        ((and (eq major-mode 'web-mode) (boundp 'web-mode-markup-indent-offset))
         web-mode-markup-indent-offset) ; other similar vars: web-mode-{css-indent,scripts}-offset
        ((and (eq major-mode 'web-mode) (boundp 'web-mode-html-offset)) ; old var
         web-mode-html-offset)
        ((and (local-variable-p 'c-basic-offset) (boundp 'c-basic-offset))
         c-basic-offset)
        ((and (eq major-mode 'yaml-mode) (boundp 'yaml-indent-offset))
         yaml-indent-offset)
        ((and (eq major-mode 'elixir-mode) (boundp 'elixir-smie-indent-basic))
         elixir-smie-indent-basic)
        (t
         (default-value 'highlight-indentation-offset))))

;;;###autoload
(define-minor-mode highlight-indentation-mode
  "Highlight indentation minor mode highlights indentation based on spaces"
  :lighter " ||"
  (when (not highlight-indentation-mode) ;; OFF
    (highlight-indentation-delete-overlays-buffer 'highlight-indentation-overlay)
    (dolist (hook highlight-indentation-hooks)
      (remove-hook (car hook) (nth 1 hook) (nth 3 hook))))

  (when highlight-indentation-mode ;; ON
    (when (not (local-variable-p 'highlight-indentation-offset))
      (set (make-local-variable 'highlight-indentation-offset)
           (highlight-indentation-guess-offset)))

    ;; Setup hooks
    (dolist (hook highlight-indentation-hooks)
      (apply 'add-hook hook))
    (highlight-indentation-redraw-all-windows 'highlight-indentation-overlay
                                              'highlight-indentation-put-overlays-region)))

;;;###autoload
(defun highlight-indentation-set-offset (offset)
  "Set indentation offset localy in buffer, will prevent
highlight-indentation from trying to guess indentation offset
from major mode"
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "Indentation offset: "))))
  (set (make-local-variable 'highlight-indentation-offset) offset)
  (when highlight-indentation-mode
    (highlight-indentation-mode)))

;;; This minor mode will highlight the indentation of the current line
;;; as a vertical bar (grey background color) aligned with the column of the
;;; first character of the current line.
(defface highlight-indentation-current-column-face
  ;; Fringe has non intrusive color in most color-themes
  '((t (:background "black")))
  "Basic face for highlighting indentation guides."
  :group 'highlight-indentation)

(defconst highlight-indentation-current-column-hooks
  '((post-command-hook (lambda ()
                         (highlight-indentation-redraw-all-windows 'highlight-indentation-current-column-overlay
                                                                   'highlight-indentation-current-column-put-overlays-region)) nil t)))

(defun highlight-indentation-current-column-put-overlays-region (start end overlay)
  "Place overlays between START and END."
  (let (o ;; overlay
        (last-indent 0)
        (indent (save-excursion (back-to-indentation) (current-column)))
        (pos start))
    (goto-char start)
    ;; (message "doing it %d" indent)
    (while (< pos end)
      (beginning-of-line)
      (while (and (integerp (char-after))
                  (not (= 10 (char-after))) ;; newline
                  (= 32 (char-after))) ;; space
        (when (= (current-column) indent)
          (setq pos (point)
                last-indent pos
                o (make-overlay pos (+ pos 1)))
          (overlay-put o overlay t)
          (overlay-put o 'priority highlight-indentation-current-column-overlay-priority)
          (overlay-put o 'face 'highlight-indentation-current-column-face))
        (forward-char))
      (forward-line) ;; Next line
      (setq pos (point)))))

;;;###autoload
(define-minor-mode highlight-indentation-current-column-mode
  "Hilight Indentation minor mode displays a vertical bar
corresponding to the indentation of the current line"
  :lighter " |"

  (when (not highlight-indentation-current-column-mode) ;; OFF
    (highlight-indentation-delete-overlays-buffer 'highlight-indentation-current-column-overlay)
    (dolist (hook highlight-indentation-current-column-hooks)
      (remove-hook (car hook) (nth 1 hook) (nth 3 hook))))

  (when highlight-indentation-current-column-mode ;; ON
    (when (not (local-variable-p 'highlight-indentation-offset))
      (set (make-local-variable 'highlight-indentation-offset)
           (highlight-indentation-guess-offset)))

    ;; Setup hooks
    (dolist (hook highlight-indentation-current-column-hooks)
      (apply 'add-hook hook))
    (highlight-indentation-redraw-all-windows 'highlight-indentation-current-column-overlay
                                              'highlight-indentation-current-column-put-overlays-region)))

(highlight-indentation-mode)

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

(add-hook 'after-init-hook 'global-company-mode)

(use-package projectile
  :ensure t
  :config
  (setq projectile-indexing-method 'native)
  (setq projectile-enable-caching t)
  :init
  (projectile-global-mode)
  (setq projectile-indexing-method 'native)  
  (setq projectile-enable-caching t))

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(fset `yes-or-no-p `y-or-n-p)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

(global-set-key (kbd "C-<return>") 'newline)
(global-undo-tree-mode -1)

(setq js-indent-level 2)

(setq auto-save-default nil)
;; Use this for remote so I can specify command line arguments
;; this function is five kinds of ugly...
(defun remote-term (new-buffer-name cmd &rest switches)
  (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name
			       term-ansi-buffer-name))
  (setq term-ansi-buffer-name
	(apply 'make-term term-ansi-buffer-name
	       cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))

;; remote shell prompts for hostname, connects and sets buffer to "
(defun remote-connect (hostname)
  (interactive "sConnect: ")
  (remote-term (format "%s ssh" hostname)
               "/usr/bin/ssh"
               hostname))

;; new shells named "localhost <N>"
(defun open-localhost ()
  (interactive)
  (ansi-term "bash" "localhost"))

(global-set-key (kbd "<f9>") 'open-localhost)
(global-set-key (kbd "<f10>") 'remote-connect)

(defun turn-on-highlight-indentation-mode ()
  "Turns on highlight-indentation-mode."
  (interactive)
  (highlight-indentation-mode 1))

(set-face-background 'highlight-indentation-face "#3d4450")
(set-face-background 'highlight-indentation-current-column-face "#a3be8c")

(define-globalized-minor-mode
  global-highlight-indentation-mode
  highlight-indentation-mode
  turn-on-highlight-indentation-mode)

(global-highlight-indentation-mode 1)

(defun zoom-frame (&optional n frame amt)
  "Increase the default size of text by AMT inside FRAME N times.
  N can be given as a prefix arg.
  AMT will default to 10.
  FRAME will default the selected frame."
  (interactive "p")
  (let ((frame (or frame (selected-frame)))
        (height (+ (face-attribute 'default :height frame) (* n (or amt 10)))))
    (set-face-attribute 'default frame :height height)
    (when (called-interactively-p)
      (message "Set frame's default text height to %d." height))))

(defun zoom-frame-out (&optional n frame amt)
  "Call `zoom-frame' with -N."
  (interactive "p")
  (zoom-frame (- n) frame amt))

(global-set-key (kbd "C-c z i") 'zoom-frame)

(global-set-key (kbd "C-c z o") 'zoom-frame-out)

(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(use-package racer-mode
  :ensure t
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-mode
  :ensure t
  :init
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (setq racer-rust-src-path "/home/jclemer/rust/src") ;; Rust source code PATH
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
  (add-hook 'rust-mode-hook #'racer-mode))

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
    (racer racer-mode flycheck-rust company-mode cargo neotree racket-mode flymake-rust haskell-snippets yasnippet enh-ruby-mode markdown-preview-mode markdown-mode ensime scala-mode elixir-mode kotlin-mode melpa-upstream-visit rust-playground rust-mode ponylang-mode js-auto-format-mode electric-spacing electric-operator electric-case yaml-mode projectile-rails projectile ggtags color-theme-x color-theme-approximate color-theme-modern color-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow solarized-theme magit nord yari use-package smartparens rvm ruby-tools rubocop nord-theme inf-ruby evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
