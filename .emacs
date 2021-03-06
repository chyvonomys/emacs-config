(setq my-system-name (car (split-string (system-name) "\\.")))

;; WIN SETUP
;; get scoop
;; $ scoop install coreutils findutils diffutils grep git
;; $ scoop bucket add extras
;; $ scoop install emacs
;; git clone emacs-config repo
;; mklink home/AppData/Roaming/.emacs emacs-config/.emacs

(if (string= my-system-name "RV589")
    (progn
      (print "Setting specific paths to find and grep on RV589")
      (setq find-program (shell-quote-argument
			  (substitute-in-file-name
			   "$USERPROFILE/scoop/shims/find.exe")))))

;; lilac setup
;; Seil:
;; -- CapsLock -> ESC
;; -- RightCommand -> RightControl
;; -- language -> Option+Space
;; -- Alfred -> Command+Space

;; use Seil to remap Right Command to Right Control

(if (string= my-system-name "lilac")
    (progn
      (print "Setting command key to act as C on lilac (osx)")
      (setq mac-command-modifier 'control)))

(require 'ido)
(ido-mode 1)

(setq my-package-list '(avy bind-key which-key magit rust-mode restclient flycheck-rust lua-mode toml-mode interaction-log ob-restclient dockerfile-mode docker-tramp))

;; EXPAND-REGION
;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

;; MULTIPLE-CURSORS
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents t)

(dolist (x my-package-list)
  (unless (package-installed-p x)
    (package-install x)))

(require 'bind-key)
(require 'magit)

(bind-key "<f5>" 'redraw-display)
(bind-key "C-<f5>" 'revert-buffer)

(defun my-edit-config ()
  (interactive)
  (find-file "~/.emacs"))
(bind-key "C-c `" 'my-edit-config)

;; 'toggles'
(bind-key "C-c t t" 'toggle-truncate-lines)
(bind-key "C-c t c" 'comment-or-uncomment-region)
(bind-key "C-c t w" 'whitespace-mode)

(bind-key "C-c g" 'magit-status)
(bind-key "C-c c" 'eshell-command)

(setq my-default-font (cond
		       ((string= my-system-name "deep") '(:family "Liberation Mono" :height 90))
		       ((string= my-system-name "MAREVO") '(:family "Liberation Mono" :height 90))
		       ((string= my-system-name "RV589") '(:family "Consolas" :height 100))
		       ((string= my-system-name "lilac") '(:family "Fantasque Sans Mono" :height 120))
		       ((string= my-system-name "COBALT") '(:family "Anka/Coder Condensed" :height 100))
		       (t nil)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type 'bar)
 '(custom-enabled-themes '(tango-dark))
 '(mode-require-final-newline nil)
 '(package-selected-packages
   '(avy which-key toml-mode rust-mode ob-restclient magit lua-mode interaction-log flycheck-rust dockerfile-mode docker-tramp bind-key))
 '(require-final-newline nil)
 '(x-stretch-cursor t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fantasque Sans Mono" :height 120))))
 '(font-lock-string-face ((t (:background "#403000" :foreground "#f0c070"))))
 '(mode-line ((t (:background "#d3d7cf" :foreground "#2e3436" :box nil))))
 '(mode-line-inactive ((t (:background "#555753" :foreground "#eeeeec" :box nil))))
 '(secondary-selection ((t (:background "#ffffbb"))))
 '(variable-pitch ((t (:family "Georgia")))))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; dont ring bell in case of following commands
(setq skip-bell-list '(mwheel-scroll
		       left-char right-char
		       previous-line next-line
		       scroll-down-command scroll-up-command))

(setq ring-bell-function (lambda () (unless (memq this-command skip-bell-list) (ding))))

;; startup
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(transient-mark-mode 1)
(delete-selection-mode 1)
(global-font-lock-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; keep history across sessions
(savehist-mode 1)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; C
(defun my-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'statement-cont 0)

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)

  (local-set-key (kbd "C-c b") 'compile))

(defun my-lua-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq lua-indent-level 3))

(add-hook 'lua-mode-hook 'my-lua-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)

(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; RUST
(require 'rust-mode)
(defun my-rust-mode-hook ()
  (flycheck-rust-setup)
  (flycheck-mode)
  (setq indent-tabs-mode nil))

(add-hook 'rust-mode-hook 'my-rust-mode-hook)
;(add-hook 'rust-mode-hook #'racer-mode)
;(add-hook 'racer-mode-hook #'eldoc-mode)
;(add-hook 'racer-mode-hook #'company-mode)
;(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;(setq company-tooltip-align-annotations t)

(set-frame-size (selected-frame) 200 50)
(split-window-horizontally)

(defface rust-unit '((t (:background "#0c3540" :foreground "#2aa198"))) "Cyan")
(font-lock-add-keywords 'rust-mode '(("\\(^\\|->\\|=>\\|:\\|,\\|;\\|[^_[:alnum:]]let\\|[^_[:alnum:]]if\\|[^_[:alnum:]]match\\|[^_[:alnum:]]return\\|\\[\\|(\\|{\\|<\\||\\|=\\)\\([[:space:]]*\\)\\(()\\)" 3 'rust-unit)))

;; workaround for ~/.emacs.d/server being 'unsafe' on windows
(when (and (>= emacs-major-version 23)
	   (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))

(setq w32-get-true-file-attributes nil)

;; AVY
(require 'avy)
(bind-key "C-," 'avy-goto-char-timer)

;; WHICH-KEY
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right)

;; INTERACTION-LOG
(require 'interaction-log)
(interaction-log-mode +1)
(global-set-key (kbd "C-h C-l") (lambda () (interactive) (display-buffer ilog-buffer-name)))

(setq lsp-rust-server 'rust-analyzer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
