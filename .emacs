(setq my-system-name (car (split-string (system-name) "\\.")))

(if (string= my-system-name "RV015")
    (progn
      (print "Setting specific paths to find and grep on RV015")
      (setq find-program (shell-quote-argument "D:/GNU/bin/find.exe"))
      (setq grep-program (shell-quote-argument "D:/GNU/bin/grep.exe"))))

;; lilac setup
;; Seil:
;; -- CapsLock -> ESC
;; -- RightCommand -> RightControl
;; -- language -> Option+Space
;; -- Alfred -> Command+Space

;; use Seil to remap Right Command to Right Control

;; (if (string= my-system-name "lilac")
;;     (progn
;;       (print "Setting command key to act as C on lilac (osx)")
;;       (setq mac-command-modifier 'control)))

(require 'ido)
(ido-mode 1)

;; EXPAND REGION
;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

(require 'package)
(package-initialize)
(require 'bind-key)
(require 'magit)

;; 'toggles'
(bind-key "C-c t t" 'toggle-truncate-lines)
(bind-key "C-c t c" 'comment-or-uncomment-region)
(bind-key "C-c t w" 'whitespace-mode)

(bind-key "C-c g" 'magit-status)
(bind-key "C-c c" 'eshell-command)

(require 'free-keys)

(setq my-default-font (cond
		       ((string= my-system-name "deep") '(:family "Liberation Mono" :height 90))
		       ((string= my-system-name "MAREVO") '(:family "Liberation Mono" :height 90))
		       ((string= my-system-name "RV015") '(:family "Droid Sans Mono" :height 110))
		       ;;((string= my-system-name "lilac") '(:family "PT Mono" :height 110))
		       ((string= my-system-name "lilac") '(:family "Inconsolata" :height 140))
		       (t nil)))

(custom-set-variables
 `(custom-enabled-themes '(tango-dark)))

(custom-set-faces
 `(default ((t ,my-default-font)))
 '(linum ((t (:foreground "LightSkyBlue4"))))
 '(font-lock-string-face ((t (:background "#403000" :foreground "#f0c070"))))
 '(mode-line ((t (:background "#d3d7cf" :foreground "#2e3436" :box nil))))
 '(mode-line-inactive ((t (:background "#555753" :foreground "#eeeeec" :box nil))))
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
(global-font-lock-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; line numbers
(global-linum-mode 1)
(setq linum-format "%4s ")

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

;; LISTEN
(server-start)

;; 175 x 45
(set-frame-size (selected-frame) 175 45)
