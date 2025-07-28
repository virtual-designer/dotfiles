(setq-default image-scaling-factor 1)

;; Require essential packages
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'editorconfig)
(require 'org-remoteimg)

;; Setup package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defun require-install (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package))
  (require package))

;; Common setup
(require-install 'company)
(require-install 'yasnippet)
(require-install 'yasnippet-snippets)

(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq display-time-day-and-date t)
(setq fill-column 80)
(setq column-number-indicator-zero-based nil)
(setq make-backup-files nil)
(setq display-time-default-load-average nil)
(setq display-time-format "   %b %d %H:%M:%S")
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(editorconfig-mode 1)
(yas-global-mode 1)
(global-company-mode 1)
(display-time-mode 1)
(column-number-mode 1)

;; Modeline
(setq-default mode-line-format
      '(
	"%e"
	mode-line-front-space (:propertize (
					    ""
					    mode-line-mule-info
					    mode-line-client
					    mode-line-modified
					    mode-line-remote
					    mode-line-window-dedicated
					    )
					   display (min-width (2.0))
					   face (:foreground "#aaa"))
	mode-line-frame-identification
	(:proprtize mode-line-buffer-identification
		    face (:weight bold))
	"   "
	mode-line-position
	(project-mode-line project-mode-line-format)
	(:propertize (vc-mode vc-mode)
		     face (:foreground "#ffa126"))
	"    ["
	(:propertize mode-name
		     face (:weight bold))
	(:propertize minor-mode-alist
		     face (:foreground "#aaa"))
	"]"
	mode-line-misc-info
	"  "
	(:propertize user-login-name
		     face (:foreground "#5daafc"))))

;; Enable IDO mode
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-confirm-unique-completion nil)
(ido-mode 1)
(ido-everywhere 1)

;; multiple-cursors
(require-install 'multiple-cursors)

(global-set-key (kbd "C-c C-<down>") 'mc/mark-next-lines)
(global-set-key (kbd "C-c C-<up>") 'mc/mark-previous-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Projectile
(setq projectile-mode-line "Projectile")

;; Org mode
(setq org-support-shift-select 1)
(setq org-use-sub-superscripts '{})
(setq org-display-remote-inline-images 'cache)
(setq org-link-preview-overlays t)

(add-hook 'org-mode-hook
          (lambda ()
            (setq tab-width 8)
	    (local-set-key (kbd "C-e C-h") #'org-html-export-to-html)))

;; Enable ido-completing-read+
(require-install 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Enable flx-ido
(require-install 'flx-ido)
(flx-ido-mode 1)

;; Keybindings
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x e") #'eval-buffer)))
(global-set-key (kbd "C-c d") #'duplicate-line)
(global-set-key (kbd "C-c m") #'compile)
(global-set-key (kbd "C-c C-m") #'recompile)

;; Indentation
(setq-default indent-tabs-mode 1)

;; C mode setup
(setq c-default-style "gnu"
      c-basic-offset 4)

(setq c-offsets-alist
      '((case-label . +)
       (block-open . 0)
       (block-close . 0)
       (substatement-open . 0)
       (brace-list-open . 0)
       (brace-list-close . 0)
       ))

;; Editing
(setq require-final-newline t)

;; Use GGtags
(defun c-mode-hook-handler()
  (ggtags-mode 1)
  (display-fill-column-indicator-mode 1))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'python-mode)
              (c-mode-hook-handler))))

;; Elcord settings
(require-install 'elcord)
(setq elcord--editor-name "GNU Emacs")
(setq elcord-client-id "1398326663602901162")

(defun discord-canary-running-p ()
  (let ((output (shell-command-to-string "ps aux | grep discord-canary")))
    (not (string-empty-p output))))

(defun action-if-discord-canary-running ()
  (when (discord-canary-running-p)
    (message "Discord Canary is running!")
    (elcord-mode 1)))

(add-hook 'after-init-hook #'action-if-discord-canary-running)

;; Name, email, signature, authinfo, etc
(setq user-mail-address "rakinar2@onesoftnet.eu.org"
      user-full-name "Ar Rakin"
      smtpmail-smtp-server "mail.onesoftnet.eu.org"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-auth-credentials "~/.authinfo.gpg"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

(setq add-log-full-name user-full-name)
(setq add-log-mailing-address user-mail-address)

(setq message-organization "OSN")
(setq message-signature-file "~/.signature")

(setq nntp-authinfo-file "~/.authinfo")
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; gnus settings
(setq gnus-select-method
      '(nntp "news.eternal-september.org"
             (nntp-port-number 563)
	     (nntp-authinfo-function nntp-send-authinfo)
             (nntp-open-connection-function nntp-open-ssl-stream)))

;; Theme
(require 'doom-themes)
(load-theme 'doom-material-dark t)

(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)

(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(setq doom-themes-treemacs-theme "doom-atom")

(set-face-attribute 'default nil :background "#111111")
(set-face-attribute 'mode-line-inactive nil
		    :background "#202020"
		    :foreground "#888888")
(set-face-attribute 'mode-line nil
		    :background "#2a2a2a"
		    :foreground "#ffffff")
(set-face-attribute 'default nil :font "Jetbrains Mono-11")
;; (set-cursor-color "#0050ff")

