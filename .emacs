;; Require essential packages
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

;; Setup package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defvar package-refreshed-contents t
  "Whether package-refresh-contents was called")

(defun require-install (package)
  (unless (package-installed-p package)
    (unless package-refreshed-contents
      (package-refresh-contents)
      (setq package-refreshed-contents t))
    (package-install package))
  (require package))

(require-install 'editorconfig)

(condition-case err
    (require 'org-remoteimg)
  (error
   (message "Failed to load org-remoteimg: %s" err)))

(condition-case err
    (require 'mu4e)
  (error
   (message "Failed to load mu4e: %s" err)))
;; Common setup
(require-install 'company)
(require-install 'yasnippet)
(require-install 'yasnippet-snippets)

(setq vc-make-backup-files nil)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq display-time-day-and-date t)
(setq fill-column 80)
(setq column-number-indicator-zero-based nil)
(setq make-backup-files nil)
(setq display-time-format "   %a, %b %d %H:%M:%S")
(setq require-final-newline t)
(setq custom-file "~/.emacs.d/custom.el")
(setq image-scaling-factor 1)
(setq-default image-scaling-factor 1)

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
	""
	(:propertize (vc-mode vc-mode)
		     face (:foreground "#ffa126"))
	"    ["
	(:propertize mode-name
		     face (:weight bold))
	(:propertize minor-mode-alist
		     face (:foreground "#aaa"))
	"]"
	(:propertize ("" mode-line-misc-info)
	         face (:foreground "#ddd"))
	"  "
	(:propertize ("" user-login-name "@" system-name)
		     face (:foreground "#5daafc"))))

;; Enable IDO mode
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-confirm-unique-completion nil)
(ido-mode 1)
(ido-everywhere 1)

;; multiple-cursors
(require-install 'multiple-cursors)

(global-set-key (kbd "C-c C-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-lines)
(global-set-key (kbd "C-<") 'mc/mark-previous-lines)
(global-set-key (kbd "C-c C-d") 'mc/mark-all-like-this)

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

;; mu4e

(setq mu4e-bookmarks
  '((:name "All Inbox"
           :query "maildir:/"
           :key ?i)))

;; Keybindings
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x e") #'eval-buffer)))
(global-set-key (kbd "C-c d") #'duplicate-line)
(global-set-key (kbd "C-c m") #'compile)
(global-set-key (kbd "C-c C-m") #'recompile)
(global-set-key (kbd "C-c C-a") #'mark-whole-buffer)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list nil)
(setq-default tab-width 4)
(setq-default standard-indent 4)

(setq indent-tabs-mode nil)
(setq tab-stop-list nil)
(setq tab-width 4)

(defvar literal-tabs-mode-map (make-sparse-keymap)
  "Keymap for literal-tabs-mode.")

(defun literal-tabs--insert-tab ()
  (interactive)
  (let ((width (if (boundp 'tab-width) tab-width 4)))
    (if (use-region-p)
        ;; region selected → indent all lines
        (let ((start (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char start)
            (while (< (point) end)
              (beginning-of-line)
              (literal-tabs--insert-tab-line width)
              (forward-line 1))))
      ;; no selection → single line
      (literal-tabs--insert-tab-line width))))

(defun literal-tabs--insert-tab-line (width)
  (if indent-tabs-mode
      (insert "\t")
    ;; spaces up to next tab stop
    (let* ((col (current-column))
           (spaces (- width (% col width))))
      (insert (make-string spaces ?\s)))))

(defun literal-tabs--backtab ()
  "Outdent current line(s) by `tab-width`, like VSCode Shift-TAB."
  (interactive)
  (let ((width (if (boundp 'tab-width) tab-width 4)))
    (if (use-region-p)
        ;; region selected → outdent all lines
        (let ((start (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char start)
            (while (< (point) end)
              (beginning-of-line)
              (literal-tabs--backtab-line width)
              (forward-line 1))))
      ;; single line
      (literal-tabs--backtab-line width))))

(defun literal-tabs--backtab-line (width)
  "Outdent current line by up to WIDTH spaces or one tab."
  (let ((col (current-column)))
    (let ((remove (min width col)))
      (save-excursion
        (move-to-column 0)
        (delete-char remove)))))

;; Bind keys
(define-key literal-tabs-mode-map (kbd "TAB") 'literal-tabs--insert-tab)
(define-key literal-tabs-mode-map (kbd "<backtab>") 'literal-tabs--backtab)

(define-minor-mode literal-tabs-mode
  "Literal labs mode"
  :lighter " LT"
  :global t
  :keymap literal-tabs-mode-map)

(literal-tabs-mode 1)

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

;; Perl
(add-to-list 'auto-mode-alist '("\\.pl\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm\\'" . cperl-mode))

;; Configure cperl-mode indentation
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq cperl-indent-level 4)  ;; block indent offset
            (setq cperl-continued-statement-offset 4) ; continuation lines
            (setq cperl-close-paren-offset -4)       ; closing parens
            (setq cperl-brace-offset 0)
            (setq cperl-label-offset -1)
            (setq cperl-indent-parens-as-block t)
            (setq cperl-tab-always-indent t)
            (setq indent-tabs-mode nil)))

;; Other modes
(defun enable-tabs-mode-hook ()
  (setq indent-tabs-mode t))
  
(setq tabs-mode-enabled-modes '("makefile" "makefile-automake"))
(dolist (mode tabs-mode-enabled-modes)
	(add-hook (intern (concat mode "-mode-hook")) #'enable-tabs-mode-hook))

;; Disable line-number mode in certain buffers
(defun disable-linenum-hook ()
  (display-line-numbers-mode 0))

(add-hook 'vterm-mode-hook #'disable-linenum-hook)

;; Editing
(setq require-final-newline t)

;; Use GGtags
(defun c-mode-hook-handler()
  (ggtags-mode 1)
  (display-fill-column-indicator-mode 1)
  (auto-fill-mode 1))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'python-mode)
              (c-mode-hook-handler))))

;; Elcord settings
(require-install 'elcord)
(setq elcord--editor-name "GNU Emacs")
(setq elcord-client-id "1398326663602901162")

;; Name, email, signature, authinfo, etc
(setq user-mail-address "rakinar2@osndevs.org"
      user-full-name "Ar Rakin"
      smtpmail-smtp-server "mail.osndevs.org"
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
(require-install 'doom-themes)
(load-theme 'doom-material-dark t)

(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)

(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(setq doom-themes-padded-modeline t)

(setq doom-themes-treemacs-theme "doom-atom")

(set-face-attribute 'default nil :background "#101010")
(set-face-attribute 'mode-line-inactive nil
		    :background "#202020"
		    :foreground "#888888")
(set-face-attribute 'mode-line nil
		    :background "#2a2a2a"
		    :foreground "#ffffff")

(set-face-attribute 'default nil :family "JetBrainsMono NFP" :height 110)
(set-face-attribute 'variable-pitch nil
                    :family "Inter"
                    :height 110)

(set-face-attribute 'cursor nil :background "#007bff")
(add-to-list 'default-frame-alist '(cursor-color . "#007bff"))

(defun new-frame-handler (new-frame)
  (modify-frame-parameters new-frame '((cursor-color . "#007bff"))))

(add-hook 'after-make-frame-functions #'new-frame-handler)

