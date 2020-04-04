;;; Based on:
;;; http://www.aaronbedra.com/emacs.d/
;;; https://github.com/munen/emacs.d/

; User details

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name "Mateus Cruz")
(setq user-mail-address "mshcruz@gmail.com")

;Package management
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

; Define packages to install
(defvar my-packages '(
		      ac-js2
		      auto-complete
		      beacon
		      browse-kill-ring
		      counsel-jq
		      comment-tags
		      darktooth-theme
		      dired-narrow
		      diminish
		      dumb-jump
		      edit-indirect
		      editorconfig
		      erc-image
		      exec-path-from-shell
		      ffap
		      forge
		      flycheck
		      flycheck-flow
		      hide-mode-line
		      ido-vertical-mode
		      impatient-mode
		      ivy counsel swiper
		      json-mode
		      js2-mode
		      js2-refactor
		      js-comint
		      magit
		      package-lint
		      pdf-tools
		      projectile
		      rainbow-mode
		      ob-restclient
		      restclient
		      rjsx-mode
		      spacemacs-theme
		      spaceline
		      smex
		      synosaurus
		      tide
		      visual-fill-column
		      web-mode
		      which-key
		      writegood-mode
		      writeroom-mode
		      zenburn-theme))

; Install packages
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p))
    (add-to-list 'package-selected-packages p))

; Gnu Elpa TLS Fix
(if (string< emacs-version
	     "26.3")
          (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

; Garbage collection
(setq gc-cons-threshold 20000000)

; Auto-save in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
            `((".*" ,temporary-file-directory t)))

; Always follow symlinks
(setq vc-follow-symlinks t)

; Sentences have one space after period
(setq sentence-end-double-space nil

; Confirm before closing Emacs
(setq confirm-kill-emacs 'y-or-n-p)

; dired-mode
(put 'dired-find-alternate-file 'disabled nil)
(setq-default dired-listing-switches "-alh")
(setq dired-recursive-copies 'always)

; dired-narrow
(require 'dired)
(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)

; Auto revert files on change
(global-auto-revert-mode t)

; Change font-size shortcut
(defun zoom-in ()
  (interactive)
  (let ((x (+ (face-attribute 'default :height)
	      10)))
    (set-face-attribute 'default nil :height x)))

(defun zoom-out ()
  (interactive)
  (let ((x (- (face-attribute 'default :height)
	      10)))
    (set-face-attribute 'default nil :height x)))

(define-key global-map (kbd "C-1") 'zoom-in)
(define-key global-map (kbd "C-0") 'zoom-out)

;Skip splash screen
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

; Display current time
(display-time-mode t)

; Do not display GUI toolbar
(tool-bar-mode 0)

; Automatic line breaks
(add-hook 'org-mode-hook 'auto-fill-mode)

; Enable narrow to region
(put 'narrow-to-region 'disabled nil)

;Turn off bars
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
            (scroll-bar-mode -1)))

; Remember the cursor position of files when reopening them
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

; Set $MANPATH, $PATH and exec-path from shell even when started from GUI helpers like dmenu or Spotlight
(exec-path-from-shell-initialize)

; windmove
(when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))

; winner-mode
(when (fboundp 'winner-mode)
    (winner-mode 1))

; eww: scroll without changing point
(add-hook 'eww-mode-hook 'scroll-lock-mode)

; Clean buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-dired-buffers ()
  "Kill all open dired buffers."
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))

; Clean up messy buffers
(defun visual-clean ()
  "Clean up messy buffers (i.e. web wikis or elfeed-show)"
  (interactive)
  (visual-line-mode)
  (visual-fill-column-mode))

; Helper to measure the running time of a function
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

; Sudo save
(defun ph/sudo-file-name (filename)
    "Prepend '/sudo:root@`system-name`:' to FILENAME if appropriate.
This is, when it doesn't already have a sudo-prefix."
    (if (not (or (string-prefix-p "/sudo:root@localhost:"
				  filename)
		 (string-prefix-p (format "/sudo:root@%s:" system-name)
				  filename)))
	(format "/sudo:root@%s:%s" system-name filename)
      filename))

(defun ph/sudo-save-buffer ()
  "Save FILENAME with sudo if the user approves."
  (interactive)
  (when buffer-file-name
    (let ((file (ph/sudo-file-name buffer-file-name)))
      (if (yes-or-no-p (format "Save file as %s ? " file))
	  (write-file file)))))

(advice-add 'save-buffer :around
	    '(lambda (fn &rest args)
	       (when (or (not (buffer-file-name))
			 (not (buffer-modified-p))
			 (file-writable-p (buffer-file-name))
			 (not (ph/sudo-save-buffer)))
		 (call-interactively fn args))))

; beadon-mode: Whenever the window scrolls a light will shine on top of your cursor so you know where it is.
(beacon-mode 1)

; browse-kill-ring
(require 'browse-kill-ring)
(setq browse-kill-ring-highlight-inserted-item t
      browse-kill-ring-highlight-current-entry nil
      browse-kill-ring-show-preview t)
(define-key browse-kill-ring-mode-map (kbd "j") 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map (kbd "k") 'browse-kill-ring-previous)

; which-key: displays available keybindings in a popup.
(add-hook 'org-mode-hook 'which-key-mode)
(add-hook 'cider-mode-hook 'which-key-mode)

; Auto complete
(ac-config-default)

; Tabs
(setq-default tab-width 2)
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq python-indent 2)
(setq css-indent-offset 2)
(add-hook 'sh-mode-hook
	  (lambda ()
	    (setq sh-basic-offset 2
		  sh-indentation 2)))
(setq web-mode-markup-indent-offset 2)

; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; Manage TODO/FIXME/etc tags
(setq comment-tags-keymap-prefix (kbd "C-c t"))
(with-eval-after-load "comment-tags"
  (setq comment-tags-keyword-faces
	`(;; A concrete TODO with actionable steps
	  ("TODO" . ,(list :weight 'bold :foreground "#DF5427"))
	  ;; A non-concrete TODO. We only know something is broken/amiss.
	  ("FIXME" . ,(list :weight 'bold :foreground "#DF5427"))
	  ;; Works, but is a code smell (quick fix). Might break down the line.
	  ("HACK" . ,(list :weight 'bold :foreground "#DF5427"))
	  ;; Assumption that needs to be verified.
	  ("CHECK" . ,(list :weight 'bold :foreground "#CC6437"))
	  ;; Use to highlight a regular, but especially important, comment.
	  ("NOTE" . ,(list :weight 'bold :foreground "#1FDA9A"))
	  ;; Use to highlight a regular, but especially important, comment.
	  ("INFO" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
	comment-tags-require-colon t
	comment-tags-case-sensitive t
	comment-tags-show-faces t
	comment-tags-lighter nil))
(add-hook 'prog-mode-hook 'comment-tags-mode)
(add-hook 'conf-mode-hook 'comment-tags-mode)

; Auto-indent with the Return key
(define-key global-map (kbd "RET") 'newline-and-indent)

; Highlight matching parenthesis
(show-paren-mode t)

; Delete trailing whitespace
(add-hook 'before-save-hook '(lambda()
			       (when (not (or (derived-mode-p 'markdown-mode)))
				 (delete-trailing-whitespace))))


; Code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

; Line numbers
(add-hook 'prog-mode-hook '(lambda ()
			     (if (version<= emacs-version "26.0.50")
				 (linum-mode)
			       (display-line-numbers-mode))))

; Tide
(require 'rjsx-mode)
(define-key rjsx-mode-map (kbd "C-c C-r") 'tide-rename-symbol)
(define-key rjsx-mode-map (kbd "C-c C-d") 'tide-documentation-at-point)

(defun setup-tide-mode ()
  (interactive)
  ;; For bigger JS projects and intense tasks like =tide=references=
  ;; the default of 2s will time out
  (setq tide-sync-request-timeout 10)
  (tide-setup)
  ;; Increase sync request timeout for bigger projects
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))
(add-hook 'rjsx-mode-hook #'setup-tide-mode)

; js-comint
(require 'js-comint)

(add-hook 'rjsx-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
	    (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
	    (local-set-key (kbd "C-c b") 'js-send-buffer)
	    (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
	                (local-set-key (kbd "C-c l") 'js-load-file-and-go)))

; flow: static typechecker for JS
(load-file "~/.emacs.d/flow-for-emacs/flow.el")

; flycheck-clow
(require 'flycheck-flow)
(add-hook 'javascript-mode-hook 'flycheck-mode)

; rjsx-mode
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

; General JS config
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(setq js2-highlight-level 3)
(setq js-indent-level 2)
;; Semicolons are optional in JS, do not warn about them missing
(setq js2-strict-missing-semi-warning nil)

; rainbow-mode
(add-hook 'prog-mode-hook 'rainbow-mode)

; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; Ruby Templates
(add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
;; Handlebars
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
;; JSON
(add-to-list 'auto-mode-alist '("\\.json?\\'" . web-mode))

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-ac-sources-alist
      '(("html" . (ac-source-words-in-buffer ac-source-abbrev))))

; Magit
(global-set-key (kbd "C-x g") 'magit-status)

; ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

; Ivy/Counsel/Swiper
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "<f6>") 'ivy-resume)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-wrap t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-x b") 'counsel-ibuffer)
;; Run `counsel-ag` against the current directory and not against the
;; whole project
(global-set-key (kbd "C-c k") '(lambda()
                                 (interactive)
                                 (counsel-ag "" default-directory nil nil)))
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(global-set-key (kbd "M-x") (lambda ()
                              (interactive)
                              (counsel-M-x "")))
(setq projectile-completion-system 'ivy)
(setq mu4e-completing-read-function 'ivy-completing-read)
(setq synosaurus-choose-method 'ivy-read)

; Spaceline
(require 'spaceline)

(spaceline-define-segment ph/flycheck-warning-segment
                          (if (flycheck-has-current-errors-p)
                              (let ((c (cdr (assq 'warning (flycheck-count-errors
                                                            flycheck-current-errors)))))
                                (powerline-raw
                                 (if c (format "%s" c))))))

(spaceline-define-segment ph/flycheck-error-segment
                          (if (flycheck-has-current-errors-p)
                              (let ((c (cdr (assq 'error (flycheck-count-errors
                                                          flycheck-current-errors)))))
                                (powerline-raw
                                 (if c (format "%s" c))))))

(spaceline-define-segment ph/flycheck-info-segment
                          (if (flycheck-has-current-errors-p)
                              (let ((c (cdr (assq 'info (flycheck-count-errors
                                                         flycheck-current-errors)))))
                                (powerline-raw
                                 (if c (format "%s" c))))))

(defface ph/spaceline-flycheck-error-face
  '((t :inherit 'mode-line
       :weight bold
       :foreground "white"
       :background "dark red"))
  "Flycheck Error Face"
  :group 'spaceline)

(defface ph/spaceline-flycheck-warning-face
  '((t :inherit 'mode-line
       :weight bold
       :foreground "white"
       :background "DarkOrange3"))
  "Flycheck Warning Face"
  :group 'spaceline)

(defface ph/spaceline-flycheck-info-face
  '((t :inherit 'mode-line
       :weight bold
       :foreground "white"
       :background "dark green"))
  "Flycheck Info Face"
  :group 'spaceline)

(defun ph/git-branch-name ()
  (replace-regexp-in-string "^ Git[:-]" "" vc-mode))

(spaceline-define-segment ph/version-control
                          "Version control information."
                          (when vc-mode
                            (s-trim (concat (ph/git-branch-name)))))

(spaceline-define-segment ph/remote-method
                          (when (and default-directory
                                     (file-remote-p default-directory 'method))
                            (file-remote-p default-directory 'method)))

(spaceline-define-segment ph/remote-user-and-host
                          (when (and default-directory
                                     (or
                                      (file-remote-p default-directory 'user)
                                      (file-remote-p default-directory 'host)))
                            (concat
                             (file-remote-p default-directory 'user) "@"
                             (file-remote-p default-directory 'host))))

(defface ph/spaceline-tramp-user-host-face
  '((t :inherit 'mode-line
       :foreground "black"
       :background "#fce94f"))
  "Tramp User@Host Face"
  :group 'spaceline)

(defface ph/spaceline-tramp-method-face
  '((t :inherit 'mode-line
       :foreground "black"
       :background "#ff5d17"))
  "Tramp Method Face"
  :group 'spaceline)

; Diminish
(eval-after-load "auto-revert"
  '(diminish 'auto-revert-mode))
(eval-after-load "beacon"
  '(diminish 'beacon-mode))
(eval-after-load "ivy"
  '(diminish 'ivy-mode))
(eval-after-load "projectile"
  '(diminish 'projectile-mode))
(eval-after-load "projectile-rails"
  '(diminish 'projectile-rails-mode))
(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))
(eval-after-load "which-key"
    '(diminish 'which-key-mode))

; Hide mode line
(require 'hide-mode-line)

(add-hook 'pdf-view-mode-hook #'hide-mode-line-mode)

; Thesaurus
(setq synosaurus-backend 'synosaurus-backend-openthesaurus)

; Flyspell
(setq flyspell-sort-corrections nil)
(setq flyspell-issue-message-flag nil)

;Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(global-set-key (kbd "C-@") 'set-mark-command)

;Display settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;Yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;Delete line no kill
(defun delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )

;Misc
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
 (setq ring-bell-function 'ignore)
(show-paren-mode t)

;Org
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "DONE")))
(add-hook 'org-mode-hook
	  (lambda ()
	    (flyspell-mode)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (writegood-mode)))

;;Start Latex mode with flyspell and writegood minor modes
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
(add-hook 'latex-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'latex-mode-hook (lambda () (writegood-mode 1)))

;;; C-x 3 C-x o --> C-t
;;; copied from http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

; Remap C-h
(global-set-key [(control ?h)] 'delete-backward-char)

;;Lines numbering
;; (global-linum-mode 1)
;; (setq linum-format "%4d \u2502 ")

;Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#111")
(set-face-foreground 'highlight nil)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (exec-path-from-shell js2-mode json-mode web-mode tide ts-comint typescript-mode flycheck-pycheckers helm-c-yasnippet yasnippet yasnippet-snippets imakado undo-tree helm-fuzzy-find flycheck ess ecb auto-complete-clang ac-clang))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

; Open file whose name is under the cursor (file at point)
(require 'ffap)
(defun ffap-string-at-point (&optional mode)
  (let* ((args
      (cdr
       (or (assq (or mode major-mode) ffap-string-at-point-mode-alist)
           (assq 'file ffap-string-at-point-mode-alist))))
         next-comment
     (pt (point))
     (beg (if (use-region-p)
          (region-beginning)
        (save-excursion
          (skip-chars-backward (car args))
          (skip-chars-forward (nth 1 args) pt)
                  (save-excursion
                    (setq next-comment
                          (progn (comment-search-forward (line-end-position) t)
                                 (point))))
          (point))))
     (end (if (use-region-p)
          (region-end)
        (save-excursion
          (skip-chars-forward (car args))
          (skip-chars-backward (nth 2 args) pt)
          (point)))))
  (when (> end next-comment)
    (setq beg next-comment))
  (setq ffap-string-at-point
      (buffer-substring-no-properties
       (setcar ffap-string-at-point-region beg)
       (setcar (cdr ffap-string-at-point-region) end)))))

;; ; Find the closest Makefile
;; (require 'cl) ; If you don't have it already
;; (defun* get-closest-pathname (&optional (file "Makefile"))
;;   "Determine the pathname of the first instance of FILE starting from the current directory towards root.
;; This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
;; of FILE in the current directory, suitable for creation"
;;   (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
;; 					  (loop
;; 					   for d = default-directory then (expand-file-name ".." d)
;;    					   if (file-exists-p (expand-file-name file d))
;; 					   return d
;; 					   if (equal d root)
;; 					   return nil)))
;; (require 'compile)
;; (add-hook 'c++-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "make -C %s run" (get-closest-pathname)))))
;; (add-hook 'latex-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "make -C %s" (get-closest-pathname)))))
;; (add-hook 'python-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "python %s" (buffer-name)))))

; Do not ask for command confirmation when compiling
(setq compilation-read-command nil)

;; Initialize myMode to override other keybidings
(load "~/SharedConf/myMode.el")
(require 'my-mode)

;Misc key bindings
;(define-key my-mode-map (kbd "RET") 'newline-and-indent)
(define-key my-mode-map (kbd "C-h") 'delete-backward-char)
(define-key my-mode-map (kbd "C-c C-k") 'compile)
(define-key my-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region)

; Split windows vertically
(setq split-height-threshold nil)

; Scroll compilation buffer to the end
(setq compilation-scroll-output t)

; Gets the major mode of the buffer.
(defun buffer-mode (&optional buffer-or-name)
    "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
	(buffer-local-value 'major-mode
						   (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

; If it is latex-mode, close compilation screen after it's done
(add-hook 'compilation-finish-functions
		  (lambda (buf strg)
			(if
				(string= "latex-mode" (buffer-mode (other-buffer (current-buffer) 1)))
				(let ((win  (get-buffer-window buf 'visible)))
				  (when win (delete-window win)))
				)
			)
		  )

; TRAMP mode
(setq tramp-default-method "ssh")


;;; .emacs ends here
