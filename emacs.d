;;; Based on Aaron Bedra's configuration file (http://www.aaronbedra.com/emacs.d/)

;User details
(setq user-full-name "NAME")
(setq user-mail-address "EMAIL")

;Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;Add path
(add-to-list 'load-path "~/Dropbox/SharedConf/")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;Skip splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;Turn off bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;Display settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;Backup directory
(add-to-list 'backup-directory-alist '("." . "~/Dropbox/.emacsBackups"))

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
(show-paren-mode t)

;Org mode
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "DONE")))
(add-hook 'org-mode-hook
	  (lambda ()
	    (flyspell-mode)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (writegood-mode)))

;Flyspell mode
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;;CUDA mode
(autoload 'cuda-mode "cuda-mode.el")
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

;;Write good mode
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)

;;Start Latex mode with flyspell and writegood minor modes
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
(add-hook 'latex-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'latex-mode-hook (lambda () (writegood-mode 1)))

;;Undo tree mode
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-\\") 'redo)

;;C mode
(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;YAsnippet
(setq yas-minor-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'yas-expand)
    map))
(require 'yasnippet)
(setq yas-snippet-dirs "~/Dropbox/SharedConf/snippets")
(yas-global-mode 1)

;;Auto complete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq ac-source-yasnippet nil)

;AC Clang
(require 'auto-complete-clang)
;(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)

;;ESS mode
(require 'ess-site)

;;Enables and configures Helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(setq helm-split-window-default-side 'right)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;;; C-x 3 C-x o --> C-t
;;; copied from http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;;Flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(r-lintr))

;;Lines numbering
(global-linum-mode 1)
(setq linum-format "%4d \u2502 ")

;Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#111")
(set-face-foreground 'highlight nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(imakado undo-tree helm-fuzzy-find flycheck ess ecb auto-complete-clang ac-clang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

; Find the closest Makefile
(require 'cl) ; If you don't have it already
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
					  (loop
					   for d = default-directory then (expand-file-name ".." d)
   					   if (file-exists-p (expand-file-name file d))
					   return d
					   if (equal d root)
					   return nil)))

; Compilation commands for different environments
(require 'compile)
(add-hook 'c++-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "make -C %s run" (get-closest-pathname)))))
(add-hook 'latex-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "make -C %s" (get-closest-pathname)))))
(add-hook 'python-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "python %s" (buffer-name)))))

; Do not ask for command confirmation when compiling
(setq compilation-read-command nil)

;; Initialize myMode to override other keybidings
(load "~/Dropbox/SharedConf/myMode.el")
(require 'my-mode)

;Misc key bindings
(define-key my-mode-map (kbd "RET") 'newline-and-indent)
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

;;; emacs.d ends here
