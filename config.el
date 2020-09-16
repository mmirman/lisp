;;;;;;;;;;;;;;;;;;;;;
;; GENERAL CONFIGS ;;
;;;;;;;;;;;;;;;;;;;;;

(transient-mark-mode 1)
(winner-mode 1)

(auto-insert-mode)

(put 'dired-find-alternate-file 'disabled nil)
(menu-bar-mode -1)

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-to-load-path '("~/.emacs.d/elpa/dash-20150611.922/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-hook 'shell-mode-hook 
          'ansi-color-for-comint-mode-on)

;; BACKUP DIRECTORY
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-default nil
 )       ; use versioned backups

(setq-default c-basic-offset 4 
              c-indent-level 4
              tab-width 4
              indent-tabs-mode nil
              truncate-lines t
              c-continued-statement-offset 4
              )


;; XTERM MOUSE 
(xterm-mouse-mode 1)
;; (mouse-sel-mode 1)

;; WINDOW EDITING
(defun prev-window ()
  (interactive)
  (other-window -1))

;; RELOAD THIS FILE
(defun reconfig () 
  (interactive)
  (load-file "~/.emacs"))

;; create a shell in a buffer with a given name
(defun shell-named ()
  "creates a shell with a given name"
  (interactive)
  (let ((shell-name (read-string "shell name: " nil)))
    (shell shell-name)))

(defun set-ido-keys ()
  (interactive)
  (global-set-key (kbd "C-x f") 'find-file)
  (global-set-key (kbd "C-x C-f") 'find-file)
)


(defun set-my-keys () 
  (interactive)
  (windmove-default-keybindings)
  (global-set-key (kbd "C-c t") 'toggle-truncate-lines)

  (global-set-key (kbd "C-x \\") 'goto-line )
  (global-set-key (kbd "C-x o") 'prev-window )
  (global-set-key (kbd "C-u") 'prev-window )
  (global-set-key (kbd "C-o") 'other-window )

  (global-set-key (kbd "C-c r") 'rename-buffer)
  (global-set-key (kbd "C-x %") 'shrink-window)
  (global-set-key (kbd "C-c C-k") 'global-set-key)
  (global-set-key (kbd "C-c C-k") 'global-set-key)
  (global-set-key (kbd "C-x a r") 'align-regexp)
  (global-set-key (kbd "M-R") 'replace-regexp)
  (global-set-key (kbd "C-x C-b") 'buffer-menu)

  (global-set-key (kbd "C-c C-n") 'phi-search)
  (global-set-key (kbd "C-c C-p") 'phi-search-backward)
  (global-set-key (kbd "C-c t") 'toggle-truncate-lines)

  (global-set-key (kbd "C-c e") 'mc/edit-lines)
  (global-set-key (kbd "C-c b") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c C-b") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c d") 'set-rectangular-region-anchor) ;; draw

  (global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)

  (global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-a") 'mc/mark-all-words-like-this)
  (global-set-key (kbd "C-c M-a") 'mc/mark-all-symbols-like-this)
  (global-set-key (kbd "C-c C-s") 'mc/mark-all-in-region)
  (global-set-key (kbd "C-c M-s") 'mc/mark-all-dwim)

  (global-set-key (kbd "C-c C-u") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-c u") 'mc/unmark-previous-like-this)

  (global-set-key (kbd "C-c y") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-c C-y") 'mc/skip-to-previous-like-this)


  (global-set-key (kbd "C-c i") 'mc/mark-next-like-this-symbol)
  (global-set-key (kbd "C-c w") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-c C-i") 'mc/mark-next-symbol-like-this)
  (global-set-key (kbd "C-c C-w") 'mc/mark-next-word-like-this)

  (global-set-key (kbd "C-c M-i") 'mc/mark-previous-like-this-symbol)
  (global-set-key (kbd "C-c M-w") 'mc/mark-previous-like-this-word)
  (global-set-key (kbd "C-x M-i") 'mc/mark-previous-symbol-like-this)
  (global-set-key (kbd "C-x M-w") 'mc/mark-previous-word-like-this)

  (global-set-key (kbd "C-c l") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-c p") 'mc/mark-pop)
  (global-set-key (kbd "C-c C-r") 'mc/reverse-regions)

  (global-set-key (kbd "C-c o") 'mc/sort-regions) ;; order

  (global-set-key (kbd "C-c j") 'mc/insert-numbers)
  (global-set-key (kbd "C-c C-j") 'mc/insert-letters)

  )

(defun set-dired-keys ()
  (interactive)
  (define-key dired-mode-map "\C-o" 'other-window)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  )

(defun set-compilation-keys ()
  (interactive)
  (define-key compilation-mode-map "\C-o" 'other-window)
  )

(set-my-keys)

;; dired mode masks some keys, so make sure that when dired mode and shell loads, 
;; we redefine them.
(add-hook 'dired-hook 'set-my-keys)
(add-hook 'dired-mode-hook 'set-dired-keys)
(add-hook 'shell-hook 'set-my-keys)
(add-hook 'compilation-mode-hook 'set-my-keys)
(add-hook 'compilation-mode-hook 'set-compilation-keys)


(defun set-latex-keys ()
  (interactive)
  (setq-local enable-local-variables :all)
  (define-key latex-mode-map "\C-c\C-m" 'compile)
  (setq-local compilation-read-command nil)
  (visual-line-mode)
  )



;;;;;;;;;;;;;;;
;; RUBY MODE ;;
;;;;;;;;;;;;;;;
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.ux$" . html-mode))

;;;;;;;;;;;;;;;;
;; CROSSHAIRS ;;
;;;;;;;;;;;;;;;;

(load-library "crosshairs")

;;;;;;;;;;;;;;;;;;
;; HASKELL MODE ;;
;;;;;;;;;;;;;;;;;;

(load-library "haskell-site-file")
(defun haskell-mode-keys ()
  (interactive)
  (load-library "haskell-mode")
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  
  (define-key haskell-mode-map "\C-co" 'haskell-hoogle)
  (setq haskell-hoogle-command "hoogle")
  (define-key haskell-mode-map "\C-cy" 'haskell-hayoo)
  (setq haskell-hayoo-command "hayoo")
  (define-key haskell-mode-map "\C-ch" 'inferior-haskell-find-haddock)
  (define-key haskell-mode-map "\C-cd" 'inferior-haskell-find-definition)
  )

(add-hook 'haskell-mode-hook 'haskell-mode-keys)

(defun reload-haskell()
  (interactive)
  (defvar hsk "*haskell*")
  (setf same-window-buffer-names (add-to-list 'same-window-buffer-names hsk))
  (switch-to-buffer hsk)
  (kill-buffer hsk)
  (inferior-haskell-load-file)
  (switch-to-buffer hsk)
  (setq same-window-buffer-names (delq hsk same-window-buffer-names))
 )

;;;;;;;;;;;;;;;;;
;; PUPPET MODE ;;
;;;;;;;;;;;;;;;;;

(autoload 'puppet-mode "puppet-mode" "Major mode for puppet files" t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;;;;;;;;;;;;;;;
;; LATEX MODE ;;
;;;;;;;;;;;;;;;;

(defun compile-TEX ()
    (interactive)
    (save-buffer)
    (tex-buffer)
    (tex-view)
)

(defun set-TEX-key ()
  (interactive)
  (flyspell-mode)
)

(add-hook 'latex-mode-hook 'set-TEX-key)

(defun my-c-style () 
  (interactive)

  (hs-minor-mode)

  (define-key hs-minor-mode-map (kbd "C-c h") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c s") 'hs-show-block)

  (c-set-style "WebKit")
  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'brace-list-open '+)
)

(add-hook 'c++-mode-common-hook 'my-c-style)
(add-hook 'c-mode-common-hook 'my-c-style)
(add-hook 'perl-mode-common-hook 'my-c-style)

(defun set-spaces () 
  (interactive)
  (setq indent-tabs-mode nil)
)

(add-hook 'change-log-mode-hook 'set-spaces)


(defun pbcopy ()
  (interactive)
  (call-process-region (region-beginning) (region-end) "pbcopy")
)

(global-set-key (kbd "M-c") 'pbcopy)

(when (> emacs-major-version 22)

  (require 'winring)
  (require 'package)

  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")
               ;; '("melpa" . "http://melpa.milkbox.net/packages/")
               t)
  
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")) t)


  (package-initialize)
  
  (setq package-list '(multiple-cursors phi-search))
  (dolist (package package-list)
    (progn (unless (package-installed-p package)
             (progn (unless package-archive-contents
                      (package-refresh-contents))
                    (package-install package)))
           (require package)
    ))

 
)



(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)



(defun my-hamlet-mode ()
  (interactive)
  (load-library "haskell-simple-indent")
  (turn-on-haskell-simple-indent)  
  )

(add-hook 'shakespeare-hamlet-mode-hook 'my-hamlet-mode)

(defun prev-frame ()
  (interactive)
  (other-frame -1)
)

(defun make-named-frame (name)
  (interactive "sFrame name: ")
  (make-frame-command)
  (if (and (stringp name) (not (equal name "")))
      (set-frame-name name))
)

(global-set-key "\C-c(" 'prev-frame)
(global-set-key "\C-c)" 'other-frame)
(global-set-key "\C-cn" 'make-named-frame)
(global-set-key "\C-cs" 'select-frame-by-name)
(global-set-key "\C-ck" 'delete-frame)


(setq tramp-default-method "ssh")


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


(add-hook 'latex-mode-hook 'remove-dos-eol)

(electric-indent-mode 0)

(add-hook 'latex-mode-hook 'set-latex-keys)
(add-hook 'tex-mode-hook 'set-latex-keys)



(setq twelf-root "/Applications/Twelf/")
(load (concat twelf-root "emacs/twelf-init.el"))
(add-hook 'twelf-mode-hook 'twelf-font-fontify-buffer)



(setq default-input-method "TeX")
(global-set-key (kbd "C-\\") 'toggle-input-method)
