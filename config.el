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


(require 'shell)

(defun shell-get-buffer-create (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
   (prog1
       (read-buffer "Shell buffer: "
        ;; If the current buffer is an inactive
        ;; shell buffer, use it as the default.
        (if (and (eq major-mode 'shell-mode)
           (null (get-buffer-process (current-buffer))))
            (buffer-name)
          (generate-new-buffer-name "*shell*")))
     (if (file-remote-p default-directory)
         ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
         (setq default-directory
         (expand-file-name
          (read-directory-name
           "Default directory: " default-directory default-directory
           t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
  (if (and (called-interactively-p 'any)
     (file-remote-p default-directory)
     (null explicit-shell-file-name)
     (null (getenv "ESHELL")))
      (with-current-buffer buffer
  (set (make-local-variable 'explicit-shell-file-name)
       (file-remote-p
        (expand-file-name
         (read-file-name
    "Remote shell path: " default-directory shell-file-name
    t shell-file-name))
        'localname))))

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
  (with-current-buffer buffer
    (unless (comint-check-proc buffer)
      (let* ((prog (or explicit-shell-file-name
           (getenv "ESHELL") shell-file-name))
       (name (file-name-nondirectory prog))
       (startfile (concat "~/.emacs_" name))
       (xargs-name (intern-soft (concat "explicit-" name "-args"))))
        (unless (file-exists-p startfile)
    (setq startfile (concat user-emacs-directory "init_" name ".sh")))
        (apply 'make-comint-in-buffer "shell" buffer prog
         (if (file-exists-p startfile) startfile)
         (if (and xargs-name (boundp xargs-name))
       (symbol-value xargs-name)
           '("-i")))
        (shell-mode))))
  buffer)


(defun open-config ()
  (interactive)
  (find-file "~/.emacs.d/lisp/config.el")
  (switch-to-buffer "config.el")
)


(defun shell ()
  (interactive)
  (switch-to-buffer (shell-get-buffer-create))
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

  (global-set-key (kbd "C-c a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c e") 'mc/edit-lines)
  (global-set-key (kbd "C-c b") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c C-b") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c d") 'set-rectangular-region-anchor) ;; draw

  (global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)


  (global-set-key (kbd "C-c C-a") 'mc/mark-all-words-like-this)
  (global-set-key (kbd "C-c M-a") 'mc/mark-all-symbols-like-this)
  (global-set-key (kbd "C-c C-s") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c M-s") 'mc/mark-all-in-region)

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

  (global-set-key (kbd "C-c C-e") 'open-config)

  (global-set-key (kbd "C-c ;") 'shell)
  (global-set-key (kbd "C-c h") 'describe-symbol)

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








