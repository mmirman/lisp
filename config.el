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
 version-control t)       ; use versioned backups




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

(defun set-my-keys () 
  (interactive)
  (windmove-default-keybindings)
  (global-set-key (kbd "C-x \\") 'goto-line )
  (global-set-key "\C-xo" 'prev-window )
  (global-set-key "\C-u" 'prev-window )
  (global-set-key "\C-o" 'other-window )

  (global-set-key "\C-cr" 'rename-buffer)
  (global-set-key (kbd "C-x %") 'shrink-window)
  (global-set-key "\C-c\C-r" 'rename-buffer)
  (global-set-key "\C-c\C-k" 'global-set-key)
  (global-set-key "\C-c\C-k" 'global-set-key)
  
  (global-set-key (kbd "C-x a r") 'align-regexp)
  )

(defun set-dired-keys ()
  (interactive)
  (define-key dired-mode-map "\C-o" 'other-window)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  )

(set-my-keys)

;; dired mode masks some keys, so make sure that when dired mode and shell loads, 
;; we redefine them.
(add-hook 'dired-hook 'set-my-keys)
(add-hook 'dired-mode-hook 'set-dired-keys)
(add-hook 'shell-hook 'set-my-keys)

;;;;;;;;;;;;;;;
;; RUBY MODE ;;
;;;;;;;;;;;;;;;
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

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





(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "find %s -name \"*.[(cpp)ch]\" -print | xargs /usr/local/bin/ctags -f %s/TAGS -a -e"  ;; need ctags 5.8 at least
			 (directory-file-name dir-name)
			 (directory-file-name dir-name)))
	)

(defun create-browse (dir-name)
    "Create browse file."
    (interactive "DDirectory: ")
    (shell-command
     (format "find %s -name \"*.[(cpp)ch]\" -print | xargs ebrowse -a -o %s/BROWSE" 
			 (directory-file-name dir-name)
			 (directory-file-name dir-name)))
	)

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
		ad-do-it
      (error (and (buffer-modified-p)
				  (not (ding))
				  (y-or-n-p "Buffer is modified, save it? ")
				  (save-buffer))
			 (er-refresh-etags extension)
			 ad-do-it))))


(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently          
    (visit-tags-table default-directory nil)))

(defun parent-directory (dir)
  (unless (equal "/" dir)
	(file-name-directory (directory-file-name dir))))
  
(defun find-file-in-heirarchy (current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR" 
  (let ((file (concat current-dir fname))
		(parent (parent-directory (expand-file-name current-dir))))
	(if (file-exists-p file)
		file
	  (when parent
		(find-file-in-heirarchy parent fname)))))

(defun get-project-root () 
  "Finds the root of the project by searching for BROWSE"
  (directory-file-name (file-name-directory (find-file-in-heirarchy default-directory "BROWSE")))
  )

(defun load-ebrowse ()
  (interactive)

  (defvar ebrowse-tag-file (get-project-root))
  
  (create-tags ebrowse-tag-file)
  (when (file-exists-p ebrowse-tag-file)
    (find-file-noselect (format "%s/BROWSE" ebrowse-tag-file) 1)
	)

  (local-set-key "\C-c>" 'ebrowse-tags-complete-symbol)
  (local-set-key "\C-c?" 'ebrowse-tags-find-declaration)
  (local-set-key "\C-c." 'ebrowse-tags-find-declaration-other-window)

)

(add-hook 'c++-mode-common-hook 'load-ebrowse)
(add-hook 'c-mode-common-hook 'load-ebrowse)


(defun my-c-lineup-arglist (langelem)
  (save-excursion
        (goto-char (c-langelem-pos langelem))
        (vector (+ (current-column) 4))
        )
  )



(c-add-style "WebKit"
              '("stroustrup"
                (c-offsets-alist
                 (innamespace . -)
                 (inline-open . 0)
                 (inher-cont . c-lineup-multi-inher)
                 (arglist-cont-nonempty . my-c-lineup-arglist)
                 (template-args-cont . +))))
(defun my-c-style () 
  (interactive)
  (c-set-style "WebKit")
  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'brace-list-open '+)
)

(add-hook 'c++-mode-common-hook 'my-c-style)
(add-hook 'c-mode-common-hook 'my-c-style)

(defun set-spaces () 
  (interactive)
  (setq indent-tabs-mode nil)
)

(add-hook 'change-log-mode-hook 'set-spaces)


(defun pbcopy ()
  (interactive)
  (call-process-region (region-beginning) (region-end) "pbcopy")
)

(global-set-key "\M-c" 'pbcopy)


(require 'winring)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
