;;==============================
;; Package Settings
;;==============================

;----------
; helm
;----------
;; refs: https://github.com/emacs-helm/helm
;; key-bindings: http://d.hatena.ne.jp/a_bicky/20140104/1388822688
(helm-mode 1)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-mini)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;;----------
;; popwin
;;----------
;; refs: http://d.hatena.ne.jp/m2ym/20110120/1295524932
;;       https://github.com/m2ym/popwin-el
(require 'popwin)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
;; refs: http://sleepboy-zzz.blogspot.jp/2012/09/anythinghelm.html
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)
(push '("^\*Helm\s.+\*$" :regexp t) popwin:special-display-config)

;;----------
;; solarized
;;----------
;; refs: https://github.com/sellout/emacs-color-theme-solarized
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

(custom-theme-set-faces
 'solarized
 '(font-lock-comment-face ((t (:foreground "#b58900")))) ; Comment
 '(font-lock-doc-face ((t (:foreground "#b58900")))) ; Comment
 '(font-lock-comment-delimiter-face ((t (:foreground "#b58900")))) ; Comment
 '(elscreen-tab-current-screen-face ((t (:foreground "#eee8d5"))))
 '(elscreen-tab-background-face ((t (:foreground "#93a1a1"))))
 '(elscreen-tab-other-screen-face ((t (:foreground "#93a1a1")))))

(set-face-attribute 'whitespace-trailing nil
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :foreground "LightSkyBlue"
                    :underline t)

;;----------
;; projectile
;;----------
(projectile-global-mode)

;;----------
;; elscreen
;;----------
(elscreen-start)
;;; タブを標準で非表示
(setq elscreen-display-tab t)        ; C-z T で toggle
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)

;; elscreen-persist
(elscreen-persist-mode 1)

(require 'elscreen-dired)
(require 'elscreen-server)

;;----------
;; open-junk-file
;;----------
;;;試行錯誤用ファイルを開くための設定
;; C-x C-zで試行錯誤ファイルを開く
(require 'open-junk-file)
(global-set-key (kbd "C-x C-z") 'open-junk-file)

;;----------
;; lispxmp
;;----------
;;;式の評価結果を注釈するための設定
;; emacs-lisp-modeでC-c C-dを押すと注釈される
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;;----------
;; smartparens
;;----------
(smartparens-global-mode)
(show-smartparens-global-mode)

;;----------
;; auto-async-byte-compile
;;----------
;;自動バイトコンパイルを無効にするファイル名の正規表現
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)           ;すぐに表示したい
(setq eldoc-minor-mode-string "")     ;モードラインにElDocと表示しない

;;----------
;; expand-region
;;----------
;; refs: http://qiita.com/ongaeshi/items/abd1016bf484c4e05ab1
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-=") 'er/contract-region)

;;----------
;; multiple-cursors
;;----------
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;----------
;; auto-complete
;;----------
(global-auto-complete-mode 1)
(setq ac-delay 0.1)                     ;補完表示までのディレイ
(setq ac-use-menu-map t)                ;補完メニュー内でC-n/C-p

;;----------
;; Ruby
;;----------

;; ruby-block, ruby-end は smartparens-ruby で代用

;;----------
;;enh-ruby-mode
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-deep-indent-paren nil)

;;----------
;;robe
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;;----------
;;smartparens-ruby
(require 'smartparens-ruby)

;;----------
;;flycheck
(add-hook 'enh-ruby-mode-hook 'flycheck-mode)

;;----------
;;infruby
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-eval-binding "Pry.toplevel_binding")
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on) ;riなどのエスケープシーケンスを処理し、色付けする


;;----------
;; Haskell
;;----------

;;----------
;;haskell-mode
;;(autoload 'haskell-mode "haskell-mode" nil t)
;(autoload 'haskell-cabal "haskell-cabal" nil t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

;;----------
;; ghc-mod
(add-to-list 'exec-path (concat (getenv "HOME") "/.dotfiles/emacs/haskell/.cabal-sandbox/bin"))
(add-to-list 'load-path (concat (getenv "HOME") "/.dotfiles/emacs/haskell/.cabal-sandbox/share/x86_64-osx-ghc-7.8.2/ghc-mod-4.1.1"))   ;office
(add-to-list 'load-path (concat (getenv "HOME") "/.dotfiles/emacs/haskell/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/ghc-mod-5.2.1.1")) ;home
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

;;----------
;; minor-mode
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook (lambda () (global-set-key (kbd "C-c t") 'ghc-show-type)))
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(custom-set-variables
 '(haskell-process-type 'cabal-repl))

;;----------
;; magit
;;----------
;; surpress warning
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)

;;----------
;; undo-tree
;;----------
(global-undo-tree-mode)

;;----------
;; smart-newline
;;----------
(global-set-key (kbd "C-m") 'smart-newline)
(add-hook 'enh-ruby-mode-hook 'smart-newline-mode)
(add-hook 'haskell-mode-hook 'smart-newline-mode)
(add-hook 'emacs-lisp-mode-hook 'smart-newline-mode)


;;----------
;; Search
;;----------

;;----------
;; helm-ag
(setq helm-ag-insert-at-point 'symbol)  ; 現在のシンボルをデフォルトのクエリにする
(global-set-key (kbd "C-M-g") 'helm-ag)
(global-set-key (kbd "C-M-k") 'backward-kill-sexp) ; 他のクエリに変更

;; projectileと連携
;; cf.) http://rubikitch.com/tag/helm-ag/
(defun helm-projectile-ag ()
  "Projectileと連携"
  (interactive)
  (helm-ag (projectile-project-root)))

;;----------
;; helm-projectile
(global-set-key (kbd "M-t") 'helm-projectile)
;; TODO: helm-projectile-* で便利なものがあればM-tからのコンビネーションで設定
;; M-t t でhelm-projectileとか

;;---------
;; color-moccur
(global-set-key (kbd "M-o") 'occur-by-moccur)

;;----------
;; migemo
(add-to-list 'exec-path "/usr/local/bin")
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;;---------
;; wdired
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;---------
;; server
;;---------

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;---------
;; markdown
;;---------
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'whitespace-action) nil)))

;; ファイル内容を標準入力で渡すのではなく、ファイル名を引数として渡すように設定
(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command-needs-filename t)
  )
(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))
