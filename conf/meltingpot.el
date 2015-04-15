;==============================
; 基本設定
;==============================

;; beepを消す
(setq visible-bell t)

;; C-h -> Delete
(global-set-key (kbd "C-h") 'delete-backward-char)

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; 行番号の表示設定
(global-linum-mode 0)
(set-face-attribute 'linum nil
                    :foreground "#aaa"
                    :background "#000")
(setq linum-format "%04d ")

;; 行番号・桁番号を表示
(line-number-mode 1)
(column-number-mode 1)

;; C-u C-SPC C-SPC ...でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; モードラインに時刻を表示する
(display-time)

;; do not show..
(setq inhibit-startup-screen t) ;; startup message
(tool-bar-mode 0) ;; tool bar
(menu-bar-mode 0) ;; menu bar
(scroll-bar-mode 0) ;; scroll bar

;; do not make..
(setq make-backup-files nil) ;; backup files
(setq auto-save-default nil) ;; auto-save files

;; whitespace-mode
;; 全角空白　はRictyで可視化するため設定不要
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         empty          ; 先頭/末尾の空行
                         space-mark
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u21E5 ?\t] [?\\ ?\t])))

(defvar my/bg-color "#333")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)

(setq whitespace-action '(auto-cleanup)) ; 保存前に自動でクリーンアップ
(global-whitespace-mode 1)
(global-set-key (kbd "C-x w") 'global-whitespace-mode)

;; line
;;refs: http://www.clear-code.com/blog/2012/3/20.html
(global-hl-line-mode)
(column-number-mode t)
(line-number-mode t)

;; ログの記録行数を増やす
(setq message-log-max 10000)

;; 履歴をたくさん保存する
(setq history-length 1000)

;; tabs
(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(foreign-regexp/regexp-type (quote ruby))
 '(magit-use-overlays nil)
 '(reb-re-syntax (quote foreign-regexp))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(tab-width 2))
(setq-default tab-width 2)

;; toggle comment
(global-set-key (kbd "M-/")  'comment-dwim)

;==============================
; Package Control
;==============================
;; refs: http://emacs-jp.github.io/packages/package-management/package-el.html

;; MELPA
;; refs: http://melpa.milkbox.net/#/getting-started
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Marmalade
;; refs: http://marmalade-repo.org/
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Initialize
(package-initialize)

;; Update package list
;(package-refresh-contents)

;; Packages to be installed
(defvar my/favorite-packages
  '(
    popwin
    helm
    ))

;; install packages in my/favorite-packages which hasnt been installed yet
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; How to upgrade installed package
;; 1. package-list-packages
;; 2. press U, x


;==============================
; Package Settings
;==============================

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
;(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;----------
; popwin
;----------
;; refs: http://d.hatena.ne.jp/m2ym/20110120/1295524932
;;       https://github.com/m2ym/popwin-el
(require 'popwin)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
;; refs: http://sleepboy-zzz.blogspot.jp/2012/09/anythinghelm.html
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)
(push '("^\*Helm\s.+\*$" :regexp t) popwin:special-display-config)
