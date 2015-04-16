;;==============================
;; 基本設定
;;==============================

;; beepを消す
(setq visible-bell t)

;; C-h -> Delete
(global-set-key (kbd "C-h") 'delete-backward-char)

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; 行番号の表示設定
(global-linum-mode 0)
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

;; system-type 判定
;; from http://d.hatena.ne.jp/tomoya/20090807/1249601308
(setq darwin-p (eq system-type 'darwin)
      linux-p  (eq system-type 'gnu/linux)
      carbon-p (eq system-type 'mac)
      meadow-p (featurep 'meadow))

;; Emacs と Mac のクリップボード共有
;; from http://hakurei-shain.blogspot.com/2010/05/mac.html
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(if (or darwin-p carbon-p)
    (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;;改行と同時にインデントも行う
(global-set-key (kbd "C-m") 'newline-and-indent)

;; find-functionをキー割り当てする
(find-function-setup-keys)
