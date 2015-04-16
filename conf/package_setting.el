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
(load-theme 'solarized t)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame
                                 'background-mode
                                 (if (display-graphic-p frame) 'light 'dark))
                        (enable-theme 'solarized)))

(custom-theme-set-faces
 'solarized
 '(font-lock-comment-face ((t (:foreground "#b58900")))) ; Comment
 '(font-lock-doc-face ((t (:foreground "#b58900")))) ; Comment
 '(font-lock-comment-delimiter-face ; Comment
   ((t (:foreground "#b58900")))))

;;----------
;; elscreen
;;----------
(elscreen-start)
;;; タブを標準で非表示
(setq elscreen-display-tab nil)        ; C-z T で toggle
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)

;;----------
;; open-junk-file
;;----------
;;;試行錯誤用ファイルを開くための設定
(require 'open-junk-file)
;; C-x C-zで試行錯誤ファイルを開く
(global-set-key (kbd "C-x C-z") 'open-junk-file)

;;----------
;; lispxmp
;;----------
;;;式の評価結果を注釈するための設定
(require 'lispxmp)
;; emacs-lisp-modeでC-c C-dを押すと注釈される
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;;----------
;; smartparens
;;----------
(smartparens-global-mode)
(show-smartparens-global-mode)

;;----------
;; auto-async-byte-compile
;;----------
(require 'auto-async-byte-compile)
;;自動バイトコンパイルを無効にするファイル名の正規表現
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2) ;すぐに表示したい
(setq eldoc-minor-mode-string "") ;モードラインにElDocと表示しない
