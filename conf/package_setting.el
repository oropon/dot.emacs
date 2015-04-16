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
;;---------
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
