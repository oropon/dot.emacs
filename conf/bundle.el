;;==============================
;; package.el
;;==============================
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
(package-refresh-contents)

;; Packages to be installed
(defvar my/favorite-packages
  '(
    ;;----------
    ;; 一般
    ;;----------

    ;; パッケージ管理
    auto-install
    el-get
    package

    ;; helm
    helm

    ;; セッション・ウィンドウ
    popwin
    elscreen
    elscreen-persist

    ;; 括弧
    smartparens

    ;; 選択
    multiple-cursors
    expand-region

    ;; undo/redo
    undo-tree

    ;; project
    projectile
    helm-projectile

    ;;----------
    ;; 入力
    ;;----------
    smart-newline

    ;;----------
    ;; 検索/置換
    ;;----------

    ;; 日本語インクリメンタルサーチ
    migemo

    ;; el-get で color-moccur と moccur-edit をinstall

    ;; 検索結果編集
    wgrep
    wdired

    ;; ag
    ag
    helm-ag

    ;;----------
    ;; 表示
    ;;----------

    ;; テーマ
    color-theme-solarized

    ;;----------
    ;; 補完
    ;;----------

    auto-complete

    ;;----------
    ;; 言語
    ;;----------

    ;; 構文チェック
    flycheck

    ;; Ruby
    enh-ruby-mode
    robe

    ;; markdown
    markdown-mode

    ;; Haskell
    haskell-mode

    ;; elisp
    lispxmp
    auto-async-byte-compile

    ;; JavaScript
    js2-mode

    ;;----------
    ;; git
    ;;----------
    magit

    ;;----------
    ;; misc
    ;;----------
    open-junk-file
    ))

;; install packages in my/favorite-packages which hasnt been installed yet
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; How to upgrade installed package
;; 1. package-list-packages
;; 2. press U, x


;;==============================
;; auto-install.el
;;==============================
;; auto-installはpackage.elで入れている
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;==============================
;; el-get.el
;;==============================
;; auto-installel-getはpackage.elで入れている
;; refs: http://emacs-jp.github.io/packages/package-management/package-el.html

(setq el-get-dir "~/.emacs.d/elisp/el-get/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; define el-get repository
(setq el-get-sources
      '(
        (:name color-moccur
               :website "https://github.com/myuhe/color-moccur.el"
               :description "multi-buffer occur (grep) mode"
               :type github
               :pkgname "myuhe/color-moccur.el")
        (:name moccur-edit
               :website "https://github.com/myuhe/moccur-edit.el"
               :description "apply replaces to multiple files"
               :type github
               :pkgname "myuhe/moccur-edit.el"
               :depends (color-moccur))
        ))

;; Packages to install from el-get
(defvar my/el-get-packages
  '(
    ;; 検索/置換
    color-moccur
    moccur-edit
    )
  "A list of packages to install from el-get at launch.")
(el-get 'sync my/el-get-packages)
