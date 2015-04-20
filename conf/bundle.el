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
    package

    ;; helm
    helm

    ;; セッション・ウィンドウ
    popwin
    elscreen

    ;; 括弧
    smartparens

    ;; 選択
    multiple-cursors
    expand-region

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

    ;; Haskell
    haskell-mode

    ;; elisp
    lispxmp
    auto-async-byte-compile

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
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
