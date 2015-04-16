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
    auto-install
    helm
    popwin
    elscreen
    elscreen-persist
    color-theme-solarized
    open-junk-file
    lispxmp
    smartparens
    auto-async-byte-compile
    package
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
