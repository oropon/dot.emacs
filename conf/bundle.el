;;==============================
;; Package Control
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
;(package-refresh-contents)

;; Packages to be installed
(defvar my/favorite-packages
  '(
    popwin
    helm
    color-theme-solarized
    ))

;; install packages in my/favorite-packages which hasnt been installed yet
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; How to upgrade installed package
;; 1. package-list-packages
;; 2. press U, x
