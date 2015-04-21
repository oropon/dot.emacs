;; エラー時にトレース表示
;(setq debug-on-error t)

;; 設定ファイル読み込み
(setq load-path
      (append '(
                "~/.emacs.d/conf"
                ) load-path))

(load "bundle")
(load "meltingpot")
(load "package_setting")
