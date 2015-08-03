;;-----------------------------------------------------------
;; General settings for the emacs
;;-----------------------------------------------------------
(setq default-tab-width 3)

;;-----------------------------------------------------------
;; Color theme for the emacs
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
;(color-theme-matrix)
;(color-theme-gray30)
(color-theme-midnight)

;;-----------------------------------------------------------
;; Evil mode package
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vimmode")
(add-to-list 'load-path "~/.emacs.d/vimmode/plugins")
(require 'evil)
(require 'evil-numbers)

;;-----------------------------------------------------------
;; This portion is to load the matlab code
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/matlab-emacs")
(load-library "matlab-load")

;;-----------------------------------------------------------
;; This portion is to load the verilog mode  
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/verilog-mode")
(require 'verilog-mode)
(custom-set-variables
 '(matlab-shell-command-switches '("-nodesktop -nosplash")))
