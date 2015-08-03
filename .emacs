;;-----------------------------------------------------------
;; General settings for the emacs
;;-----------------------------------------------------------
(setq default-tab-width 3)

;;-----------------------------------------------------------
;; This portion is to load the matlab code
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/matlab-emacs")
(load-library "matlab-load")
(custom-set-variables
 '(matlab-shell-command-switches '("-nodesktop -nosplash")))

;;-----------------------------------------------------------
;; Evil mode package
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vimmode")
(add-to-list 'load-path "~/.emacs.d/vimmode/plugins")
(require 'evil)
(require 'evil-numbers)

;;-----------------------------------------------------------
;; This portion is to load the verilog mode  
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/verilog-mode")
(require 'verilog-mode)

;;-----------------------------------------------------------
;; This section is for verilog mode
;;-----------------------------------------------------------
(add-hook 'verilog-mode-hook
      '(lambda ()
         (setq verilog-auto-newline nil)
;         (setq verilog-tab-always-indent nil)
      ))

;;-----------------------------------------------------------
;; This portion is for the emacs goodies
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/emacs-goodies")

;;-----> All package
(require 'all)

;;-----> color theme 
(require 'color-theme)   
(color-theme-initialize)
(color-theme-midnight)

;;-----> Shell command
(require 'shell-command)

;;-----> Tab bar 
(require 'tabbar)
(tabbar-mode t)
; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”,
;“User Buffer”.

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t
     "User Buffer"
     )
    ))) 

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;(global-set-key (kbd ) 'tabbar-backward)
;(global-set-key (kbd ) 'tabbar-forward)




