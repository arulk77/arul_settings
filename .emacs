;;-----------------------------------------------------------
;; General settings for the emacs
;;-----------------------------------------------------------
(setq default-tab-width 3)
(windmove-default-keybindings 'meta)

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
(add-hook 'verilog-mode-hook
      '(lambda ()
         (setq verilog-auto-newline nil)
;         (setq verilog-tab-always-indent nil)
      ))

;;-----------------------------------------------------------
;; This section is for cscope mode 
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/xcscope")
(require 'xcscope)
(cscope-setup)

;;-----------------------------------------------------------
;; This portion is for the emacs goodies
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/emacs-goodies")

;;-----> All package
(require 'all)

;;-----> color theme 
(require 'color-theme)   
(color-theme-initialize)
(color-theme-matrix)

;;-----> Shell command
(require 'shell-command)

;;-----> highlight symbol 
(require 'highlight-symbol)

;;-----> Tab bar 
(require 'tabbar)
;(tabbar-mode t)
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

(global-set-key (kbd "M-s <left>") 'tabbar-backward)
(global-set-key (kbd "M-s <right>") 'tabbar-forward)

;;-----------------------------------------------------------
;; Locally defined function
;;-----------------------------------------------------------
(defun my-set-frame-name ()
"Prompt the user for a window title, and set the current
frame's title to that string."
(interactive)
(let ((title (read-string "Enter window title: " "emacs (")))
(if (string-match "\\`emacs ([^)]+\\'" title) ; no trailing close-paren
(setq title (concat title ")")))
(set-frame-name title)
))