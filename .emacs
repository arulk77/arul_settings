;;-----------------------------------------------------------
;; General settings for the emacs
;;-----------------------------------------------------------
;; For server mode operation
(setq server-use-tcp t)

;; Tab mode
(setq default-tab-width 3)
(setq-default indent-tabs-mode nil)

(windmove-default-keybindings 'meta)
;(global-set-key "\M-;" 'other-frame)
(global-set-key (kbd "S-<right>") 'other-frame)

;; Treat _ as a word
(modify-syntax-entry ?_ "w")

;; Add cuda to c mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;;-----------------------------------------------------------
;; Remove unwanted files
;;-----------------------------------------------------------
;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)


;;-----------------------------------------------------------
;; This portion is to load the matlab code
;;-----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/matlab-emacs")
(load-library "matlab-load")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
 '(spice-output-local "Gnucap")
 '(spice-simulator "Gnucap")
 '(spice-waveform-viewer "Gwave"))

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
;(color-theme-matrix)

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
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
