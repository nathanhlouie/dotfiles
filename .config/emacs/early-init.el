(setq site-run-file nil
      inhibit-default-init t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      package-enable-at-startup nil)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

(menu-bar-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(add-hook 'after-init-hook
	  #'(lambda () (setq gc-cons-threshold (* 8 1024 1024)
			     gc-cons-percentage 0.1)))
