(setq site-run-file nil)
(setq inhibit-default-init t)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(setq package-enable-at-startup nil)

(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024)
                             gc-cons-percentage 0.1)))
