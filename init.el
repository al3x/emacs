; env
(setq explicit-shell-file-name "/bin/bash")
(setq exec-path (append '(
                          "~/bin"
                          "/usr/local/sbin"
                          "/usr/local/bin"
                          "~/.rbenv/shims/"
                          "/sbin"
                          "/opt/local/bin"
                          "/usr/local/share/npm/bin"
                          "~/.cabal/bin"
                          "/usr/local/MacGPG2/bin"
                          )
                        exec-path))
(setenv "PATH"
        (mapconcat 'identity exec-path path-separator))
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "ALTERNATE_EDITOR" "emacs")
(setenv "PROMPT_COMMAND" "")
(setenv "GPG_AGENT_INFO" nil)
(setenv "SSH_AUTH_SOCK" (expand-file-name "~/.ssh/agent.sock"))
(setenv "PS1" "${debian_chroot:+($debian_chroot)}\\u@\\h:\\w \\$ ")

(setenv "GIT_EDITOR" "emacsclient")
(setenv "GIT_COMMITTER_NAME" "Alex Payne")
(setenv "GIT_COMMITTER_EMAIL" "al3x@al3x.net")
(setenv "GIT_AUTHOR_NAME" "Alex Payne")
(setenv "GIT_AUTHOR_EMAIL" "al3x@al3x.net")

(setq default-directory "~/src")

; add directories to the load path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/vendor")

; handy function to load all elisp files in a directory
(load-file "~/.emacs.d/utilities/load-directory.el")

; load all utility functions
(mapcar 'load-directory '("~/.emacs.d/utilities"))

; load personal customizations (keybindings, colors, etc.)
(mapcar 'load-directory '("~/.emacs.d/customizations"))

; per-OS customizations
(if (string-equal system-type "gnu/linux")
    (mapcar 'load-directory '("~/.emacs.d/linux-customizations"))
    (mapcar 'load-directory '("~/.emacs.d/mac-customizations")))

; load and run El-Get
(load-file "~/.emacs.d/el-get.el")
