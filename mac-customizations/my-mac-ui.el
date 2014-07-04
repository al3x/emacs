; nice font
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Deja-Vu-Sans-Mono-16"))

; fontify this and all future frames
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)
