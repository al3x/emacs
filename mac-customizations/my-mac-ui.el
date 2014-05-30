; nice font
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Deja-Vu-Sans-Mono-15"))

; fontify this and all future frames
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)
