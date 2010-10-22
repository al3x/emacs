; nice font
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Deja-Vu-Sans-Mono-15"))

; fontify current frame
(fontify-frame nil)

; fontify any future frames
(push 'fontify-frame after-make-frame-functions)
