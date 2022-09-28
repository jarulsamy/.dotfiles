;;; ~/.dotfiles/.doom.d/libs/compUtils.el -*- lexical-binding: t; -*-

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,close the *compilation* buffer
If the compilation is successful,and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn (delete-windows-on buffer)))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame))
