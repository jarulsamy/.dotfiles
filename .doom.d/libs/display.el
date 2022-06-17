;;; ~/.dotfiles/.doom.d/libs/display.el -*- lexical-binding: t; -*-

(defun display-primary-height ()
  "Get the height of the current display this frame is on."
  (interactive)
  (nth 4 (assoc 'geometry (car (display-monitor-attributes-list)))))

(defun display-primary-width ()
  "Get the width of the current display this frame is on."
  (interactive)
  (nth 3 (assoc 'geometry (car (display-monitor-attributes-list)))))

;; Source: https://stackoverflow.com/a/94277/8846676
(defun display-set-frame-size-according-to-resolution ()
  "Automatically adjust the default frame size accoring to display resolution."
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (display-primary-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 400)
                                      (frame-char-height)))))))
