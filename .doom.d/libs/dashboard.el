;;; ../.dotfiles/.doom.d/libs/dashboard.el -*- lexical-binding: t; -*-

;;; Custom Banners
(defcustom banner-directory "~/.doom.d/banners/"
  "Directory to search for .txt files for doom dashboard banner.")

(defun dash-file-contents-to-list (filename)
  (let ((contents (with-temp-buffer
                    (insert-file-contents filename)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (split-string contents "\n")))

(defun dash-random-choice (items)
  "Pick a random item from a list."
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun dash-random-txt-file (directory)
  "Pick a random .txt file from `directory'."
  (dash-random-choice (directory-files (expand-file-name directory) 'full
                                       (rx ".txt" eos))))

(defun dashboard-random-ascii-banner ()
  (let* ((banner (dash-file-contents-to-list (dash-random-txt-file banner-directory)))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))
