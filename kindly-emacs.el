(require 'parseedn)

;; TODO - mark regions that are generated, and add an pre-save hook to set them to invisible whilst saving...?

(defvar kindly-clj-fn "kindly-emacs.kindly-emacs/kindly-plot")

(defun kindly-clj-overlays (&optional pos)
  "Get kindly-clj overlays at POS."
  (let ((pos (or pos (point))))
    (->> (overlays-at (point))
         (-filter (lambda (overlay)
                    (overlay-get overlay 'kindly-clj))))))

(defun kindly-clj-overlay (&optional pos)
  (let ((o (kindly-clj-overlays pos)))
    (if (> (length o) 1)
        (user-error "Multiple (overlapping?) kindly overlays at point!")
      (car o))))

(defun kindly-clj-delete-all-overlays-at-pos (&optional pos)
  (mapcar #'delete-overlay (kindly-clj-overlays pos)))

(defface kindly-clj-highlight-face
  '((((class color) (background light))
     :background "#eeeee0o"
     :extend t)
    (((class color) (background dark))
     :background "#eee9e9"
     :extend t))
  "Face for previewed forms"
  :package-version '(kindly-clj . "0.0.1"))

(defun kindly-clj-overlay-eval-bounds (overlay)
  (let* ((start (overlay-start overlay))
         (end (overlay-end overlay)))
    (when (and start end)
      (list start end))))

(defun kindly-clj-delete-all-at-point (&optional point)
  (interactive)
  (mapcar #'delete-overlay (kindly-clj-overlays point)))

(defun kindly-clj--overlay-update-result (overlay result)
  (overlay-put overlay 'after-string (propertize
                                      result
                                      'face 'kindly-clj-highlight-face)))

(defun kindly-clj-regenerate-at-point (&optional point)
  (interactive)
  (let ((overlay (kindly-clj-overlay (or point (point)))))
    (when-let (bounds (kindly-clj-overlay-eval-bounds overlay))
      (kindly-clj--overlay-update-result overlay "Generating...")
      (kindly-clj--overlay-update-result overlay
                                         (kindly-clj--eval-result-string bounds)))))

(defun kindly-clj--image-string (image)
  (propertize " "
              'display image
              'rear-nonsticky t
              'inhibit-isearch t
              'keymap image-map))

(defun kindly-clj--insert-svg-src (svg-string)
  (format "\n\n%s\n\n"
          (kindly-clj--image-string (svg-image svg-string))))

(defvar kindly-clj-overlay-keymap
  (define-keymap
    "C-c C-c" #'kindly-clj-regenerate-at-point))

(defun kindly-clj--eval-result-string (bounds)
  (let* ((form  (apply #'buffer-substring-no-properties bounds))
         (bg-color (face-attribute 'kindly-clj-highlight-face :background))
         (resp (cider-nrepl-sync-request:eval
                (format "(%s (do %s) \"%s\")" kindly-clj-fn form bg-color)
                nil
                (cider-current-ns)))
         (val (nrepl-dict-get resp "value"))
         (_ (when (not val)
              (user-error (prin1-to-string resp))))
         (parsed-val (parseedn-read-str val))
         (type (gethash :kindly-emacs.kindly-emacs/type parsed-val))
         (content (gethash :content parsed-val)))
    (cond
     ((eq type :svg-src) (kindly-clj--insert-svg-src content)))))

(defun kindly-clj--beginning-of-line-if-empty (pos)
  "Return start of line containing POS if POS is the first non-blank character, and POS otherwise."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (if (string-match-p "^[[:blank:]]*$"
                        (buffer-substring-no-properties (point) pos))
        (point)
      pos)))

(defun kindly-clj--next-line-if-last-char (pos)
  "Return start of line containing POS if POS is the first non-blank character, and POS otherwise."
  (if (and (not (= pos (point-max)))
           (not (string= (buffer-substring-no-properties pos (+ pos 1))
                         "\n"))
           (string= (buffer-substring-no-properties (+ pos 1) (+ pos 2))
                    "\n"))
      (1+ pos)
    pos))

(defun kindly-clj--expand-bounds (bounds)
  (-let (((start end) bounds))
    (list (kindly-clj--beginning-of-line-if-empty start)
          (kindly-clj--next-line-if-last-char end))))

(defun kindly-clj--eval-overlay-at-bounds (bounds)

  (kindly-clj--make-overlay bounds
                            (kindly-clj--eval-result-string bounds)))

(defun kindly-clj-visualise-last-sexp ()
  (interactive)
  (kindly-clj--eval-overlay-at-bounds (kindly-clj--expand-bounds (cider-last-sexp 'bounds))))

(defun kindly-clj-visualise-region ()
  (interactive)
  (-let ((((start . end)) (region-bounds))) ;; region-bounds is cons cell, cider bounds is a list
    (kindly-clj--eval-overlay-at-bounds (kindly-clj--expand-bounds (list start end)))))

(defun kindly-clj-visualise-defun-at-point ()
  (interactive)
  (kindly-clj--eval-overlay-at-bounds (kindly-clj--expand-bounds (cider-defun-at-point 'bounds))))
