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
  (let* ((metadata (overlay-get overlay 'kindly-clj-metadata))
         (start (marker-position (plist-get metadata 'start-form)))
         (end (marker-position (plist-get metadata 'end-form))))
    (when (and start end)
      (list start end))))

(defun kindly-clj-overlay-gen-bounds (overlay)
  (let* ((metadata (overlay-get overlay 'kindly-clj-metadata))
         (start (marker-position (plist-get metadata 'start-gen)))
         (end (marker-position (plist-get metadata 'end-gen))))
    (when (and start end)
      (list start end))))

(defun kindly-clj--delete-overlay-content (overlay)
  (save-excursion
    (-let (((start end) (kindly-clj-overlay-gen-bounds overlay)))
      (when (and start end)
        (delete-region start end)
        (plist-put metadata 'end-gen nil)
        (plist-put metadata 'start-gen nil)))))

(defun kindly-clj--delete-overlay-and-content (overlay)
  (kindly-clj--delete-overlay-content overlay)
  (delete-overlay overlay))

(defun kindly-clj-delete-all-at-point (&optional point)
  (interactive)
  (mapcar #'kindly-clj--delete-overlay-and-content (kindly-clj-overlays point)))

(defun kindly-clj-regenerate-at-point (&optional point)
  (interactive)
  (let ((overlay (kindly-clj-overlay (or point (point)))))
    (when-let (bounds (kindly-clj-overlay-eval-bounds overlay))
      (kindly-clj--delete-overlay-and-content overlay)
      (kindly-clj-visualise-bounds bounds))))

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

(defun kindly-clj--make-overlay (start end metadata)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'kindly-clj t)
    ;; remove when text deleted...
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'kindly-clj-metadata metadata)
    (overlay-put overlay 'face 'kindly-clj-highlight-face)
    (overlay-put overlay 'keymap kindly-clj-overlay-keymap)))

(defun kindly-clj--point-marker (point)
  (set-marker (make-marker) point))

(defun kindly-clj-visualise-bounds (bounds)
  (let* ((form  (apply #'buffer-substring-no-properties bounds))
         (bg-color (face-attribute 'kindly-clj-highlight-face :background))
         (resp (cider-nrepl-sync-request:eval
                (format "(%s (do %s) \"%s\")" kindly-clj-fn form bg-color)
                nil
                (cider-current-ns)))
         (start (car-safe bounds))
         (end   (car-safe (cdr-safe bounds)))
         (val (nrepl-dict-get resp "value"))
         (_ (when (not val)
              (user-error (prin1-to-string resp))))
         (parsed-val (parseedn-read-str val))
         (type (gethash :kindly-emacs.kindly-emacs/type parsed-val))
         (content (gethash :content parsed-val)))
    (save-excursion
      ;; TODO: Could use the after-string property of the overlay here to make this save-able...!
      ;; Also, I could then store the generated content in the overlay and re-insert after a save.
      (goto-char end)
      (insert
       (cond
        ((eq type :svg-src) (kindly-clj--insert-svg-src content))))
      (kindly-clj--make-overlay start (point)
                                `(type ,type
                                       start-form ,(kindly-clj--point-marker start)
                                       end-form ,(kindly-clj--point-marker end)
                                       start-gen ,(kindly-clj--point-marker end)
                                       end-gen ,(kindly-clj--point-marker (point)))))))

;; TODO - when I delete the form and write a new one, the new one ends up inside the markers!!
;; Mayb when I delete the form, I just auto-delete the lot...?

(defun kindly-clj-visualise-last-sexp ()
  (interactive)
  (kindly-clj-visualise-bounds (cider-last-sexp 'bounds)))

(defun kindly-clj-visualise-region ()
  (interactive)
  (-let ((((start . end)) (region-bounds)))
    (kindly-clj-visualise-bounds (list start end))))

(defun kindly-clj-defun-at-point ()
  (interactive)
  (kindly-clj-visualise-form (cider-defun-at-point 'bounds)))
