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

(defun kindly-clj--content-hash (overlay)
  (secure-hash 'sha512 (overlay-buffer overlay) (overlay-start overlay) (overlay-end overlay)))

(defun kindly-clj--overlay-modified? (overlay)
  "Return true if content of OVERLAY modified since last generation time."
  (not (string= (kindly-clj--content-hash overlay)
                (overlay-get overlay 'kindly-clj--last-eval-hash))))

(defun kindly-clj--mark-generated (overlay)
  "Sets the hash of content of OVERLAY at generation time."
  (overlay-put overlay 'kindly-clj--last-eval-hash (kindly-clj--content-hash overlay))
  (overlay-put overlay 'before-string nil))

(defun kindly-clj--update-stale-marker (overlay)
  "Update visual indicator that OVERLAY is stale."
  (overlay-put overlay 'before-string (when (kindly-clj--overlay-modified? overlay)
                                        (propertize
                                         "\n"
                                         'face 'kindly-clj-stale-visualisation-face))))

(defun kindly-clj--make-overlay (bounds result-string)
  "Create an overlay around BOUNDS displaying RESULT-STRING"
  (let ((overlay (apply #'make-overlay bounds)))
    (overlay-put overlay 'kindly-clj t)
    ;; remove when text deleted...
    (kindly-clj--overlay-update-result overlay result-string)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks (list #'kindly-clj--overlay-modification-hooks))
    (overlay-put overlay 'face 'kindly-clj-highlight-face)
    (overlay-put overlay 'keymap kindly-clj-overlay-keymap)
    (kindly-clj--update-hash overlay)))

(defface kindly-clj-highlight-face
  '((((class color) (background light))
     :background "#eeeee0o"
     :extend t)
    (((class color) (background dark))
     :background "#eee9e9"
     :extend t))
  "Face for previewed forms"
  :package-version '(kindly-clj . "0.0.1"))

(defface kindly-clj-stale-visualisation-face
  '((((class color) (background light))
     :height 1
     :background "orange"
     :extend t)
    (((class color) (background dark))
     :height 1
     :background "orange"
     :extend t))
  "Face for stale visualisation marker forms"
  :package-version '(kindly-clj . "0.0.1"))

(defun kindly-clj-overlay-eval-bounds (overlay)
  (let* ((start (overlay-start overlay))
         (end (overlay-end overlay)))
    (when (and start end)
      (list start end))))

(defun kindly-clj-delete-all-at-point (&optional point)
  (interactive)
  (mapcar #'delete-overlay (kindly-clj-overlays point)))

(defun kindly-clj--overlay-modification-hooks (overlay after? &rest _args)
  (when after?
    (kindly-clj--update-stale-marker overlay)))

(defun kindly-clj--overlay-update-result (overlay result)
  (overlay-put overlay 'after-string (propertize
                                      result
                                      'face 'kindly-clj-highlight-face))
  (kindly-clj--mark-generated overlay))

(defun kindly-clj-regenerate-at-point (&optional point)
  "Regenerate the visualisation form at point"
  (interactive)
  (let ((overlay (kindly-clj-overlay (or point (point)))))
    (when-let (bounds (kindly-clj-overlay-eval-bounds overlay))
      (kindly-clj--overlay-update-result overlay "...")
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
  "Returns visualisation string for the Clojure form within the specified BOUNDS of current buffer.

This function uses CIDER to evaluate the Clojure form(s). The result is processed and displayed based on its type.

Parameters:
  - BOUNDS: A list representing the start and end positions of the buffer region
    to evaluate.

Returns:
  - For SVG source: Returns a formatted string with SVG source applied as a display property.
  - For plain text: Returns a formatted string containing the evaluated text.
  - Raises an error if the evaluation result is empty or not in the expected format.

Example Usage:
  (kindly-clj--eval-result-string (list start-pos end-pos))"
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
     ((eq type :svg-src) (kindly-clj--insert-svg-src content))
     ((eq type :text) (format "\n%s\n" content)))))

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
  "Return a new BOUNDS which is expanded to contain any whitespace before line start and trailing newlines (unless already included)"
  (-let (((start end) bounds))
    (list (kindly-clj--beginning-of-line-if-empty start)
          (kindly-clj--next-line-if-last-char end))))

(defun kindly-clj--eval-overlay-at-bounds (bounds)
  (kindly-clj--make-overlay bounds
                            (kindly-clj--eval-result-string bounds)))

(defun kindly-clj-visualise-last-sexp ()
  "Visualise the last sexp with kindly-clj"
  (interactive)
  (kindly-clj--eval-overlay-at-bounds (kindly-clj--expand-bounds (cider-last-sexp 'bounds))))

(defun kindly-clj-visualise-region ()
  "Visualise the current region with kindly-clj"
  (interactive)
  (-let ((((start . end)) (region-bounds))) ;; region-bounds is cons cell, cider bounds is a list
    (kindly-clj--eval-overlay-at-bounds (kindly-clj--expand-bounds (list start end)))))

(defun kindly-clj-visualise-defun-at-point ()
  "Visualise the top level defun at point with kindly-clj"
  (interactive)
  (kindly-clj--eval-overlay-at-bounds (kindly-clj--expand-bounds (cider-defun-at-point 'bounds))))
