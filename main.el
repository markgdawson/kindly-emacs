
(defun extract-metadata-and-content (form-string)
  "Extract metadata and content from Clojure form string using vega-view--clojure."
  (with-temp-buffer
    (insert form-string)
    (let* ((vega-buffer (get-buffer-create "*Vega-View-Clojure*"))
           (value (progn
                    (vega-view--clojure (buffer-string) vega-buffer)
                    (with-current-buffer vega-buffer
                      (buffer-string)))))
      (when value
        (goto-char (point-min))
        (if (search-forward-regexp "\\^:kind/\\(.*?\\)$" nil t)
            (match-string 1)
          nil))))))

;; Example usage:
(let* ((form-string "^:kind/vega-lite
 (assoc-in {:mark \"bar\",
           :encoding
           {:x {:field \"a\", :type \"nominal\", :axis {:labelAngle 0}},
            :y {:field \"b\", :type \"quantitative\"}}}
          [:data :values]
          [{:a \"A\", :b 28}
           {:a \"B\", :b 55}])")
       (metadata (extract-metadata-and-content form-string)))
  (if metadata
      (message "Metadata: %s" metadata)
    (message "No metadata found")))

(defun visualize-vega-lite (data &optional force)
  "Visualize Vega-Lite data using vl2svg and insert it below the Clojure form.
If FORCE is non-nil, recreate the SVG file even if it exists."
  (let* ((json-data (json-encode data))
         (hash (secure-hash 'md5 json-data))
         (project-root (and (fboundp 'clojure-project-root-function)
                            (funcall clojure-project-root-function)))
         (relative-path (when buffer-file-name
                          (file-relative-name buffer-file-name project-root)))
         (cache-dir (and project-root (expand-file-name ".vis-cache" project-root)))
         (svg-file (and cache-dir
                        (expand-file-name (format "%s/%s.svg" relative-path hash) cache-dir)))
         (json-file (and cache-dir
                         (expand-file-name (format "%s/%s.json" relative-path hash) cache-dir))))
    (unless (and svg-file (file-exists-p svg-file) (not force))
      (when (and cache-dir relative-path)
        (make-directory (expand-file-name (file-name-directory svg-file) cache-dir) t)
        (with-temp-buffer
          (insert json-data)
          (write-region (point-min) (point-max) json-file)
          (call-process-shell-command (format "vl2svg < %s > %s" json-file svg-file)))))
    (insert "\n")
    (insert-image (create-image svg-file))
    svg-file))

(defun visualize-table (data)
  ;; Write code to generate and insert the table
  )

(defun visualize-clojure-form (form)
  (let* ((clj-ns-and-var-name "kindly-emacs-plotter")
         (var-name (format "%s/%s" clj-ns-and-var-name clj-ns-and-var-name))
         (_res (nrepl-dict-get (cider-nrepl-sync-request:eval
                                (format "(binding [*ns* (create-ns '%s)]
  (eval '(def %s (clojure.core/atom nil))))"
                                        clj-ns-and-var-name
                                        clj-ns-and-var-name)
                                (format "(reset! %s/%s nil)" var-name))
                               "value"))
         (content (nrepl-dict-get (cider-nrepl-sync-request:eval
                                   (format "(reset! %s %s)" var-name form))
                                  "value"))
         (metadata (nrepl-dict-get (cider-nrepl-sync-request:eval "(->> @bob/value meta (filter (fn [[k v]] (and v (= \"kind\" (namespace k))))) keys first name)")
                                   "value")))
    (cond
     ((string= metadata "vega-lite") (visualize-vega-lite content))
     ((string= metadata "table") (visualize-table content)))))

(defun visualize-clojure-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\\^:kind/\\(.*?\\)$" nil t)
      (forward-sexp)
      (let ((form (cider-last-sexp)))
        (forward-line 1)
        (visualize-clojure-form form)))))

(global-set-key (kbd "C-c C-c") 'visualize-clojure-buffer)


(with-current-buffer buffer


  (let* ((var-name "bob/value")
         (_res (nrepl-dict-get (cider-nrepl-sync-request:eval
                                (format "(reset! %s nil)" var-name))
                               "value"))
         (res (nrepl-dict-get (cider-nrepl-sync-request:eval
                               (format "(reset! %s %s)" var-name form))
                              "value"))
         (meta (nrepl-dict-get (cider-nrepl-sync-request:eval "(->> @bob/value meta (filter (fn [[k v]] (and v (= \"kind\" (namespace k))))) keys first name)")
                               "value")))
    meta))
