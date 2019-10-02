;;; org-auto-toggle -- Automatically toggle equations
;;; Commentary:

;;; Code:

(defvar org-auto-toggle/org-last-fragment nil
  "Holds the type and position of last valid fragment we were on. Format: (FRAGMENT_TYPE FRAGMENT_POINT_BEGIN)."
  )

(setq org-auto-toggle/org-valid-fragment-type
      '(latex-fragment
        latex-environment
        link))

(defun org-auto-toggle/org-curr-fragment ()
  "Return the type and position of the current fragment available for preview inside `org-mode`. Return nil at non-displayable fragments."
  (let* ((fr (org-element-context))
         (fr-type (car fr)))
    (when (memq fr-type org-auto-toggle/org-valid-fragment-type)
      (list fr-type
            (org-element-property :begin fr))))
  )

(defun org-auto-toggle/org-remove-fragment-overlay (fr)
  "Remove fragment overlay at FR."
  (org-clear-latex-preview (nth 1 fr) (+ (nth 1 fr) 5))
  )

(defun org-auto-toggle/org-preview-fragment (fr)
  "Preview org fragment at FR."
  (let ((fr-type (nth 0 fr))
        (fr-begin (nth 1 fr)))
    (goto-char fr-begin)
    (cond ((or (eq 'latex-fragment fr-type) ;; latex stuffs
               (eq 'latex-environment fr-type))
           (when (org-auto-toggle/org-curr-fragment) (org-preview-latex-fragment))) ;; only toggle preview when we're in a valid region (for inserting in the front of a fragment)


          ((eq 'link fr-type) ;; for images
           (let ((fr-end (org-element-property :end (org-element-context))))
             (org-display-inline-images nil t fr-begin fr-end))))
    ))


(defun org-auto-toggle/org-auto-toggle-fragment-display ()
  "Automatically toggle a displayable org mode fragment."
  (and (eq 'org-mode major-mode)
       (let ((curr (org-auto-toggle/org-curr-fragment)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            org-auto-toggle/org-last-fragment
            ;; and are on a fragment now
            curr
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (equal curr org-auto-toggle/org-last-fragment)))

           ;; go back to last one and put image back, provided there is still a fragment there
           (save-excursion
             (org-auto-toggle/org-preview-fragment org-auto-toggle/org-last-fragment)
             ;; now remove current image
             (org-auto-toggle/org-remove-fragment-overlay curr)
             ;; and save new fragment
             )
           (setq org-auto-toggle/org-last-fragment curr))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not curr)
            ;; but we were on one
            org-auto-toggle/org-last-fragment)
           ;; put image back on, provided that there is still a fragment here.
           (save-excursion
             (org-auto-toggle/org-preview-fragment org-auto-toggle/org-last-fragment))

           ;; unset last fragment
           (setq org-auto-toggle/org-last-fragment nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not org-auto-toggle/org-last-fragment)
            ;; but now we are
            curr)
           ;; remove image
           (save-excursion
             (org-auto-toggle/org-remove-fragment-overlay curr)
             )
           (setq org-auto-toggle/org-last-fragment curr))

          ))))
(provide 'org-auto-toggle)
;;; org-auto-toggle ends here
