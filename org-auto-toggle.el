;;; org-auto-toggle -- Automatically toggle equations
;;; Commentary:

;;; Code:
(require 'org)
(require 'org-element)

(defvar org-auto-toggle/org-last-fragment nil
  "Format: (type begin end)."
  )

(defvar org-auto-toggle/org-types
  '(
    ;; TODO: -> 1 list
    (code . ((lambda (begin) (setq org-hide-emphasis-markers nil) (org-restart-font-lock)) (lambda (begin) (setq org-hide-emphasis-markers t)  (org-restart-font-lock))))
    (bold . ((lambda (begin) (setq org-hide-emphasis-markers nil) (org-restart-font-lock)) (lambda (begin) (setq org-hide-emphasis-markers t)  (org-restart-font-lock))))
    (italic . ((lambda (begin) (setq org-hide-emphasis-markers nil) (org-restart-font-lock)) (lambda (begin) (setq org-hide-emphasis-markers t)  (org-restart-font-lock))))
    (verbatim . ((lambda (begin) (setq org-hide-emphasis-markers nil) (org-restart-font-lock)) (lambda (begin) (setq org-hide-emphasis-markers t)  (org-restart-font-lock))))
    (underline . ((lambda (begin) (setq org-hide-emphasis-markers nil) (org-restart-font-lock)) (lambda (begin) (setq org-hide-emphasis-markers t)  (org-restart-font-lock))))

    (entity . ((lambda (begin)  (if org-pretty-entities(org-toggle-pretty-entities))) (lambda (begin)  (if (not org-pretty-entities)(org-toggle-pretty-entities)))))

    (link . ((lambda (begin)  (org-toggle-link-display) (org-remove-inline-images)) (lambda (begin)  (org-toggle-link-display)(org-display-inline-images))))
    ;; (latex-fragment . ((lambda (begin) (org-preview-latex-fragment)) (lambda (begin) (org-clear-latex-preview begin (+ begin 5)))))
    (latex-fragment . ( (lambda (begin) (org-clear-latex-preview begin (+ begin 5)))(lambda (begin) (org-latex-preview))))
    (latex-environment . ( (lambda (begin) (org-clear-latex-preview begin (+ begin 5)))(lambda (begin) (org-latex-preview))))
    )
  "(type . (remove function) (show function))."
  )

(defun org-auto-toggle/org-curr-fragment ()
  "Return the type and position of the current fragment.
Return nil at non-displayable fragments."
  (let* ((fr (org-element-context))
         (fr-type (car fr)))
    (when (not (eq (assoc (nth 0 fr) org-auto-toggle/org-types) nil))
      (list fr-type
            (org-element-property :begin fr)(org-element-property :end fr))))
  )

(defun org-auto-toggle/org-remove-fragment-overlay (fr)
  "Remove fragment overlay at FR."
  (let ((type (assoc (nth 0 fr) org-auto-toggle/org-types)))
    (unless (eq type nil) (funcall (nth 0 (cdr type)) (nth 1 fr))))
  )

(defun org-auto-toggle/org-preview-fragment (fr)
  "Preview org fragment at FR."
  (let ((type (assoc (nth 0 fr) org-auto-toggle/org-types)))
    (unless (eq type nil) (funcall (nth 1 (cdr type)) (nth 1 fr))))
  )

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
            ;; but we were on one
            org-auto-toggle/org-last-fragment
            (not curr))
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
(define-minor-mode org-auto-toggle-mode
  "Toggle Fragments automatically."
  :global t
  (if org-auto-toggle-mode
      (add-hook 'post-command-hook 'org-auto-toggle/org-auto-toggle-fragment-display t)
      (remove-hook 'post-command-hook 'org-auto-toggle/org-auto-toggle-fragment-display t)
    )
  )
(provide 'org-auto-toggle)
;;; org-auto-toggle ends here
