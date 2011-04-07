;;
;; jstardict.el
;;
;; A thin interface for JStardict,a Stardict implementation on JVM.
;;
;; The jstardict.jar and dictionaries are put under
;; ~/site-lisp/jstardict directory by default; otherwise function
;; jsd-path must be modify to return a right path by which
;; the jar and dictionary files could be located correctly.
;;

(defconst JSTARDICT_CLASS "jstardict.Stardict")

(define-derived-mode jsd-mode nil "JStardict" "")

(defface jsd-face-hl nil "JStardict face for highlighting searched word.")
(set-face-attribute 'jsd-face-hl nil :bold 't)

(defun jsd-lookup-2 ()
  (interactive)
  (let ((the-word (thing-at-point 'word)))
    (if the-word (jsd-lookup the-word)
      (call-interactively 'jsd-lookup))
    ))
(global-set-key (kbd "C-c s") 'jsd-lookup-2)


(defun jsd-startup ()
  (interactive)
  (let ((jsd-buf) (jsd-proc (jsd-is-running)))
    (if jsd-proc jsd-proc
      (setq jsd-buf
            (scala JSTARDICT_CLASS
                   (jsd-path "jstardict.jar") nil
                   (jsd-path)))
      (with-current-buffer jsd-buf (jsd-mode))
      (setq jsd-proc (get-buffer-process jsd-buf))
      (set-process-filter jsd-proc 'jsd-output-filter)
      )
    jsd-proc
    ))

(defun jsd-lookup (word)
  (interactive "MWord: ")
  (if (jsd-buffer)
      (with-current-buffer (jsd-buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq buffer-read-only 't)
        ))
  (send-string (jsd-startup) (concat word "\n"))
  (pop-to-buffer (jsd-buffer)))
(global-set-key (kbd "C-c . s") 'jsd-lookup)

(defun jsd-switch ()
  (interactive)
  (switch-to-buffer (jsd-buffer)))

(defun jsd-exit ()
  (interactive)
  (jsd-lookup "\\\n"))

(defun jsd-is-running ()
  (and (jsd-buffer) (get-buffer-process (jsd-buffer))))

(defun jsd-buffer ()
  (get-buffer (concat "*" JSTARDICT_CLASS "*")))


(defun jsd-path (&optional path)
  (expand-file-name
   (concat-file-names "~/site-lisp/jstardict" path) "/"))

(defun jsd-output-filter (proc output)
  "Highlight the searched word. The searched word is separated 
from the explanations by a bar, ie |. String <<< means the end of 
the explanation. See jstardict.Stardict for details."
  (with-current-buffer (process-buffer proc)
    (let ((end (string-match "<<<$" output)))
      (setq buffer-read-only nil)
      (insert (substring output 0 end))
      (if end (progn
                (put-text-property 1 (string-match "|" (buffer-string))
                                   'face 'jsd-face-hl)
                (beginning-of-buffer)
                )
        )
      (setq buffer-read-only 't)      
      )))