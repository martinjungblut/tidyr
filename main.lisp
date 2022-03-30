(defun find-files (directory extension)
  (let ((directory-path (uiop:directory-exists-p directory)))
    (if directory-path
      (map 'list 'namestring
        (remove-if-not
          (lambda (filename)
            (str:ends-with? extension (namestring filename) :ignore-case t))
          (uiop:directory-files directory-path))))))

(defun with-configuration-file-lines (callback)
  (dolist (line (uiop:read-file-lines (uiop:native-namestring "~/.tidyr")))
    (let* ((parts (str:split " " line))
            (source-dir (uiop:directory-exists-p (nth 0 parts)))
            (extension (nth 1 parts))
            (destination-dir (uiop:directory-exists-p (nth 2 parts))))
      (if (not source-dir)
        (print (format nil "Source directory does not exist: ~a" (nth 0 parts))))
      (if (not destination-dir)
        (print (format nil "Destination directory does not exist: ~a" (nth 2 parts))))
      (if (and source-dir destination-dir)
        (funcall callback source-dir extension destination-dir)))))

(defun format-filepath (directory filename)
  (format nil "~a~a" directory filename))

(defun only-filename (filepath)
  (multiple-value-bind (a b filename d) (uiop:split-unix-namestring-directory-components filepath)
    (declare (ignore a))
    (declare (ignore b))
    (declare (ignore d))
    filename))

(with-configuration-file-lines
  (lambda (source-dir extension destination-dir)
    (dolist (filepath (find-files source-dir extension))
      (let ((destination (format-filepath destination-dir (only-filename filepath))))
        (print (format nil "~a -> ~a" filepath destination))
        (uiop:rename-file-overwriting-target filepath destination)))))
