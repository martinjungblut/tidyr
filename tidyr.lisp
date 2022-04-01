(defun find-files (directory extension)
  (let ((directory-path (uiop:directory-exists-p directory)))
    (if directory-path
      (map 'list 'namestring
        (remove-if-not
          (lambda (filename)
            (str:ends-with? extension (namestring filename) :ignore-case t))
          (uiop:directory-files directory-path))))))

(defun filepath-format (directory filename)
  (format nil "~a~a" directory filename))

(defun filepath-only-filename (filepath)
  (multiple-value-bind (a b filename d) (uiop:split-unix-namestring-directory-components filepath)
    (declare (ignore a))
    (declare (ignore b))
    (declare (ignore d))
    filename))

(defun with-configuration-file-lines (callback)
  (ignore-errors
    (dolist (line (uiop:read-file-lines (uiop:native-namestring "~/.tidyr")))
      (let* ((parts (str:split " " line))
              (source-dir (uiop:directory-exists-p (nth 0 parts)))
              (extension (nth 1 parts))
              (destination-dir (uiop:directory-exists-p (nth 2 parts))))
        (if (and source-dir destination-dir)
          (funcall callback source-dir extension destination-dir))))))

(defun tidyr-main ()
  (loop
    (format t "Running...~&")
    (with-configuration-file-lines
      (lambda (source-dir extension destination-dir)
        (format t "Read configuration line. Source: ~a Extension: ~a Destination: ~a~&" source-dir extension destination-dir)
        (dolist (source-fpath (find-files source-dir extension))
          (let ((destination-fpath (filepath-format destination-dir (filepath-only-filename source-fpath))))
            (format t "~a -> ~a~&" source-fpath destination-fpath)
            (uiop:rename-file-overwriting-target source-fpath destination-fpath)))))
    (finish-output nil)
    (sleep 10)))
