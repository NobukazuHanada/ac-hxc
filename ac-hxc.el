;; -*- coding: utf-8; lexical-binding: t -*-
(require 'haxe-mode)
(require 'deferred)
(require 'xml)


(defun create-haxe-completioner (work-dir hxml-name port) 
  (let ((candidates  nil)
        (dot-pos nil)
        (hxc-result "")
        (getted-hint nil))
    (letrec
        ((haxe-complete
          #'(lambda  ()
              (let* ((current-dot-pos (funcall get-cuurent-dot-pos (point)))
                     (current-filename (expand-file-name (buffer-file-name)))
                     (dot-catched (eq dot-pos current-dot-pos)))
                (if dot-catched
                    candidates
                  (progn
                    (funcall update-dot-pos current-dot-pos)
                    (funcall clean-candidates)
                    (save-buffer)
                    (funcall update-candidates-from-server
                             current-filename
                             current-dot-pos)
                    nil)))))
         
         (get-cuurent-dot-pos
          #'(lambda  (pos)
              (if (= (char-before) 10)
                  nil
                  (if (= (string-to-char ".") (char-before pos))
                      (1- pos)
                    (funcall get-cuurent-dot-pos (1- pos))))))
         
         (update-dot-pos
          #'(lambda (pos)
              (setq dot-pos pos)))

         (get-dot-pos
          #'(lambda ()
              dot-pos))

         (clean-pos
          #'(lambda ()
              (setq dot-pos nil)))
          
         (update-candidates
          #'(lambda  (result)
              (setq candidates result)))
         
         (clean-candidates
          #'(lambda ()
              (setq candidates [])))
         
         (update-candidates-from-server
          #'(lambda  (filename pos)
              (setq hxc-result "")
              (setq getted-hint nil)
              (let* ((send-string
                      (format "--cwd %s\n%s\n--display %s@%d\n\000"
                              work-dir hxml-name filename pos))
                     (process (open-network-stream
                               "get-hint-from-hxc-server"
                               "*temp-get-haxe-hint-buffer*"
                               "localhost"
                               port)))
                (set-process-filter process server-filter)
                (process-send-string process send-string)
                (process-send-eof process)
                (catch 'loop
                  (dotimes (i 3)
                    (accept-process-output process 0.3)
                    (when getted-hint
                      (let ((bf (process-buffer process)))
                        (delete-process process)
                        (kill-buffer bf))
                      (funcall server-result-to-popup-items)
                      (throw 'loop i))))
                (unless getted-hint
                  (let ((bf (process-buffer process)))
                    (delete-process process)
                    (kill-buffer bf))))))
         
         (server-filter
          #'(lambda (process result)
              (setq hxc-result (concat hxc-result result))
              (when (string-match "<\/list>" hxc-result)
                (setq getted-hint t))))
         
         (server-result-to-popup-items
          #'(lambda ()
              (let* ((xml
                      (with-temp-buffer (insert hxc-result)
                                        (xml-parse-region (point-min) (point-max))))
                     (list-node (car xml))
                     (i-nodes (xml-get-children list-node 'i)))
                (funcall update-candidates
                 (mapcar
                   (lambda (i-node)
                     (let* ((value (cdr (assoc 'n (xml-node-attributes i-node))))
                            (t-node (car (xml-get-children i-node 't)))
                            (type-expr (car (xml-node-children t-node)))
                            (d-node (car (xml-get-children i-node 'd)))
                            (doc (car (xml-node-children d-node))))
                       (popup-make-item
                        (concat "." value)
                        :summary (truncate-string-to-width
                                  (if (null type-expr) "" type-expr)
                                  22 0 nil "...")
                        :document
                        (if (null doc)
                            ""
                            (replace-regexp-in-string "\t" " " doc)))))
                   i-nodes))))))
     
      (lambda (msg)
        (pcase msg
          (`dot-pos (funcall get-cuurent-dot-pos (point)))
          (`start (funcall haxe-complete)))))))

;;; Connection Server 
(defun haxe-create-compilation-server (port)
  (let ((server-process-name (format "haxe-server %d" port))
        (server-buffer-name (format "*haxe-compilation-server %d*" port)))
    (letrec
        ((start-server #'(lambda ()
                            (start-process-shell-command
                             server-process-name
                             server-buffer-name
                             "haxe" "-v" "--wait" (number-to-string port))))
         (stop-server #'(lambda ()
                          (delete-process server-process-name)
                          (kill-buffer server-buffer-name))))
      (lambda (msg)
        (cond ((eq msg 'start) (funcall start-server))
              ((eq msg 'stop) (funcall stop-server)))))))

(defun start-hxc-server (obj)
  (funcall obj 'start))

(defun stop-hxc-server (obj)
  (funcall obj 'stop))

(defun hxc-start-server ()
  (interactive)
  (haxe-start-compilation-server))

(defun hxc-stop-server ()
  (interactive)
  (haxe-stop-compilation-server))

;;; init functions!
(defun haxe-make-local-variable () 
  (make-local-variable 'completationer)
  (make-local-variable 'server-obj))
(defun init-haxe-complete (work-dir hxml-file-name port)
  (interactive "Dworking directory: \nFhxml file name: \nnport: ")
  (haxe-make-local-variable)
  (init-haxe-comp (expand-file-name  work-dir)  hxml-file-name port))
(defun init-haxe-comp (work-dir hxml-file-name port)
  (setq server-obj (haxe-create-compilation-server port))
  (start-hxc-server server-obj)
  (setq completationer (create-haxe-completioner work-dir hxml-file-name port))
  (add-to-list 'ac-sources 'ac-haxe-complete)
  (auto-complete-mode 1))
(defun haxe-complete ()
  (funcall completationer 'start))
(defun haxe-complete-prefix-pos ()
  (let ((dot-pos (funcall completationer 'dot-pos)))
    dot-pos))

(defvar ac-haxe-complete
  '((candidates . haxe-complete)
    (prefix . haxe-complete-prefix-pos)))

(provide 'ac-hxc)
