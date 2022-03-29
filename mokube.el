(require 'ivy)
(unintern 'mokube-mode-map)
(unintern 'mokube-map)

(add-to-list 'auto-mode-alist '("\\.mkb$" . mokube-mode))

(defun mokube--goto-object-root ()
  (beginning-of-line)
  (let ((object-name nil))
    (while (not object-name)
      (if (mokube--at-object-name-p)
          (setq object-name (thing-at-point 'line t))
        (forward-line -1)))
    (mokube--format-object object-name)))

(defun mokube--get-object-at-point ()
  (save-excursion
    (mokube--goto-object-root)))

(defun mokube-goto-next-object ()
  "Go to next object"
  (interactive)
  (mokube--move-to-object 1))

(defun mokube-goto-previous-object ()
  "Go to next object"
  (interactive)
  (mokube--move-to-object -1))

(defun mokube--at-instace-name-p ()
  (save-excursion
    (beginning-of-line)
    (and (> (line-number-at-pos (point)) 5)
         (= (following-char) 32)
         (not (string-equal (substring (thing-at-point 'line t) 0 6) "  NAME"))
         (not (string-equal (substring (thing-at-point 'line t) 0 6) "  No r")))))

(defun mokube--at-object-name-p ()
  (save-excursion
    (beginning-of-line)
    (not (= (following-char) 32))))

(defun mokube--move-to-object (arg)
  (beginning-of-line)
  (forward-line arg)
  (let ((object-name nil))
    (while (not object-name)
      (if (mokube--at-object-name-p)
          (setq object-name (thing-at-point 'line t))
        (if (eq (forward-line arg) 1)
            (setq object-name "EOF"))))
    (message (mokube--format-object object-name))))

(defun mokube--format-object (object-name)
  (downcase
   (replace-regexp-in-string
    "\s" ""
    (replace-regexp-in-string ":\n" "" object-name))))

(defun mokube--get-context ()
  (save-excursion
    (message
     (save-match-data
       (string-match "Context: \\(\.+\\)"
                     (buffer-substring-no-properties (point-min) (point-max)))
       (match-string
        1
        (buffer-substring-no-properties (point-min) (point-max)))))))

(defun mokube--get-namespace ()
  (save-excursion
    (save-match-data
      (string-match "Namespace: \\(\.+\\)"
                    (buffer-substring-no-properties (point-min) (point-max)))
      (match-string
       1
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun mokube--format-kubectl-output (output)
  (let* ((lines (split-string output "\n"))
         (formatted-output (cl-map 'list
                                   'mokube--add-two-spaces
                                   lines)))
    (mapconcat 'identity formatted-output "\n")))

(defun mokube--add-two-spaces (string)
  (format "  %s" string))

(defun mokube--insert-command-result (command)
  (read-only-mode -1)
  (end-of-line)
  (newline)
  (insert (mokube--format-kubectl-output (shell-command-to-string command)))
  (delete-char -3)
  (read-only-mode 1))

(defun mokube-refresh-object ()
  (interactive)
  (if (< (line-number-at-pos) 4)
      (message "Not on an object.")
    (progn
      (let ((object (mokube--goto-object-root))
            (namespace (mokube--get-namespace)))
        (save-excursion
          (mokube--clear-object-list-maybe)
          (mokube--insert-command-result
           (format "kubectl get %s -n %s"
                   object
                   namespace))))
      )))

(defun mokube--clear-object-list-maybe ()
  (mokube--goto-object-root)
  (save-excursion
    (forward-line 1)
    (if (not (mokube--at-object-name-p))
        (mokube--clear-object-list))))

(defun mokube--clear-object-list ()
  (read-only-mode -1)
  (save-excursion
    (let ((start nil)
          (end nil))
      (if (not (mokube--at-object-name-p))
          (mokube-goto-previous-object))
      (beginning-of-line)
      (forward-line 1)
      (setq start (point))
      (mokube-goto-next-object)
      (setq end (point))
      (delete-region start end)))
  (read-only-mode 1))

(defun mokube-hide-show ()
  (interactive)
  (if (mokube--at-object-name-p)
      (progn
        (save-excursion
          (forward-line 1)
          (if (mokube--at-object-name-p)
              (progn
                (forward-line -1)
                (mokube-refresh-object))
            (mokube--clear-object-list-maybe))))))

(defun mokube--get-contexts ()
  (split-string
   (shell-command-to-string "kubectl config view -o jsonpath='{.contexts[*].name}' | tr ' ' '\n'")
   "\n"
   ))

(defun mokube--update (regexp match newValue)
  (save-excursion
    (read-only-mode -1)
    (goto-char (point-min))
    (re-search-forward regexp)
    (let* ((beg (match-beginning match))
           (end (match-end match)))
      (delete-region beg end)
      (goto-char beg)
      (insert newValue)))
  (read-only-mode 1))

(defun mokube--update-context (newValue)
  (shell-command (format "kubectl config use-context %s" newValue))
  (mokube--update "Context: \\(\.+\\)" 1 newValue))

(defun mokube--update-namespace (newValue)
  (mokube--update "Namespace: \\(\.+\\)" 1 newValue))

(defun mokube-set-context-ivy ()
  (interactive)
  (ivy-read "Contexts: "
            (mokube--get-contexts)
            :action (lambda (candidate)
                      (mokube--update-context candidate))))

(defun mokube--get-namespaces ()
  (split-string
   (shell-command-to-string "kubectl get namespaces -o name | cut -d'/' -f2 | head -c-1")
   "\n"))

(defun mokube-set-namespace-ivy ()
  (interactive)
  (ivy-read "Namespaces: "
            (mokube--get-namespaces)
            :action (lambda (candidate)
                      (mokube--update-namespace candidate))))

(defun mokube--parse-instance-name ()
  (nth 2 (split-string
          (thing-at-point 'line t) " ")))

(defun mokube-describe-object ()
  (interactive)
  (if (mokube--at-instace-name-p)
      (let* ((object (mokube--get-object-at-point))
             (instance (mokube--parse-instance-name))
             (namespace (mokube--get-namespace))
             (name (format "describe-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name
                       "kubectl" "describe" object instance "-n" namespace)
        (switch-to-buffer name))
    (message "No resource at point")))

(defun mokube-edit-object ()
  (interactive)
  (if (mokube--at-instace-name-p)
      (let* ((object (mokube--get-object-at-point))
             (instance (mokube--parse-instance-name))
             (namespace (mokube--get-namespace))
             (name (format "edit-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name
                       "kubectl" "edit" object instance "-n" namespace))
    (message "No resource at point")))

(defun mokube-delete-object ()
  (interactive)
  (if (mokube--at-instace-name-p)
      (let* ((object (mokube--get-object-at-point))
             (instance (mokube--parse-instance-name))
             (namespace (mokube--get-namespace))
             (name (format "delete-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name
                       "kubectl" "delete" object instance "-n" namespace))
    (message "No resource at point")))

(defun mokube--get-object (namespace object instance &optional format)
  "Run kubectl get to retrieve object data. Defaults to `json' format"
  (let ((format (if format
                    format
                  "json")))
    (json-read-from-string (shell-command-to-string
                 (format "kubectl get %s %s -n %s -o %s"
                         object instance namespace format)))))


(defun alist-get-recursive (data &rest keys)
  (if (not (car keys))
      data
    (apply #'alist-get-recursive
           (append (list (alist-get (car keys) data))
                   (cdr keys)))))

(defun mokube--get-property (namespace object instance &rest keys)
  "Get object Definition `keys', if no key provided return it complete
keys should look like: 'metadata 'label 'app)"
  (apply #'alist-get-recursive
         (append
          (list (mokube--get-object namespace
                                    object
                                    instance))
          keys)))

(defun mokube--log-pod (object instance namespace name log-length)
  (interactive "P")
  (start-process name name
                 "kubectl" "logs" "--tail" log-length "-f"
                 instance "-n" namespace))

(defun mokube--log-deploy (object instance namespace name log-length)
  (interactive "P")
  (let* ((label-app (mokube--get-property
                     namespace
                     object
                     instance
                     'metadata
                     'labels
                     'app))
         (label-arguments (if label-app
                              (list "-l"
                                    (format "app=%s" label-app))
                            '())))

    (apply 'start-process
           (append (list name name "kubectl"
                         "logs" "--tail" log-length
                         "-f" "-n" namespace)
                   label-arguments))))

(defun mokube-log (arg)
  (interactive "P")
  (let ((arg (number-to-string (if arg
                                   arg
                                 500)))
        (object (mokube--get-object-at-point)))
    (if (mokube--at-instace-name-p)
        (let* ((instance (mokube--parse-instance-name))
               (namespace (mokube--get-namespace))
               (name (format "log-%s-%s-%s"
                             namespace
                             object
                             instance)))
          (cond
           ((string-equal (mokube--get-object-at-point) "pods")
            (mokube--log-pod object instance namespace name arg))
           ((string-equal (mokube--get-object-at-point) "deployments")
            (mokube--log-deploy object instance namespace name arg))
           (t (message "No pod or deployment at point")))
          (switch-to-buffer name)
          (add-hook 'after-change-functions 'ansi-color-after-change nil t)))))

(defun mokube--clear-buffer-and-insert (name object namespace)
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t)
          (current-position (point))
          (using-region (use-region-p))
          (beg (if (use-region-p)
                   (region-beginning)))
          (end (if (use-region-p)
                   (region-end))))
      (erase-buffer)
      (mokube--insert-command-result
       (format "kubectl get %s -n %s"
               object
               namespace))

      (goto-char current-position)
      (if using-region
          (if (< current-position end)
              (progn
                (set-mark end)
                (goto-char beg))
            (progn
              (set-mark beg)
              (goto-char end)))))))

(defun mokube-watch-object (arg)
  (interactive "P")
  (let ((arg (if arg
                   arg
                 10)))
    (let* ((object (mokube--get-object-at-point))
           (namespace (mokube--get-namespace))
           (name (format "watch-%s-%s"
                         namespace
                         object)))
      (if (not (get-buffer name))
          (run-at-time t arg 'mokube--clear-buffer-and-insert name object namespace))
      (switch-to-buffer name))))

(defun mokube-bash-pod ()
  (interactive)
  (if (and (mokube--at-instace-name-p)
           (string-equal (mokube--get-object-at-point) "pods"))
      (let* ((object (mokube--get-object-at-point))
             (instance (mokube--parse-instance-name))
             (namespace (mokube--get-namespace))
             (name (format "bash-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (progn
          (ansi-term "/bin/zsh" name)
          (set-buffer (format "*%s*" name))
          (term-send-raw-string (format "kubectl exec -it %s -n %s -- /bin/bash \n"
                                        instance namespace)))
        (switch-to-buffer (format "*%s*" name)))
    (message "no pod at point")))

(defun mokube-port-forward-pod-or-service (arg)
  (interactive "P")
  (if (and (mokube--at-instace-name-p)
           (or (string-equal (mokube--get-object-at-point) "pods")
               (string-equal (mokube--get-object-at-point) "services")))
      (let* ((remote-port (if arg
                              (read-number "Remote Service Port: ")
                            80))
             (port (read-number "Port to forward: "))
             (object (mokube--get-object-at-point))
             (instance (mokube--parse-instance-name))
             (namespace (mokube--get-namespace))
             (name (format "portforward-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name "kubectl" "port-forward"
                       (format "%s/%s" object instance)
                       (format "%d:%d" port remote-port)
                       "-n" namespace)
        (message "command: kubectl port-forward %s/%s %d:%d"
                 object instance port remote-port))
    (message "no pod or service at point")))

(defun mokube-exec-pod ()
  (interactive)
  (if (and (mokube--at-instace-name-p)
           (string-equal (mokube--get-object-at-point) "pods"))
      (let* ((cmd (read-string "Command: "))
             (object (mokube--get-object-at-point))
             (instance (mokube--parse-instance-name))
             (namespace (mokube--get-namespace))
             (name (format "exec-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (apply 'start-process
               (append (list name name "kubectl" "-n" namespace "exec" instance)
                       (split-string cmd " ")))
        (switch-to-buffer name))
    (message "no pod at point")))

(defun mokube-top ()
  (interactive)
  (if (and (mokube--at-instace-name-p)
           (string-equal (mokube--get-object-at-point) "pods"))
      (let* ((object (mokube--get-object-at-point))
             (instance (mokube--parse-instance-name))
             (namespace (mokube--get-namespace))
             (name (format "top-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name
                       "kubectl" "top" object instance "-n" namespace)
        (switch-to-buffer name))
    (let* ((name "top-cluster"))
      (start-process name name
                     "kubectl" "top" "nodes")
      (switch-to-buffer name))))

(defvar mokube-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'mokube--get-object-at-point)
    (define-key map (kbd "u") 'mokube-goto-previous-object)
    (define-key map (kbd "C-c C-p") 'mokube-goto-previous-object)
    (define-key map (kbd "C-c C-n") 'mokube-goto-next-object)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "g") 'mokube-refresh-object)
    (define-key map (kbd "TAB") 'mokube-hide-show)
    (define-key map (kbd "C") 'mokube-set-context-ivy)
    (define-key map (kbd "N") 'mokube-set-namespace-ivy)
    (define-key map (kbd "d") 'mokube-describe-object)
    (define-key map (kbd "f") 'mokube-port-forward-pod-or-service)
    (define-key map (kbd "w") 'mokube-watch-object)
    (define-key map (kbd "t") 'mokube-top)
    (define-key map (kbd "e") 'mokube-edit-object)
    (define-key map (kbd "E") 'mokube-exec-pod)
    (define-key map (kbd "k") 'mokube-delete-object)
    (define-key map (kbd "b") 'mokube-bash-pod)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "l") 'mokube-log)
    map)
  "The keymap used in `mokube-mode'.")

(defvar mokube-highlights
  '(("Namespaces:\\|Nodes:\\|Bindings:\\|Config Maps:\\|Endpoints:\\|Events:\\|Limit Ranges:\\|Persistent Volume Claims:\\|Pods:\\|Podtemplates:\\|Replication Controllers:\\|Resource Quotas:\\|Secrets:\\|Serviceaccounts:\\|Services:\\|Controller Revisions:\\|Daemon Sets:\\|Deployments:\\|Replica Sets:\\|Stateful Sets:\\|Local Subject Access Reviews:\\|Horizontal Pod Auto Scalers:\\|Cronjobs:\\|Jobs:\\|Cron Jobs:\\|Backend Configs:\\|Ingresses:\\|Network Policies:\\|Pod Disruption Budgets:\\|Role Bindings:\\|Roles:\\|Scaling Policies:\\|Managed Certificates:\\|Elastic Search:\\|Kibana:\\|Certificate:\\|Issuer:\\|Persistent Volume Claim:\\|Storage Class:\\|Persistent Volume:\\|Cluster Issuer:\\|Horizontal Pod Autoscalers:\\|Backend Configs:\\|Service Accounts:\\|Replication Controller:\\|Daemon Set:\\|Cluster Role Binding:\\|Cluster Role:\\|Role Binding:\\|Role:\\|" . font-lock-function-name-face)
    ("  NAME\.*" . font-lock-comment-delimiter-face)
    ("Namespace:\\|Context:" . font-lock-constant-face)
    ("Context: \\(\.+\\)" . (1 font-lock-comment-face))
    ("Namespace: \\(\.+\\)" . (1 font-lock-comment-face))))

(define-derived-mode mokube-mode special-mode "mokube"
  "Moritz kubernetes major mode.
\\{mokube-map}"
  :group 'mokube-modes

  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq font-lock-defaults '(mokube-highlights))
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))
  (use-local-map mokube-map))

(add-hook 'mokube-mode-hook 'hl-line-mode)

;; use ansi colors in logs
(defun moritz/ansi-color (&optional beg end)
  "Interpret ANSI color esacape sequence by colorifying cotent.
  Operate on selected region on whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))

(defun ansi-color-after-change (beg end length)
  (save-excursion
    (goto-char beg)
    (if (string-match "\^\[" (buffer-substring beg end))
        (moritz/ansi-color beg end))))
