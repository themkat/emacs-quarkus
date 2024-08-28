(require 'helm)
(require 'request)
(require 's)
(require 'dash)
(require 'ht)
(require 'widget)

;; Experimental quarkus.el package for providing some of the same tooling that vs code does.

;; TODO: various configuration?
(defcustom quarkus-code-io-url "https://code.quarkus.io"
  "Configure to change the default API for Quarkus information."
  :type 'string
  :group 'quarkus)


;; api/extensions is the path to get the extensions
;; https://editor.swagger.io/?url=https://code.quarkus.io/q/openapi

;; TODO: extension presets for generator? 

;; Timeouts? (should we maybe set local the request-timeout variable or something?  or just "let" it? 
(defmacro quarkus--get-request (url &rest processor)
  (cl-destructuring-bind
      (name -> function)
      processor
    (if (or (null name) (null function))
        (error "Macro takes the form: url name -> processing-function")
      `(let (result)
         (request (s-concat quarkus-code-io-url ,url)
           :headers '(("accept" . "application/json"))
           :sync t
           :parser (lambda ()
                     (let ((json-object-type 'hash-table)
                           (json-array-type 'list))
                       (json-read)))
           :success (cl-function
                     (lambda (&key ,name &allow-other-keys)
                       (setq result ,function)))
           :error (cl-function
                   (lambda (&rest _ignore)
                     (message "Failed to fetch Quarkus Platform information!"))))
         result))))

(defun quarkus--get-platform-versions ()
  (quarkus--get-request
   "/api/streams"
   data -> (-map (lambda (version-info)
                   (ht-get version-info "platformVersion"))
                 data)))

(defmacro quarkus--set-text-field (field)
  `(lambda (self &rest _ignore)
     (setq-local ,field (widget-value self))))

;; can we invoke helm when a field is selected?
(defun quarkus-create ()
  (interactive)
  (let ((creation-form (get-buffer-create "*Quarkus Generate Project*"))
        (platform-versions (quarkus--get-platform-versions))
        (inhibit-read-only t))
    (with-current-buffer creation-form
      (kill-all-local-variables)
      (erase-buffer)
      (remove-overlays)

      ;; Set variables (we move out of let-scope when starting widget-mode it seems)
      (setq-local group-id "com.example"
                  artifact-id "demo"
                  version "1.0.0-SNAPSHOT"
                  platform (car platform-versions)
                  java-version "17")
      
      ;; TODO: any way to set the size? so we get a bigger heading?
      (widget-insert "~ Quarkus generate project ~\n\n")
      (widget-create 'editable-field
                     :format "Group id:    %v"
                     :notify (quarkus--set-text-field group-id)
                     group-id)
      (widget-create 'editable-field
                     :format "Artifact id: %v"
                     :notify (quarkus--set-text-field artifact-id)
                     artifact-id)
      (widget-create 'editable-field
                     :format "Version:     %v"
                     :notify (quarkus--set-text-field version)
                     version)
      (widget-insert "\nQuarkus platform version:\n")
      (apply #'widget-create (append `(radio-button-choice
                                       :notify (quarkus--set-text-field platform)
                                       :value ,platform)
                                     (-map (lambda (x)
                                             `(item ,x))
                                           platform-versions)))
      
      (widget-insert "\nJava version:\n")
      (widget-create 'radio-button-choice
                     :notify (quarkus--set-text-field version)
                     :value java-version
                     '(item "17")
                     '(item "21"))

      (widget-insert "\nBuild tool:\n")
      (widget-create 'radio-button-choice
                     :value "Maven"
                     '(item "Maven")
                     '(item "Gradle"))

      ;; TODO: dependenceies? use helm for beautiful selection of them. Allow multiple select
      
      (widget-insert "\n")
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               ;; TODO: should probably let the user select a target directory
                               ;; TODO: unzip directly?
                               (message "Probably generating app lol"))
                     "Generate app!")
      (widget-setup)
      (use-local-map widget-keymap)
      (switch-to-buffer creation-form))))


;; TODO: are there any way we could complete yaml and property files?
;; https://github.com/eclipse/lsp4mp/tree/master/microprofile.ls
;; (can the above language server also complete yaml?)

;; TODO: is requiring the quarkus cli okay here?


;; TODO: can we give a hint to the user that they might want to restart their lsp if it doesn't automatically listen to pom.xml changes (or the gradle alternative?)

(provide 'quarkus)
