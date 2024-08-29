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
                   (ht-get version-info "key"))
                 data)))

;; TODO: check if we should rewrite to only get extensions supporting our current version
(defun quarkus--get-extensions ()
  (quarkus--get-request
   "/api/extensions"
   data -> data))

(defmacro quarkus--set-text-field (field)
  `(lambda (self &rest _ignore)
     (setq-local ,field (widget-value self))))

;; can we invoke helm when a field is selected?
(defun quarkus-create ()
  (interactive)
  (message "Fetching Quarkus information, please wait...")
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
                  java-version "17"
                  build-tool "Maven"
                  selected-extensions '()

                  ;; Extension list cached while we are generating application
                  extensions (quarkus--get-extensions))
      
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
                                       :notify ,(quarkus--set-text-field platform)
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
                     :notify (quarkus--set-text-field build-tool)
                     :value build-tool
                     '(item "Maven")
                     '(item "Gradle")
                     '(item "Gradle Kotlin DSL"))
      
      (widget-insert "\n")
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               ;; TODO: should probably let the user select a target directory
                               ;; TODO: can probably put the download logic into its own function to clean up a bit
                               (shell-command (format "curl -s -X POST '%s/api/download' -H \"Content-Type: application/json\" -H \"accept: */*\" -d '%s' | bsdtar -xf-"
                                                      quarkus-code-io-url
                                                      (json-serialize (ht ("streamKey" platform)
                                                                          ("groupId" group-id)
                                                                          ("artifactId" artifact-id)
                                                                          ("version" version)
                                                                          ("buildTool" (s-upcase (s-snake-case build-tool)))
                                                                          ("javaVersion" java-version)
                                                                          
                                                                          ))
                                                      )
                                              (get-buffer-create "BAJS"))
                               ;;(dired "/Users/marie")
                               )
                     "Generate app! ")
      (widget-setup)
      (use-local-map widget-keymap)
      (switch-to-buffer creation-form))))


;; TODO: are there any way we could complete yaml and property files?
;; https://github.com/eclipse/lsp4mp/tree/master/microprofile.ls
;; (can the above language server also complete yaml?)

;; TODO: is requiring the quarkus cli okay here?

;; TODO: check if there is a better way than using the Quarkus CLI and parsing results.
(defun quarkus-add-extension ()
  (interactive)
  (let* ((current-path (file-name-directory buffer-file-name))
         (project-root (or (lsp-workspace-root)
                           (locate-dominating-file current-path
                                                   "pom.xml")
                           (locate-dominating-file current-path
                                                   "build.gradle")
                           (locate-dominating-file current-path
                                                   "build.gradle.kts")
                           (error "Probably not a Quarkus project you dipshit")))
         ;; TODO: some extensions give errors, so we should filter out the ones that can be installed. Or at least check that they can be installed for our version 
         (extensions (quarkus--get-extensions)))
    (helm :sources (helm-build-sync-source "Extensions"
                     :action (lambda (&rest _ignore)
                               ;; TODO: maybe we could have different actions? One being go to guide? the API provides lots of cool small thingys like that
                               ;;       (how to keep this extension view open)
                               ;; TODO: add extension to project
                               (shell-command (format "cd %s && quarkus extension add %s"
                                                      project-root
                                                      (s-join ","
                                                              (helm-marked-candidates))))
                               (message "Added Quarkus extension(s)!"))
                     :persistent-help "Select extensions"
                     :candidates (-map (lambda (x)
                                         ;; TODO: find a better way of presenting hints. all ways so far is fucking atrocious 
                                         (cons (format "%-50s%s"
                                                       (ht-get x "name")
                                                       (s-truncate 100 (ht-get x "description")))
                                               (ht-get x "id")))
                                       extensions)
                     )
          :buffer "*Quarkus Extensions*")))


;; TODO: can we give a hint to the user that they might want to restart their lsp if it doesn't automatically listen to pom.xml changes (or the gradle alternative?)

(provide 'quarkus)
