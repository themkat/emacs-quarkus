(require 'helm)
(require 'request)
(require 's)
(require 'widget)

;; Experimental quarkus.el package for providing some of the same tooling that vs code does.

;; TODO: various configuration?
(defcustom quarkus-code-io-url "https://code.quarkus.io"
  "Configure to change the default API for Quarkus information."
  :type 'string
  :group 'quarkus)


;; TODO: maybe we can have generation of Quarkus projects directly in Emacs? Instead of a cli?
;; maybe that as a first step?

;; TODO: extension presets for generator? 

;; TODO: function to get the availbable quarkus platform versions
;;       function to fetch available extensions, which we can use helm to search through
;; TODO: maybe create a macro or function if we reuse a lot here..
;;       maybe that one can return a json object directly to us so it is somewhat intuitive?
;; Timeouts? (should we maybe set local the request-timeout variable or something?  or just "let" it? 
(defun quarkus--get-platform-versions ()
  ;; TODO: best way to return 
  (request (s-concat quarkus-code-io-url "/api/streams")
    :headers '(("accept" . "application/json"))
    :sync t
    :parser (lambda ()
              (let ((json-object-type 'hash-table)
                    (json-array-type 'list))
                (json-read)))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message "Data: %s" data)))
    :error (cl-function
            (lambda ()
              (message "Failed to fetch Quarkus Platform information!")))))

(let ((test 1))
  (quarkus--get-platform-versions)
  test)

(defmacro quarkus--set-text-field (field)
  `(lambda (self &rest _ignore)
     (setq-local ,field (widget-value self))))

;; extensions can we create a menu in the best way in Emacs?
;; use forms? https://www.gnu.org/software/emacs/manual/html_mono/forms.html
;; or widgets? probably better? https://www.gnu.org/software/emacs/manual/html_mono/widget.html
;; can we invoke helm when a field is selected?
;; TODO: how ot make the text and everything read only?
(defun quarkus-create ()
  (interactive)
  (let ((creation-form (get-buffer-create "*Quarkus Generate Project*")))
    (with-current-buffer creation-form
      (erase-buffer)
      (remove-overlays)

      ;; Set variables (we move out of let-scope when starting widget-mode it seems)
      (setq-local group-id "com.example"
                  artifact-id "demo"
                  version "1.0.0-SNAPSHOT")
      
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
      (widget-create 'radio-button-choice
                     :value "test2"
                     '(item "KENOBI")
                     '(item "test2"))
      ;; TODO: checkbox with the Quarkus platform version with a sensible default selected?
      ;; TODO: Checkbox with build tool (maven vs gradle?)
      ;; TODO: checkbox with java version?
      ;; TODO: dependenceies? use helm for beautiful selection of them. Allow multiple select
      (widget-insert "\nJava version:\n")
      (widget-create 'radio-button-choice
                     :value "test2"
                     '(item "11")
                     '(item "17")
                     '(item "21"))

      (widget-insert "\nBuild tool:\n")
      (widget-create 'radio-button-choice
                     :value "Maven"
                     '(item "Maven")
                     '(item "Gradle"))
      
      (widget-insert "\n")
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               (message "group id: %s" group-id))
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
