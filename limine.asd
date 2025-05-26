(asdf:defsystem "limine"
  :components ((:module "src"
			:components ((:file "package")
				     (:file "main"
					    :depends-on ("macros"))
				     (:file "macros"))))
  :depends-on (:nodgui)
  :version "0.0.1"
  :licence "MIT")
