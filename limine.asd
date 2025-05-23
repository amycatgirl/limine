(asdf:defsystem "limine"
  :components ((:module "src"
		:components ((:file "main"
			      :depends-on ("macros"))
			     (:file "macros"))))
  :version "0.0.1"
  :licence "MIT")
