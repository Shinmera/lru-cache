(asdf:defsystem lru-cache-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the lru-cache system."
  :homepage "https://shinmera.com/docs/lru-cache/"
  :bug-tracker "https://shinmera.com/project/lru-cache/issues"
  :source-control (:git "https://shinmera.com/project/lru-cache.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:lru-cache :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.lru-cache.test)))
