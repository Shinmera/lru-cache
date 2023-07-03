(asdf:defsystem lru-cache-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the lru-cache system."
  :homepage "https://shinmera.github.io/lru-cache/"
  :bug-tracker "https://github.com/shinmera/lru-cache/issues"
  :source-control (:git "https://github.com/shinmera/lru-cache.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:lru-cache :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.lru-cache.test)))
