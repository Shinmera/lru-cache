(asdf:defsystem lru-cache
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A least-recently-used cache structure"
  :homepage "https://shinmera.github.io/lru-cache/"
  :bug-tracker "https://github.com/shinmera/lru-cache/issues"
  :source-control (:git "https://github.com/shinmera/lru-cache.git")
  :serial T
  :components ((:file "lru-cache")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :lru-cache-test))))
