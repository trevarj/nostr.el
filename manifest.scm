(use-modules (gnu packages))

(specifications->manifest
 '("emacs"
   "emacs-emacsql"
   "emacs-websocket"
   "emacs-plz"
   "rust"
   "rust:cargo"
   "gcc-toolchain"
   "pkg-config"
   "openssl"
   "nss-certs"
   "sqlite"))
