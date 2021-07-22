;;; Copyright © 2020 Collin J. Doering <collin@rekahsoft.ca>
;;;
;;; This file is part of the GNU Guix channel rekahsoft-guix
;;;
;;; The rekahsoft-guix channel for GNU Guix is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; The rekahsoft-guix channel for GNU Guix is distributed in the hope that it
;;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with the rekahsoft-guix channel for GNU Guix.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (rekahsoft-gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public go-github-com-mattn-go-runewidth
  (package
    (name "go-runewidth")
    (version "0.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mattn/go-runewidth")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00b3ssm7wiqln3k54z2wcnxr3k3c7m1ybyhb9h8ixzbzspld0qzs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mattn/go-runewidth"))
    (synopsis "Provides functions to get fixed width of the character or string")
    (description
     "Provides functions to get fixed width of the character or string.")
    (home-page "https://github.com/mattn/go-runewidth")
    (license license:expat)))

(define-public go-github-com-mitchellh-gox
  (package
    (name "go-gox")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mitchellh/gox")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mkh81hd7kn45dz7b6yhzqsg2mvg1g6pwx89jjigxrnqhyg9vrl7"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mitchellh/gox"
       #:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis "Dead simple, no frills Go cross compile tool")
    (description
     "Gox is a simple, no-frills tool for Go cross compilation that behaves a
lot like standard go build.  Gox will parallelize builds for multiple
platforms.  Gox will also build the cross-compilation toolchain for you.")
    (home-page "https://github.com/mitchellh/gox")
    (license license:mpl2.0)))

(define-public go-github-com-androiddnsfix
  (package
    (name "go-androiddnsfix")
    (version "ff02804463540c36e3a148dcf4b009d003cf2a31")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mtibben/androiddnsfix")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pcbjs793kd0yg3dcp79agfxm7xm3sldx2r7v66ipzpcq0j2npi2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mtibben/androiddnsfix"
       #:phases %standard-phases))
    (synopsis "Hack to get around the issues building on android")
    (description
     "Hack around the issues in https://github.com/golang/go/issues/8877.")
    (home-page "https://github.com/mtibben/androiddnsfix")
    (license license:expat)))

(define-public aws-vault
  (package
    (name "aws-vault")
    (version "6.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/99designs/aws-vault")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0892fhjmxnms09bfbjnngnnnli2d4nkwq44fw98yb3d5lbpa1j1j"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-keyring" ,go-keyring)
       ("go-github-com-androiddnsfix" ,go-github-com-androiddnsfix)
       ("go-github-com-percent" ,go-github-com-percent)
       ("go-github-com-jmespath-go-jmespath" ,go-github-com-jmespath-go-jmespath)
       ("go-github-com-aws-aws-sdk-go" ,go-github-com-aws-aws-sdk-go)
       ("go-github-com-dvsekhvalnov-jose2go" ,go-github-com-dvsekhvalnov-jose2go)
       ("go-github-com-godbus-dbus" ,go-github-com-godbus-dbus)
       ("go-github-com-go-libsecret" ,go-github-com-go-libsecret)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-gopkg-in-ini" ,go-gopkg-in-ini)
       ("go-github-com-skratchdot-open-golang" ,go-github-com-skratchdot-open-golang)
       ("go-github-com-kingpin" ,go-github-com-kingpin)
       ("go-github-com-alecthomas-template" ,go-github-com-alecthomas-template)
       ("go-github-com-alecthomas-units" ,go-github-com-alecthomas-units)))
    (arguments
     `(#:import-path "github.com/99designs/aws-vault"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-version
           (lambda _
             (substitute* "src/github.com/99designs/aws-vault/main.go"
               (("var Version = \"dev\"") (string-append "var Version = \"v" ,version "\"")))))
         (add-after 'build 'contrib
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zsh-site-dir (string-append out "/share/zsh/site-functions"))
                    (bash-completion-dir (string-append out "/share/bash-completion/completions"))
                    (fish-completion-dir (string-append out "/share/fish/completions")))
               (for-each mkdir-p `(,zsh-site-dir ,bash-completion-dir ,fish-completion-dir))
               (with-directory-excursion "src/github.com/99designs/aws-vault/contrib/completions"
                 (copy-file "zsh/aws-vault.zsh"
                               (string-append zsh-site-dir "/_aws-vault"))
                 (copy-file "bash/aws-vault.bash"
                               (string-append bash-completion-dir "/aws-vault"))
                 (copy-file "fish/aws-vault.fish"
                               (string-append fish-completion-dir "/aws-vault.fish")))
               #t))))))
    (synopsis
     "Vault for securely storing and accessing AWS credentials in
development environments")
    (description
     "AWS Vault is a tool to securely store and access AWS credentials
in a development environment.

AWS Vault stores IAM credentials in your operating system's secure keystore and
then generates temporary credentials from those to expose to your shell and
applications.  It's designed to be complementary to the AWS CLI tools, and is
aware of your profiles and configuration in ~/.aws/config.")
    (home-page "https://github.com/99designs/aws-vault")
    (license license:expat)))

(define-public go-github-com-percent
  (package
    (name "go-percent")
    (version "v0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mtibben/percent")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1iqivw8pigj259rj5yifibbvic70f9hb7k24a4sa967s4fj6agb6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mtibben/percent"
       #:phases %standard-phases))
    (synopsis "Package percent escapes strings using percent-encoding")
    (description
     "Package percent escapes strings using percent-encoding.")
    (home-page "https://github.com/mtibben/percent")
    (license license:expat)))

(define-public go-keyring
  (package
    (name "go-keyring")
    (version "v1.1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/99designs/keyring")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08rcdqpxaa9n348q10fw40q4gxpjajkyrighykk71i7mxzwkcgwn"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-github-com-percent" ,go-github-com-percent)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-dvsekhvalnov-jose2go" ,go-github-com-dvsekhvalnov-jose2go)
       ("go-github-com-godbus-dbus" ,go-github-com-godbus-dbus)
       ("go-github-com-go-libsecret" ,go-github-com-go-libsecret)
       ("password-store" ,password-store)
       ("gnupg" ,gnupg)))
    (arguments
     '(#:import-path "github.com/99designs/keyring"
       #:phases (modify-phases %standard-phases
                  ;; Tests require vagrant
                  (delete 'check))))
    (synopsis "Go library providing a uniform interface across a range of
secure credential stores")
    (description
     "Keyring provides utility functions for and a common interface to a range
of secure credential storage services.  Originally developed as part of AWS
Vault, a command line tool for securely managing AWS access from developer
workstations.

Currently Keyring supports the following backends: macOS/OSX Keychain, Windows
pcredential store, Pass, Secret Service, KDE Wallet, Encrypted File.")
    (home-page "https://github.com/99designs/keyring")
    (license license:expat)))

(define-public go-github-com-dvsekhvalnov-jose2go
  (package
    (name "go-jose2go")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dvsekhvalnov/jose2go")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nzwvk6nqi7nm2wq4mr2q6k5p0qzsl0kmwx7kgkqsg1zh53250ld"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)))
    (arguments
     '(#:import-path "github.com/dvsekhvalnov/jose2go"
       #:phases %standard-phases))
    (synopsis "Go implementation of Javascript Object Signing and Encryption specification")
    (description
     "Pure Golang (GO) library for generating, decoding and encrypting JSON Web
Tokens.  Zero dependency, relies only on standard library.")
    (home-page "https://github.com/dvsekhvalnov/jose2go")
    (license license:expat)))

(define-public go-github-com-godbus-dbus
  (package
    (name "go-dbus")
    (version "5.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/godbus/dbus")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c107893nbdfc297i9y0smljmqs167mw26i24509qd09dmvr998y"))))
    (build-system go-build-system)
    (native-inputs
     `(("dbus" ,dbus)))
    (arguments
     '(#:import-path "github.com/godbus/dbus"
       #:tests? #f ; Tests currently fail for unknown reasons
       #:phases %standard-phases))
    (synopsis "Native Go bindings for D-Bus")
    (description
     "Dbus is a simple library that implements native Go client bindings for
the D-Bus message bus system.")
    (home-page "https://github.com/godbus/dbus")
    (license license:bsd-2)))

(define-public go-github-com-go-libsecret
  (let ((commit "a6f4afe4910cad8688db3e0e9b9ac92ad22d54e1")
        (revision "0"))
    (package
      (name "go-libsecret")
      (version "5.0.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gsterjov/go-libsecret")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09zaiadnll83vs22ib89agg7anj0blw5fywvmckxllsgif6ak6v7"))))
      (build-system go-build-system)
      (native-inputs
       `(("go-github-com-godbus-dbus" ,go-github-com-godbus-dbus)))
      (arguments
       '(#:import-path "github.com/gsterjov/go-libsecret"
         #:phases %standard-phases))
      (synopsis "Go library that manages secrets via the
freedesktop.org Secret Service DBus API")
      (description
       "Native go library that manages secrets via the freedesktop.org Secret
Service DBus API")
      (home-page "https://github.com/gsterjov/go-libsecret")
      (license license:expat))))

(define-public go-github-com-aws-aws-sdk-go
  (package
    (name "go-aws-sdk-go")
    (version "1.36.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aws/aws-sdk-go")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "169mkkw1cff1px6326krwvfpfj07sb4y5rbn003gi4bk176h6ry9"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-github-com-jmespath-go-jmespath" ,go-github-com-jmespath-go-jmespath)))
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go"
       #:phases %standard-phases))
    (synopsis "aws-sdk-go is the official AWS SDK for the Go programming language")
    (description
     "aws-sdk-go is the official AWS SDK for the Go programming language.")
    (home-page "https://github.com/aws/aws-sdk-go")
    (license license:asl2.0)))

(define-public go-gopkg-in-ini
  (package
    (name "go-ini")
    (version "v1.62.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gopkg.in/ini.v1")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dm9ydqyflasp5li22kb0w73s6kp2swii8naqfhnz64v171gmm5v"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-github.com-smartystreets-goconvey" ,go-github.com-smartystreets-goconvey)))
    (arguments
     '(#:import-path "gopkg.in/ini.v1"
       #:phases %standard-phases))
    (synopsis "Provides INI file read and write functionality in Go")
    (description
     "Provides INI file read and write functionality in Go.")
    (home-page "https://gopkg.in/ini.v1")
    (license license:asl2.0)))

(define-public go-github-com-skratchdot-open-golang
  (let ((commit "79abb63cd66e41cb1473e26d11ebdcd68b04c8e5")
        (revision "0"))
    (package
      (name "go-open-golang")
      (version "1.42.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/skratchdot/open-golang.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jc13jn8cj7p14n79zhav2nwga6kf9rqs01ic5k7j7agwzzly3ww"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/skratchdot/open-golang"
         #:phases (modify-phases %standard-phases
                    (delete 'build)
                    (delete 'check))))
      (synopsis "Open a file, directory, or URI using the OS's default application")
      (description
       "Open a file, directory, or URI using the OS's default application for
that object type.  Optionally, you can specify an application to use.

This is a proxy for the following commands: OSX - open, Windows - start,
Linux/Other - xdg-open")
      (home-page "https://github.com/skratchdot/open-golang")
      (license license:expat))))

(define-public go-github-com-kingpin
  (package
    (name "go-kingpin")
    (version "2.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alecthomas/kingpin")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mndnv3hdngr3bxp7yxfd47cas4prv98sqw534mx7vp38gd88n5r"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-github-com-alecthomas-template" ,go-github-com-alecthomas-template)
       ("go-github-com-alecthomas-units" ,go-github-com-alecthomas-units)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (arguments
     '(#:import-path "github.com/alecthomas/kingpin"
       #:phases %standard-phases))
    (synopsis "Go library provides utilities for building command line interfaces")
    (description
     "Go library provides utilities for building command line interfaces.")
    (home-page "https://github.com/alecthomas/kingpin")
    (license license:expat)))

(define-public go-github-com-alecthomas-template
  (let ((commit "a0175ee3bccc567396460bf5acd36800cb10c49c")
        (revision "0"))
      (package
        (name "go-alecthomas-template")
        (version "0.0.0")
        (source (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/alecthomas/template")
                        (commit commit)))
                  (file-name (git-file-name name version))
                  (sha256
                   (base32
                    "0qjgvvh26vk1cyfq9fadyhfgdj36f1iapbmr5xp6zqipldz8ffxj"))))
        (build-system go-build-system)
        (arguments
         '(#:import-path "github.com/alecthomas/template"
           #:phases %standard-phases))
        (synopsis "Fork of Go's text/template adding newline elision")
        (description
         "This is a fork of Go 1.4's text/template package with one addition: a
backslash immediately after a closing delimiter will delete all subsequent
newlines until a non-newline.")
        (home-page "https://github.com/alecthomas/template")
        (license license:bsd-3))))

(define-public go-github-com-alecthomas-units
  (let ((commit "2efee857e7cfd4f3d0138cc3cbb1b4966962b93a")
        (revision "0"))
      (package
        (name "go-alecthomas-units")
        (version "0.0.0")
        (source (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/alecthomas/units")
                        (commit commit)))
                  (file-name (git-file-name name version))
                  (sha256
                   (base32
                    "1j65b91qb9sbrml9cpabfrcf07wmgzzghrl7809hjjhrmbzri5bl"))))
        (build-system go-build-system)
        (native-inputs
         `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
        (arguments
         '(#:import-path "github.com/alecthomas/units"
           #:phases %standard-phases))
        (synopsis "Helpful unit multipliers and functions for Go")
        (description
         "Helpful unit multipliers and functions for Go")
        (home-page "https://github.com/alecthomas/units")
        (license license:expat))))
