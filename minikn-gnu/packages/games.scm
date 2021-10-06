;;; Copyright © 2020 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2021 Demis Balbach <db@minikn.xyz>
;;;
;;; This file is part of the GNU Guix channel minikn-guix
;;;
;;; The minikn-guix channel for GNU Guix is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; The minikn-guix channel for GNU Guix is distributed in the hope that it
;;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with the minikn-guix channel for GNU Guix.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (minikn-gnu packages games)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bootstrap)          ;for 'bootstrap-guile-origin'
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages less)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages man)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public libostree
  (package
    (name "libostree")
    (version "2021.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ostreedev/ostree/releases/download/v"
             (version-major+minor version) "/libostree-" version ".tar.xz"))
       (sha256
        (base32 "1cyhr3s7xsgnsais5m4cjwdwcq46naf25r1k042c4n1y1jgs798g"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Don't try to use the non-existing '/var/tmp' as test
             ;; directory.
             (setenv "TEST_TMPDIR" (getenv "TMPDIR"))
             #t)))
       ;; XXX: fails with:
       ;;     tap-driver.sh: missing test plan
       ;;     tap-driver.sh: internal error getting exit status
       ;;     tap-driver.sh: fatal: I/O or internal error
       #:tests? #f))
    (native-inputs
     `(("attr" ,attr)                   ; for tests
       ("bison" ,bison)
       ("glib:bin" ,glib "bin")         ; for 'glib-mkenums'
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("avahi" ,avahi)
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("e2fsprogs" ,e2fsprogs)
       ("fuse" ,fuse)
       ("glib" ,glib)
       ("gpgme" ,gpgme)
       ("libarchive" ,libarchive)
       ("libsoup" ,libsoup)
       ("util-linux" ,util-linux)))
    (home-page "https://ostree.readthedocs.io/en/latest/")
    (synopsis "Operating system and container binary deployment and upgrades")
    (description
     "@code{libostree} is both a shared library and suite of command line
tools that combines a \"git-like\" model for committing and downloading
bootable file system trees, along with a layer for deploying them and managing
the boot loader configuration.")
    (license license:lgpl2.0+)))

(define-public flatpak-git
  (package
   (name "flatpak-git")
   (version "1.11.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/flatpak/flatpak/releases/download/"
                         version "/flatpak-" version ".tar.xz"))
     (sha256
      (base32 "1r6xw7r3ir2vaa30n3mily6m7d51cf4qv22fkqlzzy3js0wjf5fv"))))

   ;; Wrap 'flatpak' so that GIO_EXTRA_MODULES is set, thereby allowing GIO to
   ;; find the TLS backend in glib-networking.
   (build-system glib-or-gtk-build-system)

   (arguments
    '(#:configure-flags
      (list
       "--enable-documentation=no" ;; FIXME
       "--enable-system-helper=no"
       "--localstatedir=/var"
       (string-append "--with-system-bubblewrap="
                      (assoc-ref %build-inputs "bubblewrap")
                      "/bin/bwrap")
       (string-append "--with-system-dbus-proxy="
                      (assoc-ref %build-inputs "xdg-dbus-proxy")
                      "/bin/xdg-dbus-proxy"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-tests
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively
             (string-append (assoc-ref inputs "glibc-utf8-locales")
                            "/lib/locale/") "/tmp/locale")
            (for-each make-file-writable (find-files "/tmp"))
            (substitute* "tests/make-test-runtime.sh"
              (("cp `which.*") "echo guix\n")
              (("cp -r /usr/lib/locale/C\\.\\*")
               (string-append "mkdir ${DIR}/usr/lib/locale/en_US; \
cp -r /tmp/locale/*/en_US.*")))
            (substitute* "tests/libtest.sh"
              (("/bin/kill") (which "kill"))
              (("/usr/bin/python3") (which "python3")))
            #t))
        ;; Many tests fail for unknown reasons, so we just run a few basic
        ;; tests.
        (replace 'check
          (lambda _
            (setenv "HOME" "/tmp")
            (invoke "make" "check"
                    "TESTS=tests/test-basic.sh tests/test-config.sh testcommon"))))))
    (native-inputs
    `(("bison" ,bison)
      ("dbus" ,dbus) ; for dbus-daemon
      ("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")          ; for glib-mkenums + gdbus-codegen
      ("glibc-utf8-locales" ,glibc-utf8-locales)
      ("gobject-introspection" ,gobject-introspection)
      ("libcap" ,libcap)
      ("pkg-config" ,pkg-config)
      ("python" ,python)
      ("python-pyparsing" ,python-pyparsing)
      ("socat" ,socat)
      ("which" ,which)))
   (propagated-inputs `(("glib-networking" ,glib-networking)
                        ("gnupg" ,gnupg)
                        ("gsettings-desktop-schemas"
                         ,gsettings-desktop-schemas)))
   (inputs
    `(("appstream-glib" ,appstream-glib)
      ("bubblewrap" ,bubblewrap)
      ("dconf" ,dconf)
      ("fuse" ,fuse)
      ("gdk-pixbuf" ,gdk-pixbuf)
      ("gpgme" ,gpgme)
      ("json-glib" ,json-glib)
      ("libarchive" ,libarchive)
      ("libostree" ,libostree)
      ("libseccomp" ,libseccomp)
      ("libsoup" ,libsoup)
      ("libxau" ,libxau)
      ("libxml2" ,libxml2)
      ("util-linux" ,util-linux)
      ("xdg-dbus-proxy" ,xdg-dbus-proxy)))
   (home-page "https://flatpak.org")
   (synopsis "System for building, distributing, and running sandboxed desktop
applications")
   (description "Flatpak is a system for building, distributing, and running
sandboxed desktop applications on GNU/Linux.")
   (license license:lgpl2.1+)))
