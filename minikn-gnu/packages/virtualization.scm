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

(define-module (minikn-gnu packages virtualization)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public libpod
  (package
    (name "libpod")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/libpod.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05xbxldhm3cgdjysidzpc8wnq17mgd84mq60jmbq6zcc9jl21axh"))))
    (build-system go-build-system)
    (native-inputs
     `(("gpgme" ,gpgme)
       ("pkg-config" ,pkg-config)
       ("libassuan" ,libassuan)
       ("libseccomp" ,libseccomp)
       ("go-md2man" ,go-md2man)))
    (propagated-inputs
     `(("runc" ,runc)
       ("conmon" ,conmon)
       ("slirp4netns" ,slirp4netns)))
    (arguments
     '(#:import-path "github.com/containers/libpod"
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (with-directory-excursion "src/github.com/containers/libpod"
                        (setenv "HOME" "/tmp")
                        (invoke "make" "all"
                                "GIT_COMMIT=NONE"
                                "GIT_BRANCH=NONE"))))
                  ;; FIXME: tests currently require docker
                  ;; (replace 'check
                  ;;   (lambda _
                  ;;     (with-directory-excursion "src/github.com/containers/libpod"
                  ;;       (setenv "HOME" "/tmp")
                  ;;       (invoke "make" "test"))))
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (with-directory-excursion "src/github.com/containers/libpod"
                          (invoke "make" "install"
                                  (string-append "PREFIX=" out)
                                  (string-append "DESTDIR=" out)))))))))
    (synopsis "Library used to create containers pods and home of Podman")
    (description
     "Libpod provides a library for applications looking to use the container
pod concept, popularized by kubernets.  Libpod also contains the pod manager
tool Podman.  Podman manages pods, containers, container images, and container
volumes.")
    (home-page "https://github.com/containers/libpod")
    (license license:asl2.0)))

(define-public conmon
  (package
    (name "conmon")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/conmon.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s23gm0cq4mylv882dr1n8bqql42674vny3z58yy77lwzmifc6id"))))
    (build-system go-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib)))
    (arguments
     '(#:import-path "github.com/containers/conmon"
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (with-directory-excursion "src/github.com/containers/conmon"
                        (setenv "HOME" "/tmp")
                        (invoke "make" "all"
                                "CC=gcc"
                                "GIT_COMMIT=NONE"
                                "GIT_BRANCH=NONE"))))
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (with-directory-excursion "src/github.com/containers/conmon"
                          (invoke "make" "install"
                                  (string-append "PREFIX=" out)))))))))
    (synopsis "OCI container runtime monitor. ")
    (description
     "Conmon is a monitoring program and communication tool between a container
manager (like podman or CRI-O) and an OCI runtime (like runc or crun) for a single
container.")
    (home-page "https://github.com/containers/conmon")
    (license license:asl2.0)))

(define-public buildah
  (package
    (name "buildah")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/buildah.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19yf93pq4vw24h76kl32c6ryvg5fp5mixakw9c6sqydf7m74z9i8"))))
    (build-system go-build-system)
    (inputs
     `(("git" ,git)))
    (propagated-inputs
     `(("runc" ,runc)
       ("cni-plugins" ,cni-plugins)))
    (native-inputs
     `(("gpgme" ,gpgme)
       ("pkg-config" ,pkg-config)
       ("libassuan" ,libassuan)
       ("libseccomp" ,libseccomp)
       ("lvm2" ,lvm2)
       ("eudev" ,eudev)
       ("glib" ,glib)
       ("btrfs-progs" ,btrfs-progs)
       ("libostree" ,libostree)
       ("libselinux" ,libselinux)
       ("go-md2man" ,go-md2man)))
    (arguments
     '(#:import-path "github.com/containers/buildah"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (with-directory-excursion "src/github.com/containers/buildah"
               (setenv "HOME" "/tmp")
               (invoke "make" "binary" "docs" "GIT_COMMIT=NONE"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "src/github.com/containers/buildah"
                 (invoke "make" "install"
                         "GIT_COMMIT=NONE"
                         (string-append "PREFIX=" out)))
               #t))))))
    (synopsis "Tool that facilitates building OCI images")
    (description
     "Buildah provides a command line tool for creating and manipulating OCI
container images.")
    (home-page "https://github.com/containers/buildah")
    (license license:asl2.0)))

(define-public go-md2man
  (package
    (name "go-md2man")
    (version "1.0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cpuguy83/go-md2man.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bqkf2bvy1dns9zd24k81mh2p1zxsx2nhq5cj8dz2vgkv1xkh60i"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cpuguy83/go-md2man"
       #:install-source? #f
       #:phases %standard-phases))
    (synopsis "Converts markdown into roff (man pages)")
    (description
     "Uses blackfriday to process markdown into man pages.")
    (home-page "https://github.com/cpuguy83/go-md2man")
    (license license:expat)))

(define-public cni-plugins
  (package
    (name "cni-plugins")
    (version "0.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containernetworking/plugins.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07d8knnabfjipzvcqbq7h8gd940lln934xp57nf5x31d3hpmvzws"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/containernetworking/plugins"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (with-directory-excursion "src/github.com/containernetworking/plugins"
               (setenv "HOME" "/tmp")
               (invoke "./build_linux.sh"))))
         (delete 'check) ;; Tests currently use sudo
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively
                "src/github.com/containernetworking/plugins/bin"
                (string-append out "/usr/lib/cni"))
               #t))))))
         ;; FIXME: Enable tests
         ;; (replace 'check
         ;;   (lambda _
         ;;     (with-directory-excursion "src/github.com/containernetworking/plugins"
         ;;       (invoke "./test_linux.sh"))))
    (synopsis "Some standard networking plugins, maintained by the CNI team")
    (description
     "A collection of CNI networking plugins.")
    (home-page "https://github.com/containernetworking/plugins")
    (license license:asl2.0)))

(define-public slirp4netns
  (package
    (name "slirp4netns")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rootless-containers/slirp4netns.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jai403d51w1ym7y12hq2k0hrysnc0d5kkzq8wcx7g00jk1rhkff"))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO: Tests currently fail
     '(#:tests? #f))
    (inputs
     `(("glib" ,glib)
       ("libcap" ,libcap)
       ("libseccomp" ,libseccomp)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (synopsis "User-mode networking for unprivileged network namespaces")
    (description
     "slirp4netns allows connecting a network namespace to the Internet in a
completely unprivileged way, by connecting a TAP device in a network namespace
to the usermode TCP/IP stack (\"slirp\").")
    (home-page "https://github.com/rootless-containers/slirp4netns")
    (license license:gpl2+)))
