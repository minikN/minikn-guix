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

(define-module (rekahsoft-gnu packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages serialization)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public python-virtualenv-clone
  (package
    (name "python-virtualenv-clone")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "virtualenv-clone" version))
        (sha256
          (base32
            "0absh96fsxk9di7ir76f5djyfm2c214wnyk53avrhjy8akflhpk6"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-tox" ,python-tox)))
    (home-page
      "https://github.com/edwardgeorge/virtualenv-clone")
    (synopsis "script to clone virtualenvs.")
    (description "script to clone virtualenvs.")
    (license license:expat)))

(define-public python-arpeggio
  (package
    (name "python-arpeggio")
    (version "1.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Arpeggio" version))
        (sha256
          (base32
            "0aq2pmhfyq7vhbhyq8jgxiphncy1s79rmrsggz4p52m4cdhy134l"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/textX/Arpeggio")
    (synopsis "Packrat parser interpreter")
    (description "Packrat parser interpreter")
    (license license:expat)))

(define-public python-ordereddict
  (package
    (name "python-ordereddict")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ordereddict" version))
        (sha256
          (base32
            "07qvy11nvgxpzarrni3wrww3vpc9yafgi2bch4j2vvvc42nb8d8w"))))
    (build-system python-build-system)
    (home-page "UNKNOWN")
    (synopsis
      "A drop-in substitute for Py2.7's new collections.OrderedDict that works in Python 2.4-2.6.")
    (description
      "A drop-in substitute for Py2.7's new collections.OrderedDict that works in Python 2.4-2.6.")
    (license license:expat)))

(define-public python-parver
  (package
    (name "python-parver")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parver" version))
       (sha256
        (base32
         "0a6jp17c1ag6b9yp5xgy9wvznk3g0v2f8gpwkcwxpyc9ygk98zdm"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-six" ,python-six)
       ("python-attrs" ,python-attrs)
       ("python-arpeggio" ,python-arpeggio)
       ("python-pytest" ,python-pytest)
       ("python-hypothesis" ,python-hypothesis)
       ("python-pretend" ,python-pretend)))
    (home-page "https://github.com/RazerM/parver")
    (synopsis
     "Parse and manipulate version numbers")
    (description
     "Parse and manipulate version numbers.")
    (license license:expat)))

(define-public python-pipenv
  (package
    (name "python-pipenv")
    (version "2018.11.26")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pipenv" version))
       (sha256
        (base32
         "0ip8zsrwmhrankrix0shig9g8q2knmr7b63sh7lqa8a5x03fcwx6"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-arpeggio" ,python-arpeggio)))
    (propagated-inputs
     `(("python-certifi" ,python-certifi)
       ("python-invoke" ,python-invoke)
       ("python-parver" ,python-parver)
       ("python-ordereddict" ,python-ordereddict)
       ("python-requests" ,python-requests)
       ("python-virtualenv" ,python-virtualenv)
       ("python-virtualenv-clone" ,python-virtualenv-clone)))
    (arguments
     ;; FIXME: Tests currently fail due to an issue with python-typing and the
     ;; built in typing in pythong 3.7
     '(#:tests? #f))
    (home-page "https://github.com/pypa/pipenv")
    (synopsis
     "Python Development Workflow for Humans")
    (description
     "Python Development Workflow for Humans.")
    (license license:expat)))

(define-public python-dbusmock
  (package
    (name "python-dbusmock")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-dbusmock" version))
       (sha256
        (base32
         "1hl6zk3blc561csjp5sfpbglpbwr9qp65f9k4smdxmgwsbp30zs9"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-shell-path
           (lambda _
             (substitute* "tests/test_code.py"
               (("/bin/bash") (which "bash")))
             #t)))))
    (native-inputs
     `(;; For tests.
       ("dbus" ,dbus) ; for dbus-daemon
       ("python-nose" ,python-nose)
       ("which" ,which)))
    (propagated-inputs
     `(("python-dbus" ,python-dbus)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://github.com/martinpitt/python-dbusmock")
    (synopsis "Python library for mock D-Bus objects")
    (description "python-dbusmock allows for the easy creation of mock objects on
D-Bus.  This is useful for writing tests for software which talks to D-Bus
services such as upower, systemd, logind, gnome-session or others, and it is
hard (or impossible without root privileges) to set the state of the real
services to what you expect in your tests.")
    (license license:lgpl3+)))

(define-public python-ujson
  (package
    (name "python-ujson")
    (version "2.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ujson" version))
        (sha256
         (base32
          "18z9gb9ggy1r464b9q1gqs078mqgrkj6dys5a47529rqk3yfybdx"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "deps") #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-to-system-double-conversion
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((d-c (assoc-ref inputs "double-conversion")))
               (substitute* "setup.py"
                 (("./deps/double-conversion/double-conversion\"")
                  (string-append d-c "/include/double-conversion\""))
                 (("-lstdc++" stdc)
                  (string-append "-L" d-c "/lib\","
                                 " \"-ldouble-conversion\","
                                 " \"" stdc)))
               #t)))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest"))))))
    (native-inputs
     `(("double-conversion" ,double-conversion)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/ultrajson/ultrajson")
    (synopsis "Ultra fast JSON encoder and decoder for Python")
    (description
     "UltraJSON is an ultra fast JSON encoder and decoder written in pure C with
bindings for Python 3.")
    (license license:bsd-3)))

(define-public python-ujson-1
  (package
    (inherit python-ujson)
    (version "1.35")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ujson" version))
       (sha256
        (base32
         "11jz5wi7mbgqcsz52iqhpyykiaasila4lq8cmc2d54bfa3jp6q7n"))))
    (arguments
     '(#:phases %standard-phases))
    (native-inputs '())
    (home-page "http://www.esn.me")
    (description
     "UltraJSON is an ultra fast JSON encoder and decoder written in pure C with
bindings for Python 2.5+ and 3.")))

(define-public python2-ujson-1
  (package-with-python2 python-ujson-1))

(define-public python-iocapture
  ;; The latest release is more than a year older than this commit.
  (let ((commit "fdc021c431d0840303908dfc3ca8769db383595c")
        (revision "1"))
    (package
      (name "python-iocapture")
      (version "0.1.2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/oinume/iocapture")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mkbhqibxvgwg0p7slr8dfraa3g2s6bsayladhax2jccwj4kcndz"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'check)
           (add-after 'install 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (add-installed-pythonpath inputs outputs)
               (invoke "py.test" "-v" "tests")
               #t)))))
      (propagated-inputs
       `(("python-flexmock" ,python-flexmock)
         ("python-pytest" ,python-pytest)
         ("python-pytest-cov" ,python-pytest-cov)
         ("python-six" ,python-six)))
      (home-page "https://github.com/oinume/iocapture")
      (synopsis "Python capturing tool for stdout and stderr")
      (description
       "This package helps you to capture the standard out (stdout) and the
standard error channel (stderr) in your program.")
      (license license:expat))))

(define-public python-argh
  ;; There are 21 commits since the latest release containing important
  ;; improvements.
  (let ((commit "dcd3253f2994400a6a58a700c118c53765bc50a4")
        (revision "1"))
    (package
      (name "python-argh")
      (version (git-version "0.26.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/neithere/argh")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1p5h3dnpbsjmqrvil96s71asc6i3gpinmbrabqmwnrsxprz7r3ns"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-iocapture" ,python-iocapture)
         ("python-mock" ,python-mock)
         ("python-pytest" ,python-pytest)
         ("python-pytest-cov" ,python-pytest-cov)
         ("python-pytest-xdist" ,python-pytest-xdist)
         ("python-tox" ,python-tox)))
      (home-page "https://github.com/neithere/argh/")
      (synopsis "Argparse wrapper with natural syntax")
      (description
       "python-argh is a small library that provides several layers of
abstraction on top of @code{python-argparse}.  The layers can be mixed.  It is
always possible to declare a command with the highest possible (and least
flexible) layer and then tune the behaviour with any of the lower layers
including the native API of @code{python-argparse}.")
      (license license:lgpl3+))))

(define-public python-ppft
  (package
    (name "python-ppft")
    (version "1.6.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ppft" version))
       (sha256
        (base32
         "1z1invkhszc5d2mvgr221v7cszzifcc77mz0pv3wjp6x5q2768cy"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ; there are none
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://pypi.org/project/ppft/")
    (synopsis "Fork of Parallel Python")
    (description
     "This package is a fork of Parallel Python.  The Parallel Python
module (@code{pp}) provides an easy and efficient way to create
parallel-enabled applications for @dfn{symmetric multiprocessing} (SMP)
computers and clusters.  It features cross-platform portability and dynamic
load balancing.")
    (license license:bsd-3)))

(define-public python-pox
  (package
    (name "python-pox")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pox" version))
       (sha256
        (base32
         "0y17ckc2p6i6709s279sjdj4q459mpcc38ymg9zv9y6vl6jf3bq6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (mkdir-p "/tmp/guix")
             (setenv "SHELL" "bash")
             (setenv "USERNAME" "guix")
             (setenv "HOME" "/tmp/guix") ; must end on USERNAME...
             (invoke "py.test" "-vv")
             #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("which" ,which)))
    (home-page "https://pypi.org/project/pox/")
    (synopsis "Python utilities for filesystem exploration and automated builds")
    (description
     "Pox provides a collection of utilities for navigating and manipulating
filesystems.  This module is designed to facilitate some of the low level
operating system interactions that are useful when exploring a filesystem on a
remote host.  Pox provides Python equivalents of several shell commands such
as @command{which} and @command{find}.  These commands allow automated
discovery of what has been installed on an operating system, and where the
essential tools are located.")
    (license license:bsd-3)))

(define-public python-pathos
  (package
    (name "python-pathos")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pathos" version))
       (sha256
        (base32
         "0in8hxdz7k081ijn6q94gr39ycy7363sx4zysmbwyvd7snqjrbi1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append (getcwd) ":" (getenv "PYTHONPATH")))
             (invoke "python" "./tests/__main__.py"))))))
    (propagated-inputs
     `(("python-dill" ,python-dill)
       ("python-multiprocess" ,python-multiprocess)
       ("python-pox" ,python-pox)
       ("python-ppft" ,python-ppft)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://pypi.org/project/pathos/")
    (synopsis
     "Parallel graph management and execution in heterogeneous computing")
    (description
     "Python-pathos is a framework for heterogeneous computing.  It provides a
consistent high-level interface for configuring and launching parallel
computations across heterogeneous resources.  Python-pathos provides configurable
launchers for parallel and distributed computing, where each launcher contains
the syntactic logic to configure and launch jobs in an execution environment.")
    (license license:bsd-3)))

(define-public python-flit
  (package
    (name "python-flit")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flit" version))
       (sha256
        (base32
         "0h5vvmqinqzn97mr3ix7zx53af9ad4fimjjwqpx88yp8qhz4r5bc"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; XXX: Check requires network access.
    (home-page "https://flit.readthedocs.io/")
    (synopsis
     "Simple packaging tool for simple packages")
    (description
     "Flit is a simple way to put Python packages and modules on PyPI.  Flit
packages a single importable module or package at a time, using the import
name as the name on PyPI.  All subpackages and data files within a package
are included automatically.")
    (license license:bsd-3)))

(define-public python-pathtools
  (package
    (name "python-pathtools")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pathtools" version))
       (sha256
        (base32
         "1h7iam33vwxk8bvslfj4qlsdprdnwf8bvzhqh3jq5frr391cadbw"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/gorakhargosh/pathtools")
    (synopsis "Path utilities for Python")
    (description "Pattern matching and various utilities for file systems
paths.")
    (license license:expat)))

(define-public python-fastentrypoints
  (package
    (name "python-fastentrypoints")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastentrypoints" version))
       (sha256
        (base32
         "02s1j8i2dzbpbwgq2a3fiqwm3cnmhii2qzc0k42l0rdxd4a4ya7z"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/ninjaaron/fast-entry_points")
    (synopsis
     "Makes entry_points specified in setup.py load more quickly")
    (description
     "Using entry_points in your setup.py makes scripts that start really
slowly because it imports pkg_resources.  This package allows such setup
scripts to load entry points more quickly.")
    (license license:bsd-3)))

(define-public python-funcparserlib
  (package
    (name "python-funcparserlib")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "funcparserlib" version))
       (sha256
        (base32
         "07f9cgjr3h4j2m67fhwapn8fja87vazl58zsj4yppf9y3an2x6dp"))))
    (native-inputs
     `(("python-tox" ,python-tox)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "tox"))))))
    (build-system python-build-system)
    (home-page
     "https://github.com/vlasovskikh/funcparserlib")
    (synopsis
     "Recursive descent parsing library based on functional combinators")
    (description
     "This package is a recursive descent parsing library for Python based on
functional combinators.  Parser combinators are just higher-order functions
that take parsers as their arguments and return them as result values.")
    (license license:expat)))

(define-public python-speg
  (package
    (name "python-speg")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "speg" version ".zip"))
       (sha256
        (base32 "0w9y4jf4787dzhy6rvhwi0mpl0r8qkqmqmyv2hpwdpv8w53yzjqh"))))
    (arguments
     `(#:tests? #f))                    ;FIXME: tests fail, not sure why
    (native-inputs
     `(("unzip" ,unzip)))
    (build-system python-build-system)
    (home-page "https://github.com/avakar/speg")
    (synopsis "PEG-based parser interpreter with memoization")
    (description "This package is a PEG-based parser and interpreter with
memoization.")
    (license license:expat)))

(define-public python-cson
  (package
    (name "python-cson")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cson" version))
       (sha256
        (base32 "00cyvigg4npbph39ghkg77xbxisa6plf75vii24igxfizik0337f"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-speg" ,python-speg)))
    (home-page "https://github.com/avakar/pycson")
    (synopsis "Parser for Coffeescript Object Notation (CSON)")
    (description "This package is a parser for Coffeescript Object
Notation (CSON).")
    (license license:expat)))

(define-public python-asynctest
  (package
    (name "python-asynctest")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asynctest" version))
       (sha256
        (base32
         "1b3zsy7p84gag6q8ai2ylyrhx213qdk2h2zb6im3xn0m5n264y62"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "-X" "dev" "-m" "unittest" "-v" "test")))
         (add-after 'unpack 'disable-tests
           (lambda* _
             (substitute* "test/test_selector.py"
               ;; XXX: This test fails for unknown reason inside the build
               ;; environment.
               (("def test_events_watched_outside_test_are_ignored")
                "@unittest.skip('disabled by guix')
    def test_events_watched_outside_test_are_ignored")))))))
    (home-page "https://github.com/Martiusweb/asynctest")
    (synopsis "Extension of unittest for testing asyncio libraries")
    (description
     "The package asynctest is built on top of the standard unittest module
and cuts down boilerplate code when testing libraries for asyncio.")
    (license license:asl2.0)))

(define-public python-aionotify
  (package
    (name "python-aionotify")
    (version "0.2.0")
    (source
     (origin
       ;; Source tarball on PyPi lacks tests
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rbarrois/aionotify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sk9i8czxgsbrswsf1nlb4c82vgnlzi8zrvrxdip92w2z8hqh43y"))
       (patches (search-patches "python-aionotify-0.2.0-py3.8.patch"))))
    (build-system python-build-system)
    (home-page "https://github.com/rbarrois/aionotify")
    (synopsis "Asyncio-powered inotify library")
    (description
     "@code{aionotify} is a simple, asyncio-based inotify library.")
    (license license:bsd-3)))

(define-public python-forbiddenfruit
  (package
    (name "python-forbiddenfruit")
    (version "0.1.3")
    (source
     (origin
       ;; Source tarball on PyPi lacks Makefile that builds and runs tests
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clarete/forbiddenfruit")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fp2xvdqpi910j9r3q68x38phpxbm700gjdi2m2j5gs91xdnyyh2"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "make" "SKIP_DEPS=1"))))))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-coverage" ,python-coverage)))
    (home-page "https://github.com/clarete/forbiddenfruit")
    (synopsis "Patch python built-in objects")
    (description "This project allows Python code to extend built-in types.")
    (license (list license:gpl3+ license:expat))))

(define-public python-shouldbe
  (package
    (name "python-shouldbe")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "shouldbe" version))
       (sha256
        (base32
         "16zbvjxf71dl4yfbgcr6idyim3mdrfvix1dv8b95p0s9z07372pj"))
       (patches (search-patches "python-shouldbe-0.1.2-cpy3.8.patch"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-forbiddenfruit" ,python-forbiddenfruit)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/directxman12/should_be")
    (synopsis "Python Assertion Helpers inspired by Shouldly")
    (description
     "Python Assertion Helpers inspired by Shouldly.")
    (license license:isc)))

(define-public python-k5test
  (package
    (name "python-k5test")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "k5test" version))
       (sha256
        (base32
         "1lqp3jgfngyhaxjgj3n230hn90wsylwilh120yjf62h7b1s02mh8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ;; `which`, `kadmin.local` binaries called inside library
       ("which" ,which)
       ("mit-krb5" ,mit-krb5)))
    (native-inputs `(("mit-krb5" ,mit-krb5)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* _
             (substitute* "k5test/realm.py"
               (("'kadmin_local'") "'kadmin.local'")))))))
    (home-page "https://github.com/pythongssapi/k5test")
    (synopsis "Library for setting up self-contained Kerberos 5 environments")
    (description
     "@code{k5test} is a library for setting up self-contained Kerberos 5
environments, and running Python unit tests inside those environments.  It is
based on the file of the same name found alongside the MIT Kerberos 5 unit
tests.")
    (license license:isc)))

(define-public python-gssapi
  (package
    (name "python-gssapi")
    (version "1.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gssapi" version))
       (sha256
        (base32
         "02i5s7998dg5kcr4m0xwamd8vjqk1816xbzldyp68l91f6bynwcr"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-decorator" ,python-decorator)
       ("python-six" ,python-six)))
    (inputs
     `(("mit-krb5" ,mit-krb5)))
    ;; for tests
    (native-inputs
     `(("python-shouldbe" ,python-shouldbe)
       ("python-parameterized" ,python-parameterized)
       ("python-k5test" ,python-k5test)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/pythongssapi/python-gssapi")
    (synopsis "Python GSSAPI Wrapper")
    (description
     "Python-GSSAPI provides both low-level and high level wrappers around the
GSSAPI C libraries.  While it focuses on the Kerberos mechanism, it should
also be useable with other GSSAPI mechanisms.")
    (license license:isc)))

(define-public python-check-manifest
  (package
    (name "python-check-manifest")
    (version "0.37")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "check-manifest" version))
        (sha256
         (base32
          "0lk45ifdv2cpkl6ayfyix7jwmnxa1rha7xvb0ih5999k115wzqs4"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("git" ,git)))
    (home-page "https://github.com/mgedmin/check-manifest")
    (synopsis "Check MANIFEST.in in a Python source package for completeness")
    (description "Python package can include a MANIFEST.in file to help with
sending package files to the Python Package Index.  This package checks that
file to ensure it completely and accurately describes your project.")
    (license license:expat)))

(define-public python-android-stringslib
  (package
    (name "python-android-stringslib")
    (version "0.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://framagit.org/tyreunom/python-android-strings-lib")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0gij55qzzq1h83kfpvhai1vf78kkhyvxa6l17m2nl24454lhfin4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://framagit.org/tyreunom/python-android-strings-lib")
    (synopsis "Android strings.xml support")
    (description "Android Strings Lib provides support for android's strings.xml
files.  These files are used to translate strings in android apps.")
    (license license:expat)))

(define-public python-watchdog
  (package
    (name "python-watchdog")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "watchdog" version))
        (sha256
         (base32
          "07cnvvlpif7a6cg4rav39zq8fxa5pfqawchr46433pij0y6napwn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-failing
           (lambda _
             (delete-file "tests/test_inotify_buffer.py")
             (delete-file "tests/test_snapshot_diff.py")
             #t)))))
    (propagated-inputs
     `(("python-argh" ,python-argh)
       ("python-pathtools" ,python-pathtools)
       ("python-pyyaml" ,python-pyyaml)))
    (native-inputs
     `(("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-timeout" ,python-pytest-timeout)))
    (home-page "https://github.com/gorakhargosh/watchdog")
    (synopsis "Filesystem events monitoring")
    (description "This package provides a way to monitor filesystem events
such as a file modification and trigger an action.  This is similar to inotify,
but portable.")
    (license license:asl2.0)))

(define-public python-wget
  (package
    (name "python-wget")
    (version "3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wget" version ".zip"))
       (sha256
        (base32
         "0qb0y7ipby42m4m7h0ipazpdyc3bn9xi46lvifcwwl5albn31rim"))))
    (build-system python-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (home-page "https://bitbucket.org/techtonik/python-wget/")
    (synopsis "Pure Python download utility")
    (description "The python-wget library provides an API to download files
with features similar to the @command{wget} utility.")
    (license license:unlicense)))

(define-public offlate
  (package
    (name "offlate")
    (version "0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://framagit.org/tyreunom/offlate")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "13pqnbl05wcyldfvl75fp89vjgwsvxyc69vhnb17kkha2rc2k1h7"))))
    (build-system python-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("python-android-stringslib" ,python-android-stringslib)
        ("python-dateutil" ,python-dateutil)
        ("python-gitlab" ,python-gitlab)
        ("python-lxml" ,python-lxml)
        ("python-polib" ,python-polib)
        ("python-pyenchant" ,python-pyenchant)
        ("python-pygit2" ,python-pygit2)
        ("python-pygithub" ,python-pygithub)
        ("python-pyqt" ,python-pyqt)
        ("python-requests" ,python-requests)
        ("python-ruamel.yaml" ,python-ruamel.yaml)
        ("python-translation-finder" ,python-translation-finder)
        ("python-watchdog" ,python-watchdog)))
    (native-inputs
     `(("qttools" ,qttools)))
    (home-page "https://framagit.org/tyreunom/offlate")
    (synopsis "Offline translation interface for online translation tools")
    (description "Offlate offers a unified interface for different translation
file formats, as well as many different online translation platforms.  You can
use it to get work from online platforms, specialized such as the Translation
Project, or not such a gitlab instance when your upstream doesn't use any
dedicated platform.  The tool proposes a unified interface for any format and
an upload option to send your work back to the platform.")
    (license license:gpl3+)))

(define-public python-titlecase
  (package
    (name "python-titlecase")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "titlecase" version))
       (sha256
        (base32
         "0486i99wf8ssa7sgn81fn6fv6i4rhhq6n751bc740b3hzfbpmpl4"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/ppannuto/python-titlecase")
    (synopsis "Capitalize strings similar to book titles")
    (description
     "Python-Titlecase is a Python port of John Gruber's titlecase.pl.
It capitalizes (predominantly English) strings in a way that is similar to
book titles, using the New York Times Manual of Style to leave certain words
lowercase.")
    (license license:expat)))

(define-public python-pypng
  (package
    (name "python-pypng")
    (version "0.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pypng" version))
       (sha256
        (base32 "02qpa22ls41vwsrzw9r9qhj1nhq05p03hb5473pay6y980s86chh"))))
    (build-system python-build-system)
    (home-page "https://github.com/drj11/pypng")
    (synopsis "Pure Python PNG image encoder/decoder")
    (description
     "The PyPNG module implements support for PNG images.  It reads and writes
PNG files with all allowable bit depths (1/2/4/8/16/24/32/48/64 bits per
pixel) and colour combinations: greyscale (1/2/4/8/16 bit); RGB, RGBA,
LA (greyscale with alpha) with 8/16 bits per channel; colour mapped
images (1/2/4/8 bit).  Adam7 interlacing is supported for reading and writing.
A number of optional chunks can be specified (when writing) and
understood (when reading): tRNS, bKGD, gAMA.

PyPNG is not a high level toolkit for image processing (like PIL) and does not
aim at being a replacement or competitor.  Its strength lies in fine-grained
extensive support of PNG features.  It can also read and write Netpbm PAM
files, with a focus on its use as an intermediate format for implementing
custom PNG processing.")
    (license license:expat)))

(define-public python-fuzzywuzzy
  (package
    (name "python-fuzzywuzzy")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fuzzywuzzy" version))
       (sha256
        (base32
         "1s00zn75y2dkxgnbw8kl8dw4p1mc77cv78fwfa4yb0274s96w0a5"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-hypothesis" ,python-hypothesis)
       ("python-pycodestyle" ,python-pycodestyle)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-levenshtein" ,python-levenshtein)))
    (home-page "https://github.com/seatgeek/fuzzywuzzy")
    (synopsis "Fuzzy string matching in Python")
    (description "Approximate string matching using
@emph{Levenshtein Distance} to calculate the differences between
sequences.")
    (license license:gpl2)))

(define-public python2-fuzzywuzzy
  (package-with-python2 python-fuzzywuzzy))

(define-public python-block-tracing
  (package
    (name "python-block-tracing")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "block_tracing" version))
        (sha256
         (base32
          "0s2y729qr5rs7n506qfh8cssk8m2bi6k2y5vbrh2z3raf2d01alz"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; no tests
    (home-page "https://github.com/rianhunter/block_tracing")
    (synopsis "Protect process memory")
    (description
     "@code{block_tracing} is a tiny Python library that can be used to
prevent debuggers and other applications from inspecting the memory within
your process.")
    (license license:expat)))

(define-public python-gcovr
  (package
    (name "python-gcovr")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gcovr" version))
       (sha256
        (base32
          "0gyady7x3v3l9fm1zan0idaggqqcm31y7g5vxk7h05p5h7f39bjs"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-lxml" ,python-lxml)
       ("python-jinja2" ,python-jinja2)))
    (home-page "https://gcovr.com/")
    (synopsis "Utility for generating code coverage results")
    (description
      "Gcovr provides a utility for managing the use of the GNU gcov
utility and generating summarized code coverage results.  It is inspired
by the Python coverage.py package, which provides a similar utility for
Python.")
    (license license:bsd-3)))

(define-public python-owslib
  (package
    (name "python-owslib")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "OWSLib" version))
       (sha256
        (base32 "0v8vg0naa9rywvd31cpq65ljbdclpsrx09788v4xj7lg10np8nk0"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; TODO: package dependencies required for tests.
    (synopsis "Interface for Open Geospatial Consortium web service")
    (description
     "OWSLib is a Python package for client programming with Open Geospatial
Consortium (OGC) web service (hence OWS) interface standards, and their related
content models.")
    (home-page "https://geopython.github.io/OWSLib/")
    (license license:bsd-3)))

(define-public python-docusign-esign
  (package
    (name "python-docusign-esign")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "docusign_esign" version))
              (sha256
               (base32
                "01f3h03vc97syjlmqyl7xa5j90pzgmwpspc5a0gra9saynnbkx37"))))
    (build-system python-build-system)
    ;; Testing requires undocumented setup changes, and so testing is disabled here.
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("python-certifi", python-certifi)
        ("python-six", python-six)
        ("python-dateutil", python-dateutil)
        ("python-urllib3", python-urllib3)
        ("python-pyjwt", python-pyjwt)
        ("python-cryptography", python-cryptography)
        ("python-nose", python-nose)))
    (synopsis "DocuSign Python Client")
    (description "The Official DocuSign Python Client Library used to interact
 with the eSign REST API.  Send, sign, and approve documents using this client.")
    (home-page "https://www.docusign.com/devcenter")
    (license license:expat)))

(define-public python-xattr
  (package
    (name "python-xattr")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xattr" version))
       (sha256
        (base32
         "0i4xyiqbhjz2g16zbim17zjdbjkw79xsw8k59942vvq4is1cmfxh"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cffi" ,python-cffi)))
    (home-page "https://github.com/xattr/xattr")
    (synopsis
     "Python wrapper for extended filesystem attributes")
    (description "This package provides a Python wrapper for using extended
filesystem attributes.  Extended attributes extend the basic attributes of files
and directories in the file system.  They are stored as name:data pairs
associated with file system objects (files, directories, symlinks, etc).")
    (license license:expat)))

(define-public python-json-logger
  (package
    (name "python-json-logger")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-json-logger" version))
       (sha256
        (base32
         "10g2ya6nsvn5vxzvq2wb8q4d43i3d7756i5rxyjna6d0y9i138xp"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/madzak/python-json-logger")
    (synopsis "JSON log formatter in Python")
    (description "This library allows standard Python logging to output log data
as JSON objects.  With JSON we can make our logs more readable by machines and
we can stop writing custom parsers for syslog-type records.")
    (license license:bsd-3)))

(define-public python-daiquiri
  (package
    (name "python-daiquiri")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "daiquiri" version))
       (sha256
        (base32
         "1qmank3c217ddiig3xr8ps0mqaydcp0q5a62in9a9g4zf72zjnqd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-json-logger" ,python-json-logger)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-six" ,python-six)))
    (home-page "https://github.com/jd/daiquiri")
    (synopsis
     "Library to configure Python logging easily")
    (description "The daiquiri library provides an easy way to configure
logging in Python.  It also provides some custom formatters and handlers.")
    (license license:asl2.0)))

(define-public python-pifpaf
  (package
    (name "python-pifpaf")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pifpaf" version))
       (sha256
        (base32
         "1gy9p4nqf70fh38wn4icyfm7i9wrvx22wnjpg71g89wxbz27igaa"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "setup.py" "testr" "--slowest"
                     "--testr-args=until-failure"))))))
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-daiquiri" ,python-daiquiri)
       ("python-fixtures" ,python-fixtures)
       ("python-jinja2" ,python-jinja2)
       ("python-pbr" ,python-pbr)
       ("python-psutil" ,python-psutil)
       ("python-six" ,python-six)
       ("python-xattr" ,python-xattr)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-os-testr" ,python-os-testr)
       ("python-requests" ,python-requests)
       ("python-testrepository" ,python-testrepository)
       ("python-testtools" ,python-testtools)))
    (home-page "https://github.com/jd/pifpaf")
    (synopsis "Tools and fixtures to manage daemons for testing in Python")
    (description "Pifpaf is a suite of fixtures and a command-line tool that
starts and stops daemons for a quick throw-away usage.  This is typically
useful when needing these daemons to run integration testing.  It originally
evolved from its precursor @code{overtest}.")
    (license license:asl2.0)))

(define-public python-pytest-check-links
  (package
    (name "python-pytest-check-links")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       ;; URI uses underscores
       (uri (pypi-uri "pytest_check_links" version))
       (sha256
        (base32
         "12x3wmrdzm6wgk0vz02hb769h68nr49q47w5q1pj95pc89hsa34v"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-docutils" ,python-docutils)
       ("python-html5lib" ,python-html5lib)
       ("python-nbconvert" ,python-nbconvert)
       ("python-nbformat" ,python-nbformat)
       ("python-pytest" ,python-pytest)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-pbr-minimal" ,python-pbr-minimal)))
    (home-page "https://github.com/minrk/pytest-check-links")
    (synopsis "Check links in files")
    (description "This package provides a pytest plugin that checks URLs for
HTML-containing files.")
    (license license:bsd-3)))

(define-public python-json5
  (package
    (name "python-json5")
    (version "0.8.5")
    (source
     (origin
       ;; sample.json5 is missing from PyPi source tarball
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dpranke/pyjson5")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nyngj18jlkgvm1177lc3cj47wm4yh3dqigygvcvw7xkyryafsqn"))))
    (build-system python-build-system)
    (home-page "https://github.com/dpranke/pyjson5")
    (synopsis
     "Python implementation of the JSON5 data format")
    (description
     "JSON5 extends the JSON data interchange format to make it slightly more
usable as a configuration language.  This Python package implements parsing and
dumping of JSON5 data structures.")
    (license license:asl2.0)))

(define-public python-frozendict
  (package
    (name "python-frozendict")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "frozendict" version))
       (sha256
        (base32 "0ibf1wipidz57giy53dh7mh68f2hz38x8f4wdq88mvxj5pr7jhbp"))))
    (build-system python-build-system)
    (home-page "https://github.com/slezica/python-frozendict")
    (synopsis "Simple immutable mapping for Python")
    (description
     "@dfn{frozendict} is an immutable wrapper around dictionaries that
implements the complete mapping interface.  It can be used as a drop-in
replacement for dictionaries where immutability is desired.")
    (license license:expat)))

(define-public python-unpaddedbase64
  (package
    (name "python-unpaddedbase64")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matrix-org/python-unpaddedbase64")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0if3fjfxga0bwdq47v77fs9hrcqpmwdxry2i2a7pdqsp95258nxd"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/unpaddedbase64/")
    (synopsis "Encode and decode Base64 without “=” padding")
    (description
     "RFC 4648 specifies that Base64 should be padded to a multiple of 4 bytes
using “=” characters.  However this conveys no benefit so many protocols
choose to use Base64 without the “=” padding.")
    (license license:asl2.0)))

(define-public python-py-cpuinfo
  (package
    (name "python-py-cpuinfo")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py-cpuinfo" version))
       (sha256
        (base32
         "0045y6832gqjg63jmw0qj2jwyypgjwr7sfdq3lfv49b6fxpl5xic"))))
    (build-system python-build-system)
    (home-page "https://github.com/workhorsy/py-cpuinfo")
    (synopsis "Get CPU info with Python")
    (description
     "This Python module returns the CPU info by using the best sources of
information for your operating system.")
    (license license:expat)))

(define-public python-canonicaljson
  (package
    (name "python-canonicaljson")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "canonicaljson" version))
       (sha256
        (base32 "09cpacc8yvcc74i63pdmlfaahh77dnvbyw9zf29wml2zzwqfbg25"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-frozendict" ,python-frozendict)
       ("python-simplejson" ,python-simplejson)))
    (home-page "https://github.com/matrix-org/python-canonicaljson")
    (synopsis "Canonical JSON")
    (description
     "Deterministically encode JSON.

@itemize
@item Encodes objects and arrays as RFC 7159 JSON.
@item Sorts object keys so that you get the same result each time.
@item Has no insignificant whitespace to make the output as small as possible.
@item Escapes only the characters that must be escaped, U+0000 to
 U+0019 / U+0022 / U+0056, to keep the output as small as possible.
@item Uses the shortest escape sequence for each escaped character.
@item Encodes the JSON as UTF-8.
@item Can encode frozendict immutable dictionaries.
@end itemize")
    (license license:asl2.0)))

(define-public python-signedjson
  (package
    (name "python-signedjson")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "signedjson" version))
       (sha256
        (base32 "0280f8zyycsmd7iy65bs438flm7m8ffs1kcxfbvhi8hbazkqc19m"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-canonicaljson" ,python-canonicaljson)
       ("python-importlib-metadata" ,python-importlib-metadata)
       ("python-pynacl" ,python-pynacl)
       ("python-typing-extensions" ,python-typing-extensions)
       ("python-unpaddedbase64" ,python-unpaddedbase64)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/matrix-org/python-signedjson")
    (synopsis "Sign JSON objects with ED25519 signatures")
    (description
     "Sign JSON objects with ED25519 signatures.

@itemize
@item More than one entity can sign the same object.
@item Each entity can sign the object with more than one key making it easier to
rotate keys
@item ED25519 can be replaced with a different algorithm.
@item Unprotected data can be added to the object under the @dfn{\"unsigned\"}
key.
@end itemize")
    (license license:asl2.0)))

(define-public python-daemonize
  (package
    (name "python-daemonize")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "daemonize" version))
       (sha256
        (base32 "1hwbl3gf9fdds9sc14zgjyjisjvxidrvqc11xlbb0b6jz17nw0nx"))))
    (build-system python-build-system)
    (home-page "https://github.com/thesharp/daemonize")
    (synopsis "Library for writing system daemons in Python")
    (description "Daemonize is a library for writing system daemons in Python.")
    (license license:expat)))

(define-public python-pymacaroons
  (package
    (name "python-pymacaroons")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pymacaroons" version))
       (sha256
        (base32 "1f0357a6g1h96sk6wy030xmc1p4rd80a999qvxd28v7nlm1blsqy"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-pynacl" ,python-pynacl)))
    (home-page "https://github.com/ecordell/pymacaroons")
    (synopsis "Python Macaroon Library")
    (description
     "Macaroons, like cookies, are a form of bearer credential.  Unlike opaque
tokens, macaroons embed caveats that define specific authorization
requirements for the target service, the service that issued the root macaroon
and which is capable of verifying the integrity of macaroons it receives.

Macaroons allow for delegation and attenuation of authorization.  They are
simple and fast to verify, and decouple authorization policy from the
enforcement of that policy.")
    (license license:expat)))

(define-public python-ldap3
  (package
    (name "python-ldap3")
    (version "2.7")
    (home-page "https://github.com/cannatag/ldap3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xw9fkqld21xsvdpaqir8ccc2l805xnn9gxahsnl70xzp3mwl0xv"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ;TODO: Tests need a real LDAP server to run
       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "nosetests" "-s" "test"))
                      #t)))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-gssapi" ,python-gssapi)
       ("python-pyasn1" ,python-pyasn1)))
    (synopsis "Python LDAP client")
    (description
     "LDAP3 is a strictly RFC 4510 conforming LDAP V3 pure Python client
library.")
    (license license:lgpl3+)))

(define-public python-boltons
  (package
    (name "python-boltons")
    (version "20.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "boltons" version))
       (sha256
        (base32
         "0lrr40qqj3ch8xarvyzbnbjs79pz5aywklllq53l347h1b8xnkg4"))))
    (build-system python-build-system)
    (home-page "https://github.com/mahmoud/boltons")
    (synopsis "Extensions to the Python standard library")
    (description
     "Boltons is a set of over 230 pure-Python utilities in the same spirit
as — and yet conspicuously missing from — the standard library, including:

@itemize
@item Atomic file saving, bolted on with fileutils
@item A highly-optimized OrderedMultiDict, in dictutils
@item Two types of PriorityQueue, in queueutils
@item Chunked and windowed iteration, in iterutils
@item Recursive data structure iteration and merging, with iterutils.remap
@item Exponential backoff functionality, including jitter, through
iterutils.backoff
@item A full-featured TracebackInfo type, for representing stack traces, in
tbutils
@end itemize")
    (license license:bsd-3)))

(define-public python-eliot
  (package
    (name "python-eliot")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "eliot" version))
       (sha256
        (base32 "0wabv7hk63l12881f4zw02mmj06583qsx2im0yywdjlj8f56vqdn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-journald-support
           (lambda _
             (for-each delete-file
                     '("eliot/tests/test_journald.py"
                       "eliot/journald.py"))
             #t))
         (add-after 'remove-journald-support 'remove-eliot-prettyprint-tests
           ;; remove command-line tool's tests. TODO eliot-prettyprint should
           ;; be installed and these tests should pass.
           (lambda _
             (delete-file "eliot/tests/test_prettyprint.py")
             #t)))))
    (propagated-inputs
     `(("python-boltons" ,python-boltons)
       ("python-pyrsistent" ,python-pyrsistent)
       ("python-six" ,python-six)
       ("python-zope-interface" ,python-zope-interface)))
    (native-inputs
     `(("python-black" ,python-black)
       ("python-coverage" ,python-coverage)
       ("python-dask" ,python-dask)
       ("python-flake8" ,python-flake8)
       ("python-hypothesis" ,python-hypothesis)
       ("python-pytest" ,python-pytest)
       ("python-setuptools" ,python-setuptools)
       ("python-sphinx" ,python-sphinx)
       ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)
       ("python-testtools" ,python-testtools)
       ("python-twine" ,python-twine)
       ("python-twisted" ,python-twisted)))
    (home-page "https://github.com/itamarst/eliot/")
    (synopsis "Eliot: the logging system that tells you why it happened")
    (description
     "@dfn{eliot} is a Python logging system that outputs causal chains of
actions: actions can spawn other actions, and eventually they either succeed
or fail. The resulting logs tell you the story of what your software did: what
happened, and what caused it.")
    (license license:asl2.0)))

(define-public python-pem
  (package
    (name "python-pem")
    (version "20.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pem" version))
       (sha256
        (base32
         "1xh88ss279fprxnzd10dczmqwjhppbyvljm33zrg2mgybwd66qr7"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-certifi" ,python-certifi)
       ("python-coverage" ,python-coverage)
       ("python-pretend" ,python-pretend)
       ("python-pyopenssl" ,python-pyopenssl)
       ("python-pytest" ,python-pytest)
       ("python-sphinx" ,python-sphinx)
       ("python-twisted" ,python-twisted)))
    (home-page "https://pem.readthedocs.io/")
    (synopsis "Easy PEM file parsing in Python")
    (description
     "This package provides a Python module for parsing and splitting PEM files.")
    (license license:expat)))

(define-public python-txsni
  ;; We need a few commits on top of 0.1.9 for compatibility with newer
  ;; Python and OpenSSL.
  (let ((commit "5014c141a7acef63e20fcf6c36fa07f0cd754ce1")
        (revision "0"))
    (package
      (name "python-txsni")
      (version (git-version "0.1.9" revision commit))
      (home-page "https://github.com/glyph/txsni")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page) (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0imfxx4yjj1lbq0n5ad45al3wvv4qv96sivnc1r51i66mxi658z8"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-pyopenssl" ,python-pyopenssl)
         ("python-service-identity" ,python-service-identity)
         ("python-twisted" ,python-twisted)))
      (synopsis "Run TLS servers with Twisted")
      (description
       "This package provides an easy-to-use SNI endpoint for use
with the Twisted web framework.")
      (license license:expat))))

(define-public python-txacme
  (package
    (name "python-txacme")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "txacme" version))
       (sha256
        (base32 "1cplx4llq7i508w6fgwjdv9di7rsz9k9irfmzdfbiz6q6a0ykf1d"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-acme" ,python-acme)
       ("python-attrs" ,python-attrs)
       ("python-eliot" ,python-eliot)
       ("python-josepy" ,python-josepy)
       ("python-pem" ,python-pem)
       ("python-treq" ,python-treq)
       ("python-twisted" ,python-twisted)
       ("python-txsni" ,python-txsni)))
    (native-inputs
     `(("python-fixtures" ,python-fixtures)
       ("python-hypothesis" ,python-hypothesis)
       ("python-mock" ,python-mock)
       ("python-service-identity"
        ,python-service-identity)
       ("python-testrepository" ,python-testrepository)
       ("python-testscenarios" ,python-testscenarios)
       ("python-testtools" ,python-testtools)))
    (home-page "https://github.com/twisted/txacme")
    (synopsis "Twisted implexmentation of the ACME protocol")
    (description
     "ACME is Automatic Certificate Management Environment, a protocol that
allows clients and certificate authorities to automate verification and
certificate issuance.  The ACME protocol is used by the free Let's Encrypt
Certificate Authority.

txacme is an implementation of the protocol for Twisted, the event-driven
networking engine for Python.")
    (license license:expat)))

(define-public python-pysaml2
  (package
    (name "python-pysaml2")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pysaml2" version))
       (sha256
        (base32
         "1h8cmxh9cvxhrdfmkh92wg6zpxmhi2fixq1cy4hxismmaar7bsny"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)
       ("python-dateutil" ,python-dateutil)
       ("python-defusedxml" ,python-defusedxml)
       ("python-pyopenssl" ,python-pyopenssl)
       ("python-pytz" ,python-pytz)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (home-page "https://idpy.org")
    (synopsis "Python implementation of SAML Version 2 Standard")
    (description
     "PySAML2 is a pure python implementation of SAML Version 2 Standard.
It contains all necessary pieces for building a SAML2 service provider or
an identity provider.  The distribution contains examples of both.

This package was originally written to work in a WSGI environment, but
there are extensions that allow you to use it with other frameworks.")
    (license license:asl2.0)))

(define-public python-click-plugins
  (package
    (name "python-click-plugins")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "click-plugins" version))
       (sha256
        (base32 "0jr6bxj67vg988vkm6nz8jj98v9lg46bn49lkhak3n598jbrkas6"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-click" ,python-click)))
    (synopsis "Extension for Click to register external CLI commands")
    (description "This package provides n extension module for Click to
register external CLI commands via setuptools entry-points.")
    (home-page "https://github.com/click-contrib/click-plugins")
    (license license:bsd-3)))

(define-public python-diceware
  (package
    (name "python-diceware")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "diceware" version))
       (sha256
        (base32
         "0klb0ysybzlh2wihvir82hgq62v0jjmlcqklwajyms7c0p529yby"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/ulif/diceware/")
    (synopsis "Generates memorable passphrases")
    (description "This package generates passphrases by concatenating words
randomly picked from wordlists.  It supports several sources of
randomness (including real life dice) and different wordlists (including
cryptographically signed ones).")
    (license license:gpl3+)))

(define-public pyzo
  (package
    (name "pyzo")
    (version "4.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyzo" version))
       (sha256
        (base32 "1zplxcb78qy8qibifmnsx5i9gnlfmw9n6nr4yflsabpxw57mx4m1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-home-directory
           (lambda _
             ;; Tests fail with "Permission denied: '/homeless-shelter'".
             (setenv "HOME" "/tmp")
             #t)))
       ;; Tests fail with "Uncaught Python exception: invalid literal for
       ;; int() with base 10: 'test'".
       #:tests? #f))
    (propagated-inputs
     `(("python-pyqt" ,python-pyqt)))
    (home-page "https://pyzo.org")
    (synopsis
     "Python IDE for scientific computing")
    (description
     "Pyzo is a Python IDE focused on interactivity and introspection,
which makes it very suitable for scientific computing.  Its practical
design is aimed at simplicity and efficiency.

It consists of two main components, the editor and the shell, and uses
a set of pluggable tools to help the programmer in various ways.  Some
example tools are source structure, project manager, interactive help,
workspace...")
    (license license:bsd-2)))

(define-public python-osc
  (package
    (name "python-osc")
    (version "1.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-osc" version))
        (sha256
          (base32
            "0cnh0z5lnng7fh48nmfaqqn8j25k13gkd4rhxd3m6sjqiix9s3vn"))))
    (build-system python-build-system)
    (home-page "https://github.com/attwad/python-osc")
    (synopsis "Open Sound Control server and client implementations")
    (description
      "@code{python-osc} is a pure Python library with no external
dependencies.  It implements the @uref{http://opensoundcontrol.org/spec-1_0,
Open Sound Control 1.0} specification.")
    (license license:unlicense)))

(define-public python-voluptuous
  (package
    (name "python-voluptuous")
    (version "0.11.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "voluptuous" version))
        (sha256
          (base32
            "0mplkcpb5d8wjf8vk195fys4y6a3wbibiyf708imw33lphfk9g1a"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/alecthomas/voluptuous")
    (synopsis "Python data validation library")
    (description
     "Voluptuous is a Python data validation library.  It is primarily
intended for validating data coming into Python as JSON, YAML, etc.")
    (license license:bsd-3)))

(define-public python-cmd2
  (package
    (name "python-cmd2")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cmd2" version))
       (sha256
        (base32
         "1f18plbc9yyvhn0js3d2bii9yld8zfl775gxsaw9jza5pmlg9ss2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-attrs" ,python-attrs)
       ("python-colorama" ,python-colorama)
       ("python-pyperclip" ,python-pyperclip)
       ("python-wcwidth" ,python-wcwidth)))
    (native-inputs
     `(("python-codecov" ,python-codecov)
       ("python-coverage" ,python-coverage)
       ("python-doc8" ,python-doc8)
       ("python-flake8" ,python-flake8)
       ("python-invoke" ,python-invoke)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-sphinx" ,python-sphinx)
       ("python-sphinx-autobuild" ,python-sphinx-autobuild)
       ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)
       ("python-tox" ,python-tox)
       ("python-twine" ,python-twine)
       ("which" ,which)))
    (home-page "https://github.com/python-cmd2/cmd2")
    (synopsis "Tool for building interactive command line applications")
    (description
     "Cmd2 is a tool for building interactive command line applications in
Python.  Its goal is to make it quick and easy for developers to build
feature-rich and user-friendly interactive command line applications.  It
provides a simple API which is an extension of Python's built-in @code{cmd}
module.  @code{cmd2} provides a wealth of features on top of @code{cmd} to
make your life easier and eliminates much of the boilerplate code which would
be necessary when using @code{cmd}.")
    (license license:expat)))

(define-public python-pytidylib
  (package
    (name "python-pytidylib")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytidylib" version))
              (sha256
               (base32
                "1wqa9dv5d7swka14rnky862hc7dgk2g3dhlrz57hdn3hb7bwic92"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'qualify-libtidy
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libtidy (string-append (assoc-ref inputs "tidy")
                                           "/lib/libtidy.so")))
               (substitute* "tidylib/tidy.py"
                 (("ctypes\\.util\\.find_library\\('tidy'\\)")
                  (format #f "'~a'" libtidy)))
               #t))))))
    (inputs `(("tidy" ,tidy)))
    (home-page "https://github.com/countergram/pytidylib")
    (synopsis "Python wrapper for HTML Tidy library")
    (description
     "PyTidyLib is a Python package that wraps the HTML Tidy library.  This
allows you, from Python code, to “fix” invalid (X)HTML markup.")
    (license license:expat)))

(define-public python2-pytidylib
  (package-with-python2 python-pytidylib))

(define-public python-mujson
  (package
    (name "python-mujson")
    (version "1.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mujson" version))
        (sha256
         (base32
          "0wbj6r8yzsdx2b0kbldlkznr1a9nn33za2q9x3g0hbg420dwzn97"))))
    (build-system python-build-system)
    (home-page "https://github.com/mattgiles/mujson")
    (synopsis "Use the fastest JSON functions available at import time")
    (description "This packages selects the fastest JSON functions available
at import time.")
    (license license:expat)))

(define-public python-bashlex
  (package
    (name "python-bashlex")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bashlex" version))
       (sha256
        (base32
         "1z9g96fgsfpdwawp4sb5x6hbdhmda7kgmcrqlf9xx4bs1f8f14js"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pregenerate-yacc-tables
           (lambda _
             ;; parser.py caches tables, which attempts to write to site lib
             ;; see https://github.com/idank/bashlex/issues/51
             (invoke "python" "-c" "import bashlex"))))))
    (home-page
     "https://github.com/idank/bashlex")
    (synopsis "Python parser for bash")
    (description "@code{bashlex} is a Python port of the parser used
internally by GNU bash.

For the most part it's transliterated from C, the major differences are:

@itemize
@item it does not execute anything
@item it is reentrant
@item it generates a complete AST
@end itemize
")
    (license license:gpl3+)))

(define-public python-jinxed
  (package
    (name "python-jinxed")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jinxed" version))
        (sha256
         (base32
          "1n7vl03rhjd0xhjgbjlh8x9f8yfbhamcwkgvs4jg7g5qj8f0wk89"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((ncurses (assoc-ref inputs "ncurses")))
               (setenv "TERM" "LINUX")
               (setenv "TERMINFO" (string-append ncurses "/share/terminfo"))
               #t))))
       #:tests? #f)) ; _curses.error: setupterm: could not find terminal
    (native-inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://github.com/Rockhopper-Technologies/jinxed")
    (synopsis "Jinxed Terminal Library")
    (description
     "Jinxed is an implementation of a subset of the Python curses library.")
    (license license:mpl2.0)))

(define-public python-blessed
  (package
    (name "python-blessed")
    (version "1.17.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "blessed" version))
        (sha256
         (base32
          "1wdj342sk22hfrg0n91x2qnqsbzbiyq9y009v3pxnvfzn9bx0wbn"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; Don't get hung up on Windows test failures.
            (delete-file "blessed/win_terminal.py") #t))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jinxed" ,python-jinxed)
       ("python-six" ,python-six)
       ("python-wcwidth" ,python-wcwidth)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/jquast/blessed")
    (synopsis "Wrapper around terminal capabilities")
    (description
     "Blessed is a thin, practical wrapper around terminal styling, screen
positioning, and keyboard input.")
    (license license:expat)))

(define-public python-readme-renderer
  (package
    (name "python-readme-renderer")
    (version "26.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "readme_renderer" version))
        (sha256
         (base32
          "13fnrv7z3y0yfafzcjbl55cqxncvbxadr72ql4l29pgyvrqxpsfb"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-bleach" ,python-bleach)
       ("python-docutils" ,python-docutils)
       ("python-pygments" ,python-pygments)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/pypa/readme_renderer")
    (synopsis "Render README files in Warehouse")
    (description
     "Readme Renderer is a library that will safely render arbitrary README
files into HTML.  It is designed to be used in Warehouse to render the
@code{long_description} for packages.  It can handle Markdown, reStructuredText,
and plain text.")
    (license license:asl2.0)))

(define-public python-lazr-delegates
  (package
    (name "python-lazr-delegates")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lazr.delegates" version))
        (sha256
         (base32
          "1rdnl85j9ayp8n85l0ciip621j9dcziz5qnmv2m7krgwgcn31vfx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "setup.py" "nosetests"))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-zope-interface" ,python-zope-interface)))
    (home-page "https://launchpad.net/lazr.delegates")
    (synopsis "Easily write objects that delegate behavior")
    (description
     "The @code{lazr.delegates} package makes it easy to write objects that
delegate behavior to another object.  The new object adds some property or
behavior on to the other object, while still providing the underlying interface,
and delegating behavior.")
    (license license:lgpl3)))

(define-public python-lazr-config
  (package
    (name "python-lazr-config")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lazr.config" version))
        (sha256
         (base32
          "11xpddgyhyj7sf27wbmrq5lnqk21wnprx3ajycgwlxjamh6sgffd"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "-s" "-m" "nose" "-P" "lazr"))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-lazr-delegates" ,python-lazr-delegates)
       ("python-zope-interface" ,python-zope-interface)))
    (home-page "https://launchpad.net/lazr.config")
    (synopsis "Create configuration schemas and process and validate configurations")
    (description
     "The LAZR config system is typically used to manage process configuration.
Process configuration is for saying how things change when we run systems on
different machines, or under different circumstances.  This system uses ini-like
file format of section, keys, and values.  The config file supports inheritance
to minimize duplication of information across files.  The format supports schema
validation.")
    (license license:lgpl3)))

(define-public python-flufl-bounce
  (package
    (name "python-flufl-bounce")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flufl.bounce" version))
        (sha256
         (base32
          "01lg1b0jpf8605mzaz9miq3nray6s7a7gc8n4wzg5nsxl8fglcp4"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-atpublic" ,python-atpublic)
       ("python-zope-interface" ,python-zope-interface)))
    (native-inputs
     `(("python-nose2" ,python-nose2)))
    (home-page "https://fluflbounce.readthedocs.io/en/latest/")
    (synopsis "Email bounce detectors")
    (description "The @code{flufl.bounce} library provides a set of heuristics
and an API for detecting the original bouncing email addresses from a bounce
message.  Many formats found in the wild are supported, as are VERP and
RFC 3464.")
    (license (list license:asl2.0
                   license:lgpl3))))    ; only for setup_headers.py

(define-public python-flufl-i18n
  (package
    (name "python-flufl-i18n")
    (version "3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flufl.i18n" version))
        (sha256
         (base32
          "1flwpn1xhgc957zj3zxw92dhdjh0lsy0hdvzq32dzqpsajfsvq1r"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-atpublic" ,python-atpublic)))
    (home-page "https://flufli18n.readthedocs.io")
    (synopsis "API for Python internationalization")
    (description
     "This package provides a high level, convenient API for managing
internationalization translation contexts in Python application.  There is a
simple API for single-context applications, such as command line scripts which
only need to translate into one language during the entire course of thei
execution.  There is a more flexible, but still convenient API for multi-context
applications, such as servers, which may need to switch language contexts for
different tasks.")
    (license license:asl2.0)))

(define-public python-flufl-lock
  (package
    (name "python-flufl-lock")
    (version "4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flufl.lock" version))
        (sha256
         (base32
          "055941zyma3wfx25jhm8wcsghpv3jc3iwi1gdrdjhzcnfhn62lxq"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-atpublic" ,python-atpublic)
       ("python-psutil" ,python-psutil)))
    (home-page "https://flufllock.readthedocs.io")
    (synopsis "NFS-safe file locking with timeouts for POSIX systems")
    (description
     "The @dfn{flufl.lock} package provides NFS-safe file locking with
timeouts for POSIX systems.  It is similar to the @code{O_EXCL} option of the
@code{open} system call but uses a lockfile.  Lock objects support lock-breaking
and have a maximum lifetime built-in.")
    (license (list license:asl2.0
                   license:lgpl3))))    ; only for setup_helpers.py

(define-public python-flufl-testing
  (package
    (name "python-flufl-testing")
    (version "0.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flufl.testing" version))
        (sha256
         (base32
          "1nkm95mhcfhl4x5jgs6y97ikszaxsfh07nyawsih6cxxm6l62641"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose2" ,python-nose2)))
    (home-page "https://gitlab.com/warsaw/flufl.testing")
    (synopsis "Collection of test tool plugins")
    (description
     "This package contains a small collection of test tool plugins for
@code{nose2} and @code{flake8}.")
    (license license:asl2.0)))
