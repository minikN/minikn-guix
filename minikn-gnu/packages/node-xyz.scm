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

(define-module (minikn-gnu packages node-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system node))

(define-public node-intelephense
  (package
    (name "node-intelephense")
    (version "v1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmewburn/vscode-intelephense")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15k6fwfsk3m05wvmgn1vxhilk5pq2z3y2hixiaspbr00b8mw5k1q"))))
    (build-system node-build-system)
    (home-page "https://github.com/bmewburn/vscode-intelephense")
    (synopsis "PHP code intelligence for Visual Studio Code")
    (description "Intelephense is a high performance PHP language server packed full of essential features for productive PHP development")
    (license (license:non-copyleft "file://LICENSE.txt"
                                   "See LICENSE.txt in the distribution."))))
