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

(define-module (rekahsoft-gnu packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system emacs)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages mail)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;;;
;;; Emacs hacking.
;;;

(define-public emacs-treemacs-20201225
  (let ((commit "6b045fd585421ab3c9e1185c2508d34af700490b")
        (revision "1"))
    (package
      (inherit emacs-treemacs)
      (version (git-version "2.8.0" revision commit))
      (name "emacs-treemacs-20201225")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Alexander-Miller/treemacs")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0sifzkhyd4k2ffvf2gn6frg7qd28my8w7wy2cqqa4i9gxhbflbsj"))))
      (arguments (substitute-keyword-arguments (package-arguments emacs-treemacs)
                   ((#:phases phases)
                    `(alist-delete 'check ,phases)))))))

(define-public emacs-treemacs-extra-20201225
  (package
    (inherit emacs-treemacs-20201225)
    (name "emacs-treemacs-extra-20201225")
    (propagated-inputs
     `(,@(package-propagated-inputs emacs-treemacs-20201225)
       ("emacs-evil" ,emacs-evil)
       ("emacs-magit" ,emacs-magit)
       ("emacs-projectile" ,emacs-projectile)
       ("emacs-persp-mode" ,emacs-persp-mode)
       ("emacs-all-the-icons" ,emacs-all-the-icons-20201225)
       ("emacs-perspective" ,emacs-perspective)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments emacs-treemacs-20201225)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'chdir-elisp 'copy-extra
             (lambda _
               (copy-recursively "../extra" ".")))))))))

(define-public emacs-all-the-icons-20201225
  (let ((commit "6917b08f64dd8487e23769433d6cb9ba11f4152f")
        (revision "1"))
    (package
      (inherit emacs-all-the-icons)
      (version (git-version "4.0.1" revision commit))
      (name "emacs-all-the-icons-20201225")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/domtronn/all-the-icons.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jzpil1k5brg4dvy0fxibbwwb2hkni5fkxng4n0wfv6099b2zc68")))))))

(define-public emacs-doom-themes-20201225
  (let ((commit "3761dfce75144e49789b6576e187acd82e2731ee")
        (revision "1"))
    (package
      (inherit emacs-doom-themes)
      (version (git-version "2.1.6" revision commit))
      (name "emacs-doom-themes-20201225")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hlissner/emacs-doom-themes")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0a0lz9kkaaxj2r8xkcgyczz9pxq5hb4kd8mivqxmcj9572xs6v1r"))))
      (arguments
       (substitute-keyword-arguments
           (package-arguments emacs-doom-themes)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'remove-broken-themes
               (lambda _
                 (delete-file "themes/doom-plain-theme.el"))))))))))

(define-public emacs-helm-tramp
  (package
    (name "emacs-helm-tramp")
    (version "1.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://stable.melpa.org/packages/helm-tramp-"
               version
               ".el"))
        (sha256
          (base32
           "1nhlfq113ngcb74n76z8c2wnki14vls18mml36iwi8y9k63flkcd"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-helm" ,emacs-helm)))
    (home-page
      "https://github.com/masasam/emacs-helm-tramp")
    (synopsis
      "Tramp helm interface for ssh, docker, vagrant")
    (description
      "Provides interfaces of Tramp.
You can also use tramp with helm interface as root
If you use it with docker-tramp, you can also use docker with helm interface
If you use it with vagrant-tramp, you can also use vagrant with helm interface
")
    (license license:gpl3)))

(define-public emacs-rebox2
  (package
    (name "emacs-rebox2")
    (version "20121113.1300")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/rebox2-"
               version
               ".el"))
        (sha256
          (base32
           "0ji5x2hnjklz9y4jd11hcfncbcfzgq1dg8r3s7dkrf696y4ziwwy"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/lewang/rebox2")
    (synopsis
      "Inserting and modification of comment boxes in various styles")
    (description
      "Rebox2 provides major and minor modes for inserting and modification of
comment box is various text styles.")
    (license #f)))

;; TODO: ./rekahsoft-gnu/packages/emacs-xyz.scm:172:6: emacs-ibuffer-vc@20181225.2227: URI https://melpa.org/packages/ibuffer-vc-20181225.2227.el not reachable: 404 ("Not Found")
(define-public emacs-ibuffer-vc
  (package
    (name "emacs-ibuffer-vc")
    (version "20181225.2227")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/ibuffer-vc-"
               version
               ".el"))
        (sha256
          (base32
            "16nry1631g4draqh09r2rq92xpv5vsyxx9yd7r5i2m6kblqh4y3f"))))
    (build-system emacs-build-system)
    (home-page
      "https://github.com/purcell/ibuffer-vc")
    (synopsis
      "Group ibuffer's list by VC project, or show VC status")
    (description
      "Adds functionality to ibuffer for grouping buffers by their parent
vc root directory, and for displaying and/or sorting by the vc
status of listed files.
")
    (license license:gpl3)))

;; TODO: ./rekahsoft-gnu/packages/emacs-xyz.scm:198:6: emacs-highlight-indent-guides@20190108.3: URI https://melpa.org/packages/highlight-indent-guides-20190108.3.el not reachable: 404 ("Not Found")
(define-public emacs-highlight-indent-guides
  (package
    (name "emacs-highlight-indent-guides")
    (version "20190108.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/highlight-indent-guides-"
               version
               ".el"))
        (sha256
          (base32
            "02imngb4y6f9vp4jxiwdbdcjxllvjwaa2rlh61zy49n2wivrlwh0"))))
    (build-system emacs-build-system)
    (home-page
      "https://github.com/DarthFennec/highlight-indent-guides")
    (synopsis "Minor mode to highlight indentation")
    (description
      "This minor mode highlights indentation levels via font-lock.  Indent widths
are dynamically discovered, which means this correctly highlights in any
mode, regardless of indent width, even in languages with non-uniform
indentation such as Haskell.  This mode works properly around hard tabs and
mixed indentation, and it behaves well in large buffers.")
    (license #f)))

(define-public emacs-intel-hex-mode
  (package
    (name "emacs-intel-hex-mode")
    (version "20180423.31")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/intel-hex-mode-"
               version
               ".el"))
        (sha256
          (base32
           "1nvwjk686wna96srk3y9xniixas729x6m1rcjnc8lz1zkap9z9bv"))))
    (build-system emacs-build-system)
    (home-page
      "https://github.com/mschuldt/intel-hex-mode")
    (synopsis "Mode for Intel Hex files")
    (description
      "Use this mode for editing files in the intel hex format.  The
intel-hex-mode will do font locking, and calculate checksums.")
    (license license:gpl2)))

(define-public emacs-highlight-parentheses
  (package
    (name "emacs-highlight-parentheses")
    (version "20180704.1102")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/highlight-parentheses-"
               version
               ".el"))
        (sha256
          (base32
            "0dxnw7d966pm44a54rmd40vrrps56jaayrc8wwj7wxl2imdzqzbl"))))
    (build-system emacs-build-system)
    (home-page
      "https://github.com/tsdh/highlight-parentheses.el")
    (synopsis "Highlight surrounding parentheses")
    (description
      "Enable the mode using M-x highlight-parentheses-mode or by adding it to
a hook.")
    (license license:gpl2)))

(define-public emacs-quack
  (package
    (name "emacs-quack")
    (version "20181106.1301")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/quack-"
               version
               ".el"))
        (sha256
          (base32
           "0ph6zh91kasgbvi425a5m1hz94pxk6qa3srpy4khifsbn7cwiyga"))))
    (build-system emacs-build-system)
    (home-page "https://www.neilvandyke.org/quack/")
    (synopsis
      "Enhanced support for editing and running Scheme code")
    (description
      "Quack enhances Emacs support for Scheme programming.  The name
Quack was a play on DrScheme.")
    (license license:gpl2)))

(define-public emacs-hamlet-mode
  (package
    (name "emacs-hamlet-mode")
    (version "20131208.724")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/hamlet-mode-"
               version
               ".el"))
        (sha256
          (base32
           "1r6nxi3d6bmnxrjjsam79vsprfwlnbhpag98vb1hj05yqp13l55v"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
        ("emacs-s" ,emacs-s)))
    (home-page
      "https://github.com/lightquake/hamlet-mode")
    (synopsis "Hamlet editing mode")
    (description
      "An Emacs major mode for editing files written in Hamlet, a
Haskell compile-time HTML templating engine.  Currently it only provides
syntax highlighting.

Functions and variables with // in their name are private and may change or
go away at any time.")
    (license #f)))

;; TODO: ./rekahsoft-gnu/packages/emacs-xyz.scm:323:6: emacs-mu4e-maildirs-extension@20180606.812: URI https://melpa.org/packages/mu4e-maildirs-extension-20180606.812.el not reachable: 404 ("Not Found")
(define-public emacs-mu4e-maildirs-extension
  (package
    (name "emacs-mu4e-maildirs-extension")
    (version "20180606.812")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/mu4e-maildirs-extension-"
               version
               ".el"))
        (sha256
          (base32
            "0rcn8mkabhn3w010hchr3xg72z7815j8fshfrcgxxcf9kygsg85b"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-dash" ,emacs-dash)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "emacs" "-batch"
                     "--eval" "(provide 'mu4e)"
                     "--eval" "(defvar mu4e~main-buffer-name \"tests\")"
                     "--eval" "(defvar mu4e-mu-binary \"mu\")"
                     "--eval" "(require 'cl)"
                     "-l" "dash.el"
                     "-l" "mu4e-maildirs-extension.el"
                     "--eval" "(byte-compile-file \"mu4e-maildirs-extension.el\")"))))))
    (home-page
      "https://github.com/agpchil/mu4e-maildirs-extension")
    (synopsis
      "Show mu4e maildirs summary in mu4e-main-view")
    (description "Runs a mu command (async) in a shell process for each
maildir to count unread and total mails.")
    (license license:gpl3)))

(define-public emacs-helm-unicode
  (package
    (name "emacs-helm-unicode")
    (version "20180608.1407")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/helm-unicode-"
               version
               ".el"))
        (sha256
          (base32
           "0w0wvh9kwa4rj98ldi9ppsprw74926a1niih36dp7hbb61xmm7ny"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-helm" ,emacs-helm)))
    (home-page "https://github.com/bomgar/helm-unicode")
    (synopsis "Helm command for unicode characters.")
    (description
      "A helm command for looking up unicode characters by name.")
    (license #f)))

(define-public emacs-vimish-fold
  (package
    (name "emacs-vimish-fold")
    (version "20200524.1729")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/vimish-fold-"
               version
               ".el"))
        (sha256
          (base32
            "08p826zn4ris9b46r1d5z9v9jdj4lyr6kdmw4yhmnm9sk41z4bm4"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-f" ,emacs-f)))
    (home-page
      "https://github.com/matsievskiysv/vimish-fold")
    (synopsis "Fold text like in Vim")
    (description
      "This is a package to perform text folding like in Vim.  It has the
following features:

* folding of active regions;
* good visual feedback: it's obvious which part of text is folded;
* persistence by default: when you kill a buffer your folds don't
  disappear;
* persistence scales well, you can work on hundreds of files with lots of
  folds without adverse effects;
* it does not break indentation;
* folds can be toggled from folded state to unfolded and back very
  easily;
* quick navigation between existing folds;
* you can use mouse to unfold folds (good for beginners and not only for
  them);
* for fans of code@{avy package}: you can use code@{avy} to fold text with minimal
  number of key strokes!")
    (license #f)))

(define-public emacs-grapnel
  (package
    (name "emacs-grapnel")
    (version "20131001.1534")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/grapnel-"
               version
               ".el"))
        (sha256
          (base32
            "1vnd025v7am19bamp0y50lq9abf2rzrbjslppvbk1ybb3lvw8n7q"))))
    (build-system emacs-build-system)
    (home-page
      "https://www.github.com/leathekd/grapnel")
    (synopsis
      "HTTP request lib with flexible callback dispatch")
    (description
      "Grapnel is an HTTP request library that uses a curl subprocess and
offers flexible callback dispatch.  Not only can you pass in an
alist of request outcomes to callback functions (see below) but you
can also override the dispatch function itself if the default one
doesn't suit your needs.  Further, grapnel will build the query
string, request data (i.e., POST body), and headers from alists
that are passed in.

An example:
(grapnel-retrieve-url
 \"www.google.com\"
 '((success . (lambda (res hdrs) (message \"%s\" res)))
   (failure . (lambda (res hdrs) (message \"Fail: %s\" res)))
   (error   . (lambda (res err)  (message \"Err: %s\" err))))
 \"GET\"
 '((q . \"ASIN B001EN71CW\")))")
    (license license:gpl3)))

(define-public emacs-ix
  (package
    (name "emacs-ix")
    (version "20131027.1629")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/ix-"
               version
               ".el"))
        (sha256
          (base32
            "165nr8cz2y0mpcav0bkc8kak5zji4fayrs9v9wdzn0nksq6wdbls"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-grapnel" ,emacs-grapnel)
        ("curl" ,curl)))
    (home-page
      "https://www.github.com/theanalyst/ix.el")
    (synopsis
      "Emacs client for http://ix.io pastebin")
    (description
      "ix.el is a simple emacs client to http://ix.io cmdline pastebin. At
the moment using the code@{ix} command on a selection sends the
selection to ix.io, entire buffer is sent if selection is inactive,
on success the url is notified in the minibuffer as well as saved
in the kill ring.

It is recommended to set a user name and token so that you can
later delete or replace a paste.  Set this via the variables
code@{ix-user} and code@{ix-token} via M-x customize-group RET ix

Posts (if posted with user and token) can be deleted by code@{ix-delete} command
which prompts for post id (the string after http://ix.io/)

curl is used as the backend via grapnel http request library.")
    (license #f)))

(define-public persp-projectile
  (package
    (name "emacs-persp-projectile")
    (version "20180616.1944")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/persp-projectile-"
               version
               ".el"))
        (sha256
          (base32
            "1pc1v6rc44wl7sq0qchgc4lxiy8lr7zvp0hygcp95c1x0dkqs8jl"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-perspective" ,emacs-perspective)
        ("emacs-projectile" ,emacs-projectile)))
    (home-page "https://github.com/bbatsov/persp-projectile")
    (synopsis
      "Perspective integration with Projectile")
    (description
      "This library bridges perspective mode to the awesome library
Projectile.  The idea is to create a separate perspective when
switching project.  A perspective is an independent workspace for
Emacs, similar to multiple desktops in Gnome and MacOS.  I often
work on many projects at the same time, and using perspective and
projectile together allows me to easily know which project I'm
current in, and focus on files that only belong to current project
when switching buffer.

To use this library, put this file in your Emacs load path, and
call (require 'persp-projectile)

See perspective.el on github: https://github.com/nex3/perspective-el
")
    (license license:gpl3+)))

(define-public emacs-helm-rg
  (let ((commit "ee0a3c09da0c843715344919400ab0a0190cc9dc")
        (revision "1"))
    (package
      (name "emacs-helm-rg")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/cosmicexplorer/helm-rg")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0m4l894345n0zkbgl0ar4c93v8pyrhblk9zbrjrdr9cfz40bx2kd"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; TODO: Build is currently broken:
           ;;       https://github.com/cosmicexplorer/helm-rg/issues/20
           (delete 'build))))
      (native-inputs
       `(("emacs" ,emacs-minimal)))
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-helm" ,emacs-helm)))
      (home-page
       "https://github.com/cosmicexplorer/helm-rg")
      (synopsis "Helm interface to ripgrep")
      (description
       "Helm interface for ripgrep.")
      (license license:gpl3+))))

(define-public emacs-ace-link-2021-01-1
  (let ((commit "298f02f7dd117f9ec01f6aa2a2ddfecae0efb7f4")
        (revision "1"))
    (package
      (inherit emacs-ace-link)
      (name "emacs-ace-link-2021-01-1")
      (version (git-version "0.5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/abo-abo/ace-link")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1i243wfwrbxn00sh96248lpqfb7cvxqqwlc78nf8kim4ymylpp41")))))))

(define-public emacs-ace-mc
  (let ((commit "6877880efd99e177e4e9116a364576def3da391b")
        (revision "1"))
    (package
      (name "emacs-ace-mc")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mm--/ace-mc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "180licc7w5b6f42ifwvllbnmf3aq8cbr8jhkbk37lzick4sv10d2"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-multiple-cursors" ,emacs-multiple-cursors)
         ("emacs-ace-jump-mode" ,emacs-ace-jump-mode)))
      (home-page
       "https://github.com/mm--/ace-mc")
      (synopsis "Easily add and remove multiple cursors using ace jump mode")
      (description
       "Easily add and remove multiple cursors using ace jump mode.")
      (license license:gpl3+))))

(define-public emacs-org-roam-server
  (package
    (name "emacs-org-roam-server")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/org-roam/org-roam-server")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jp8mkqx1l3w166b16l2d5zsqjcc836bkclplgjk4laysb6msry8"))))
    (build-system emacs-build-system)
    (inputs `(("emacs" ,emacs)))
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-simple-httpd" ,emacs-simple-httpd)
       ("emacs-s" ,emacs-s)
       ("emacs-f" ,emacs-f)
       ("emacs-org" ,emacs-org)
       ("emacs-org-roam" ,emacs-org-roam)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'copy-assets
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site-lisp (string-append "/share/emacs/site-lisp/org-roam-server-" ,version))
                    (assets (string-append out site-lisp "/assets")))
               (copy-recursively "assets" assets)
               (copy-file "index.html"
                          (string-append out site-lisp "/index.html"))
               #t)))
         (add-after 'build 'install-application
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (emacs (assoc-ref inputs "emacs"))
                    (xapplications (string-append out "/share/applications")))
               (mkdir-p xapplications)
               (call-with-output-file
                   (string-append xapplications "/org-protocol.desktop")
                 (lambda (port)
                   (format port
                           "[Desktop Entry]~@
                            Name=Org-Protocol~@
                            Comment=Protocol which allows interaction between org-roam-server in emacs and a web browser~@
                            Exec=~a/bin/emacsclient %u~@
                            Icon=emacs-icon~@
                            Type=Application~@
                            Terminal=false~@
                            MimeType=x-scheme-handler/org-protocol~%"
                           emacs)))
               #t))))))
    (home-page
     "https://github.com/org-roam/org-roam-server")
    (synopsis "Web Application to Visualize the Org-Roam Database")
    (description
     "Web Application to Visualize the Org-Roam Database.")
    (license license:gpl3+)))

(define-public emacs-mu4e-dashboard
  (let ((commit "7956bf9631abfecdf4fbff168aa92064dc4886fb")
        (revision "1"))
    (package
      (name "emacs-mu4e-dashboard")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rougier/mu4e-dashboard")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0fkbkdc269l4sbpvbnmqq02vl80dz80l3d2zq6ax6dwhlvxk2blp"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("mu" ,mu)))
      (home-page "https://github.com/rougier/mu4e-dashboard")
      (synopsis "Provides mu4e org-mode link type for building org powered dashboards")
      (description
       "Provides a new mu4e org link type that allows for mu4e queries from
org-mode.  These links can then be organized to form a org-mode powered mu4e
dashboard!")
      (license license:gpl3+))))

(define-public emacs-org-timeline
  (package
      (name "emacs-org-timeline")
      (version "0.4.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Fuco1/org-timeline")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "106603835m3dy3bzjiasq2w574faxnn5kb72gr0y0mdkd0iwh8qa"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-org" ,emacs-org)))
      (home-page "https://github.com/Fuco1/org-timeline")
      (synopsis "Add graphical view of agenda to agenda buffer")
      (description
       "Add graphical view of agenda to agenda buffer.")
      (license license:gpl3+)))
