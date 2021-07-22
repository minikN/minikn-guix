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

(define-module (rekahsoft-gnu packages shellutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tmux)
  #:use-module (rekahsoft-gnu packages golang))

(define-public fzf
  (package
    (name "fzf")
    (version "0.24.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/junegunn/fzf")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17k32wr70sp7ag69xww2q9mrgnzakgkjw6la04n3jlhfa5z37dzj"))))
    (build-system go-build-system)
    (native-inputs
     `(("github.com/mattn/go-isatty" ,go-github-com-mattn-go-isatty)
       ("github.com/mattn/go-runewidth" ,go-github-com-mattn-go-runewidth)
       ("github.com/mattn/go-shellwords" ,go-github-com-mattn-go-shellwords)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-github-com-saracen-walker" ,go-github-com-saracen-walker)
       ("go-golang.org-x-sync-errgroup" ,go-golang.org-x-sync-errgroup)))
    (propagated-inputs
     `(("tmux" ,tmux)
       ("perl" ,perl)))
    (arguments
     `(#:import-path "github.com/junegunn/fzf"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'setup-env
           (lambda _
             (setenv "FZF_VERSION" ,version)
             (setenv "FZF_REVISION" ,version)))
         (replace 'build
           (lambda _
             (with-directory-excursion "src/github.com/junegunn/fzf"
               (invoke "make"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share"))
               (with-directory-excursion "src/github.com/junegunn/fzf"
                 (invoke "make" "install")
                 (copy-recursively "bin" (string-append out "/bin"))
                 (copy-recursively "man" (string-append out "/share/man"))
                 (copy-recursively "shell" (string-append out "/share/fzf")))))))))
    (synopsis "Command line fuzzy finder")
    (description
     "@command{fzf} is a general purpose command line fuzzy finder.  It's an
interactive uniz filter for command-line that can be used with any lists;
files, command history, processes, hostnames, bookmarks, git commits, etc..")
    (home-page "https://github.com/junegunn/fzf")
    (license license:expat)))

(define-public spaceship-prompt
  (package
    (name "spaceship-prompt")
    (version "3.11.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/denysdovhan/spaceship-prompt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q7m9mmg82n4fddfz01y95d5n34xnzhrnn1lli0vih39sgmzim9b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (func-path (string-append out "/share/zsh/site-functions"))
                    (install-path (string-append out "/lib/spaceship-prompt")))
               (for-each mkdir-p `(,func-path ,install-path))
               (for-each (lambda (dir)
                           (copy-recursively dir (string-append install-path "/" dir)))
                         '("lib" "modules" "sections"))
               (copy-file "spaceship.zsh" (string-append install-path "/spaceship.zsh"))
               (symlink (string-append install-path "/spaceship.zsh")
                        (string-append func-path "/prompt_spaceship_setup"))))))))
    (synopsis "Zsh prompt for Astronauts")
    (description
     "Spaceship is a minimalistic, powerful and extremely customizable
Zsh prompt.  It combines everything you may need for convenient work,
without unecessary complications, like a real spaceship.")
    (home-page "https://github.com/denysdovhan/spaceship-prompt")
    (license license:expat)))

(define-public zsh-syntax-highlighting
  (package
    (name "zsh-syntax-highlighting")
    (version "0.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zsh-users/zsh-syntax-highlighting")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "039g3n59drk818ylcyvkciv8k9mf739cv6v4vis1h9fv9whbcmwl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (setenv "PREFIX" out)
               ;; TODO: These 2 lines are required otherwise the build
               ;;       fails. This is due to the checkout is being modified, which
               ;;       is somehow an issue (but is unexpected
               (invoke "touch" "docs/all.md")
               (chmod "README.md" #o700)

               (invoke "make" "install")))))))
    (synopsis "Fish shell-like syntax highlighting for Zsh")
    (description
     "This package provides syntax highlighting for the shell zsh.  It enables
highlighting of commands whilst they are typed at a zsh prompt into an
interactive terminal.  This helps in reviewing commands before running them,
particularly in catching syntax errors.")
    (home-page "https://github.com/zsh-users/zsh-syntax-highlighting")
    (license license:bsd-3)))
