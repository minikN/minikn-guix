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

(define-module (minikn-gnu packages terraform)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (minikn-gnu packages golang))

(define-public terraform
  (package
    (name "terraform")
    (version "0.12.28")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hashicorp/terraform.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05ymr6vc0sqh1sia0qawhz0mag8jdrq157mbj9bkdpsnlyv209p3"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-gox" ,go-github-com-mitchellh-gox)))
    (arguments
     `(#:import-path "github.com/hashicorp/terraform"
       #:install-source? #f
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (with-directory-excursion "src/github.com/hashicorp/terraform"
                        (setenv "TF_RELEASE" "")
                        (setenv "XC_OS" "linux")
                        (setenv "XC_ARCH"
                                (let ((system ,(or (%current-target-system)
                                                   (%current-system))))
                                  (cond
                                   ((string-prefix? "x86_64" system)
                                    "amd64")
                                   ((string-prefix? "i686" system)
                                    "386")
                                   (else ""))))
                        (invoke "./scripts/build.sh"))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (with-directory-excursion "src/github.com/hashicorp/terraform"
                          (rename-file "bin/terraform" "bin/terraform-0.12")
                          (copy-recursively "bin" (string-append out "/bin")))))))))
    (synopsis "Safely and predictably create, change, and improve infrastructure")
    (description
     "Terraform is a tool for building, changing, and versioning
infrastructure safely and efficiently.  Terraform can manage existing and
popular service providers as well as custom in-house solutions.")
    (home-page "https://github.com/hashicorp/terraform")
    (license license:mpl2.0)))

(define* (wrap-terraform terraform
                         #:optional
                         (name (string-append (package-name terraform) "-wrapper")))
  (package/inherit terraform
    (name name)
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
    (propagated-inputs `(("terraform" ,terraform)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
         (begin
           (use-modules (guix build utils))
           (let ((bin (string-append (assoc-ref %outputs "out") "/bin"))
                 (terraform (string-append (assoc-ref %build-inputs "terraform") "/bin/")))
                (mkdir-p bin)
                (symlink (string-append terraform "terraform-0.12")
                         (string-append bin "/" "terraform"))
                #t))))
    (synopsis "Wrapper for the terraform 0.12 commands")
    (description
     "This package provides wrappers for the commands of Terraform@tie{}0.12.x such
that they can be invoked under their usual name---e.g., @command{terraform}
instead of @command{terraform-0.12}.")))

(define-public terraform-wrapper (wrap-terraform terraform))

(define-public terraform-0.11
  (package
    (name "terraform0.11")
    (version "0.11.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hashicorp/terraform.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bzz5wy13gh8j47mxxp6ij6yh20xmxd9n5lidaln3mf1bil19dmc"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-gox" ,go-github-com-mitchellh-gox)))
    (arguments
     `(#:import-path "github.com/hashicorp/terraform"
       #:install-source? #f
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (with-directory-excursion "src/github.com/hashicorp/terraform"
                        (setenv "TF_RELEASE" "")
                        (setenv "XC_OS" "linux")
                        (setenv "XC_ARCH"
                                (let ((system ,(or (%current-target-system)
                                                   (%current-system))))
                                  (cond
                                   ((string-prefix? "x86_64" system)
                                    "amd64")
                                   ((string-prefix? "i686" system)
                                    "386")
                                   (else ""))))
                        (invoke "./scripts/build.sh"))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (with-directory-excursion "src/github.com/hashicorp/terraform"
                          (copy-recursively "bin" (string-append out "/bin")))))))))
    (synopsis "Safely and predictably create, change, and improve infrastructure")
    (description
     "Terraform is a tool for building, changing, and versioning
infrastructure safely and efficiently.  Terraform can manage existing and
popular service providers as well as custom in-house solutions.")
    (home-page "https://github.com/hashicorp/terraform")
    (license license:mpl2.0)))

(define terraform-provider-phases
  '(modify-phases %standard-phases
     (add-after 'remove-go-references 'change-out
       (lambda* (#:key outputs #:allow-other-keys)
         (let* ((out (assoc-ref outputs "out"))
                (original-path (string-append out  "/bin"))
                (install-path (string-append out "/libexec/terraform")))
           (mkdir-p install-path)
           (copy-recursively original-path install-path)
           (delete-file-recursively original-path))))))

(define-public terraform-provider-aws
  (package
    (name "terraform-provider-aws")
    (version "2.14.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-aws.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12z0ggbwwgv44a7wh3y9aq425hlax8hyrx4mhx2wyydfdhqs11h1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-aws"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "@command{Terraform} AWS provider")
    (description
     "Terraform AWS provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-aws")
    (license license:mpl2.0)))

(define-public terraform-provider-google
  (package
    (name "terraform-provider-google")
    (version "2.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-google.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r4x7l20w8wb6a356vx71lra466p7hfww1jyv0bg7z1f015qs91l"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-google"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Google Cloud Platform provider ")
    (description
     "Terraform Google Cloud Platform provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-google")
    (license license:mpl2.0)))

(define-public terraform-provider-kubernetes
  (package
    (name "terraform-provider-kubernetes")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-kubernetes.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fah2310hic2svjfya3051sv8sp5gwgrfc1y2ggdn66hjz0x6ir3"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-kubernetes"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Kubernetes provider ")
    (description
     "Terraform Kubernetes provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-kubernetes")
    (license license:mpl2.0)))

(define-public terraform-provider-azurerm
  (package
    (name "terraform-provider-azurerm")
    (version "1.30.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-azurerm.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zxvj6k1xs151dik8yhjl31r563dsc7iykv32g10w5szyigvxqfd"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-azurerm"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Azure Resource Manager provider")
    (description
     "Terraform Azure Resource Manager provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-azurerm")
    (license license:mpl2.0)))

(define-public terraform-provider-alicloud
  (package
    (name "terraform-provider-alicloud")
    (version "1.46.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-alicloud.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cndsc0k7mk5xmaakszbl680igxfvxzlcnrfr07qz6bk796p9sfp"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-alicloud"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Alicloud provider")
    (description
     "Terraform Alicloud provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-alicloud")
    (license license:mpl2.0)))

(define-public terraform-provider-vsphere
  (package
    (name "terraform-provider-vsphere")
    (version "1.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-vsphere.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02qkm8n0z8v8xfjkvy5nkjc4p37xqp5xsds10b3b060vb5l6yfg4"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-vsphere"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Vsphere provider")
    (description
     "Terraform Vsphere provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-vsphere")
    (license license:mpl2.0)))

(define-public terraform-provider-cloudflare
  (package
    (name "terraform-provider-cloudflare")
    (version "1.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-cloudflare.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mb771dw5fw2s6jd63sybwza49axyj9967fl454m3sznhpl96y8z"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-cloudflare"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Cloudflare provider")
    (description
     "Terraform Cloudflare provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-cloudflare")
    (license license:mpl2.0)))

(define-public terraform-provider-vault
  (package
    (name "terraform-provider-vault")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-vault.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19ckfm89877s7jp8vssph9mdjr3srq4m6kc2zn2ky979775bi5lw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-vault"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Vault provider")
    (description
     "Terraform Vault provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-vault")
    (license license:mpl2.0)))

(define-public terraform-provider-gitlab
  (package
    (name "terraform-provider-gitlab")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-gitlab.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11j84kyw2lsqsn6ya070hx0k6igb3hhjl8zlnr4q3gpmv51y23qc"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-gitlab"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Gitlab provider")
    (description
     "Terraform Gitlab provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-gitlab")
    (license license:mpl2.0)))

(define-public terraform-provider-github
  (package
    (name "terraform-provider-github")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-github.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05ca0vw6s7q40zcs50mr6k2swwx3a3njd4dxdz00qki58hgb87y6"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-github"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Github provider")
    (description
     "Terraform Github provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-github")
    (license license:mpl2.0)))

(define-public terraform-provider-digitalocean
  (package
    (name "terraform-provider-digitalocean")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-digitalocean.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x55clpy3i2jrnmdwlya82vd3aiyvrzpx5y57xjs29x232cygw7r"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-digitalocean"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Digitalocean provider")
    (description
     "Terraform Digitalocean provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-digitalocean")
    (license license:mpl2.0)))

(define-public terraform-provider-helm
  (package
    (name "terraform-provider-helm")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-helm.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bni05ski94w3ihhf0jp3mk8dzkbm9jlgv6xb23403ppcbgy7s9d"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-helm"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Helm provider")
    (description
     "Terraform Helm provider.")
    (home-page "https://helm.com/terraform-providers/terraform-provider-helm")
    (license license:mpl2.0)))

(define-public terraform-provider-linode
  (package
    (name "terraform-provider-linode")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-linode.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i2x0jd26qanziihlc38s7rxiab66ld2s5gz18rrmrbwhcl95fwj"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-linode"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform Linode provider")
    (description
     "Terraform Linode provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-linode")
    (license license:mpl2.0)))

(define-public terraform-provider-random
  (package
    (name "terraform-provider-random")
    (version "2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-random.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "102bgd8s9yhm2ny2akv04mhwf5mphqhsxx9vxjbg7ygqnz9ka5nw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-random"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform random provider")
    (description
     "Terraform Random provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-random")
    (license license:mpl2.0)))

(define-public terraform-provider-null
  (package
    (name "terraform-provider-null")
    (version "2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-null.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0di1hxmd3s80sz8hl5q2i425by8fbk15f0r4jmnm6vra0cq89jw2"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-null"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform null provider")
    (description
     "Terraform null provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-null")
    (license license:mpl2.0)))

(define-public terraform-provider-heroku
  (package
    (name "terraform-provider-heroku")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-heroku.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "161pc6s4ifzi58cx32sdk9nan4kz6gn3mch1pnmp03f6390s2pcm"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-heroku"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform heroku provider")
    (description
     "Terraform Heroku provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-heroku")
    (license license:mpl2.0)))

(define-public terraform-provider-nomad
  (package
    (name "terraform-provider-nomad")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-nomad.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04dh7gas6viny6bkx89fkwmxrw101b8bmw14m2mzfkxn70cl2na4"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-nomad"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform nomad provider")
    (description
     "Terraform Nomad provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-nomad")
    (license license:mpl2.0)))

(define-public terraform-provider-postgresql
  (package
    (name "terraform-provider-postgresql")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-postgresql.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fywq1vzm8fcd0jcvzx1n3jilrk0r6ylqz9r4dwsyc64iqvmrpps"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-postgresql"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform postgresql provider")
    (description
     "Terraform Postgresql provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-postgresql")
    (license license:mpl2.0)))

(define-public terraform-provider-grafana
  (package
    (name "terraform-provider-grafana")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-grafana.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y41yhp82phgw83frkgrjzpymsys2abvdwa1n905rz7i15ybasqc"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-grafana"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform grafana provider")
    (description
     "Terraform Grafana provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-grafana")
    (license license:mpl2.0)))

(define-public terraform-provider-docker
  (package
    (name "terraform-provider-docker")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-docker.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w69kjlrgy4397c9nz3llqsqcjzchwqw1ksw4wg34xnnq20pd4ql"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-docker"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform docker provider")
    (description
     "Terraform Docker provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-docker")
    (license license:mpl2.0)))

(define-public terraform-provider-openstack
  (package
    (name "terraform-provider-openstack")
    (version "1.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/terraform-providers/terraform-provider-openstack.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y9b9h523zflq4z9cwjgfa0l92j4ac5pc1r6vja1aw3gg2ln80x9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/terraform-providers/terraform-provider-openstack"
       #:install-source? #f
       #:phases ,terraform-provider-phases))
    (synopsis "Terraform openstack provider")
    (description
     "Terraform Openstack provider.")
    (home-page "https://github.com/terraform-providers/terraform-provider-openstack")
    (license license:mpl2.0)))
