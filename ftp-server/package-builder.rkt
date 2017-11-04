#|
RktFTPd Binary Package Builder
******************************************************************************

Summary
------------------------------------------------------------------------------
This file is part of RktFTPd.


License
------------------------------------------------------------------------------
RktFTPd
Copyright (C) 2011-2015 Mikhail Mosienko <netluxe@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
|#

#lang racket/base

(require racket/contract
         racket/cmdline
         racket/file
         file/zip
         compiler/embed
         compiler/distribute)

(provide/contract [build-package (->* [] [#;optional
                                          #:read-cmd-line? boolean?
                                          #:dest-path path-string?
                                          #:src-path path-string?]
                                      void?)])

(define (build-package #:read-cmd-line? [read-cmd-line? #f]
                       #:dest-path [dest-path "rktftpd.zip"]
                       #:src-path [src-path (current-directory)])
  (when read-cmd-line?
    (command-line
     #:program "rktftpd-package-builder"
     #:once-each
     [("-o" "--out") 
      file
      "Place the output into <file>"
      (set! dest-path file)]
     [("-s" "--src") 
      src
      "Place the output into <file>"
      (set! src-path src)]))
  (unless (complete-path? dest-path)
    (set! dest-path (path->complete-path dest-path)))
  (unless (complete-path? src-path)
    (set! src-path (path->complete-path src-path)))
  (let* ([temp-dir (build-path (find-system-path 'temp-dir) "rktftpd-package")]
         [compiled-dir "compiled"]
         [bin "bin"]
         [dest-exe (build-path temp-dir "rktftpd")]
         [etc "etc"]
         [etc-dest (build-path temp-dir etc)]
         [certs "certs"]
         [certs-src (build-path src-path etc certs)]
         [certs-dest (build-path temp-dir etc certs)]
         [lib "lib"]
         [lib-dest (build-path temp-dir lib)]
         [plt "plt"]
         [plt-lib (build-path lib plt)]
         [license "LICENSE"]
         [license-src (build-path src-path license)]
         [license-dest (build-path temp-dir license)]
         [readme "README"]
         [readme-src (build-path src-path readme)]
         [readme-dest (build-path temp-dir readme)])
    (display "Please wait for final assembly of the package")
    (let ([thd (letrec ([loop (λ ()
                                (sleep 1/2) 
                                (write-char #\.)
                                (flush-output)
                                (loop))])
                 (thread loop))])
      (with-handlers ([void (λ (e)
                              (delete-directory/files temp-dir)
                              (kill-thread thd)
                              (printf "\nError in ~a\n" (exn-message e)))])
        
        (when (file-exists? dest-path)
          (delete-file dest-path))
        
        (when (directory-exists? temp-dir)
          (delete-directory/files temp-dir))
        
        (when (directory-exists? compiled-dir)
          (delete-directory/files compiled-dir))
        
        (make-directory temp-dir)
        (make-directory etc-dest)
        (with-output-to-file (build-path etc-dest "rktftpd.conf")
          (λ () (display "
(rktftpd-config
 [server 1
         (welcome-message \"%v\\n%c\\nPlease visit http://chiselapp.com/user/netluxe/repository/RktFTPd/index\")
         (host&port \"127.0.0.1\" 2121) ;; For testing is better to choose an unprivileged high-numbered port.
         (passive-host&ports \"127.0.0.1\" 40000 40999)
         (max-allow-wait 5)
         (port-timeout 15)
         (pasv-timeout 15)
         (data-timeout 15)
         (session-timeout 120)
         (max-clients-per-IP 3)
         ;(max-clients 15)
         ;(retr-transfer-rate 1024)
         ;(stor-transfer-rate 512)
         ;(anonymous-retr-rate 256)
         ;(anonymous-stor-rate 256)
         (login-fail-sleep 30)
         (max-login-attempts 15)
         (passwd-sleep 0)
         (hide-dotobjects? #f)
         (text-user&group-names? #t)
         (hide-ids? #f)
         (pasv-enable? #t)
         (port-enable? #t)
         (read-only? #f)
         (hide-names ()) ;; Example (hide-names (\"\\\\.emacs.*$\"))
         (deny-names ())
         (disable-ftp-commands ())
         (allow-foreign-address #f)
         (default-real-user \"user-name\")
         (default-root-dir \"/\")
         (users-file \"../etc/rktftpd.users\")
         (log-enable? #t)
         (log-file \"../logs/rktftpd1.*.log\")
         (debug? #f)]
 [smi-server
  (host&port \"127.0.0.1\" 41234)
  (ssl
   [protocol sslv3]
   [key \"../etc/certs/test.pem\"]
   [certificate \"../etc/certs/test.pem\"])
  (admin-login-fail-sleep 80)
  (admin-max-passwd-attempts 15)]

; [default-locale-encoding \"cp1251\"]
 [password \"$1$$CoERg7ynjYLsj2j4glJ34.\"] ;; password: 'admin'
 [allow-unprivileged-server? #t]
 [make-pidfile? #f]
 [pidfile \"/var/run/rktftpd.pid\"])
"))
          #:mode 'text
          #:exists 'truncate/replace)
        (with-output-to-file (build-path etc-dest "rktftpd.users")
          (λ () (display "
(ftp-users
 (user \"root\"
       [real-user         \"root\"]
       [anonymous?        #f]
       [hide-ids?         #f]
       [hide-dotobjects?  #f]
       [mfmt-enable?      #t]
       [mff-enable?       #t]
       [site-enable?      #t]
       [ftp-perm          #f] ; equal lracmfd
       [local-root-dir    \"/\"])
 ;(user \"ftp\"
 ;      [real-user      \"ftp\"])
 (user \"anonymous\"
       [real-user \"ftp\"])
 (user \"nopriv\"
       [real-user  \"ftp\"]
       [password   \"$1$$z2VkRbfNoE/xHLBj8i2cv.\"] ;; password: 'ftp'
       [anonymous? #f]))
"))
          #:mode 'text
          #:exists 'truncate/replace)
        (copy-directory/files certs-src certs-dest)
        (copy-directory/files src-path lib-dest)
        (copy-file license-src license-dest)
        (copy-file readme-src readme-dest)
        
        (parameterize ([current-namespace (make-base-namespace)]
                       [current-directory lib-dest])
          (let* ([main-module 'main]
                 [main-module-path (format "~a.rkt" main-module)]
                 [main-module-full-path (build-path lib-dest main-module-path)])
            (with-output-to-file main-module-full-path
              (λ () (display "#lang racket/base\n\n(require (file \"rktftpd.rkt\"))\n(serve/wait #:read-cmd-line? #t)\n"))
              #:mode 'text
              #:exists 'truncate/replace)
            (create-embedding-executable 
             dest-exe
             #:modules `((#f (file ,main-module-path)))
             #:cmdline '("-U" "--")
             #:literal-expression (compile `(namespace-require '',main-module)))))
        (assemble-distribution temp-dir (list dest-exe))
        
        (parameterize ([current-directory temp-dir])
          (zip dest-path
               readme
               license
               etc
               bin
               plt-lib))
        
        (delete-directory/files temp-dir)
        (newline)
        (kill-thread thd)))))
