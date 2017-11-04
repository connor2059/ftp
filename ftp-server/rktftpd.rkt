#|
Managed FTP Server
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

#lang racket

(require (file "lib/private/openssl.rkt")
         (file "lib/private/net.rkt")
         (file "lib/system/daemon.rkt")
         (file "lib/system/signals.rkt")
         (file "lib/system/crypt.rkt")
         (file "lib/system/account.rkt")
         (file "lib/system/terminal.rkt")
         (file "lib/system/process.rkt")
         (file "lib/ftp-commands.rkt")
         (file "lib/ftp-server.rkt"))

(provide/contract [serve (->* [] [#;optional
                                  #:show-banner? boolean?
                                  #:read-cmd-line? boolean?
                                  #:echo-mode? boolean?
                                  #:debug-mode? boolean?
                                  #:script-mode? boolean?
                                  #:smi-srv-enabled? boolean?
                                  #:config-list list?]
                              void?)]
                  [serve/wait (->* [] [#;optional
                                       #:show-banner? boolean?
                                       #:read-cmd-line? boolean?
                                       #:echo-mode? boolean?
                                       #:debug-mode? boolean?
                                       #:script-mode? boolean?
                                       #:smi-srv-enabled? boolean?
                                       #:config-list list?]
                                   void?)]
                  [shutdown (-> void?)]
                  [running? (-> boolean?)])

(define default-welcome-message        "%v\n%c")
(define default-ftp-host               "127.0.0.1")
(define default-ftp-port               21)
(define default-ssl-protocol           #f)
(define default-ssl-key                "etc/certs/server-1.pem")
(define default-ssl-certificate        "etc/certs/server-1.pem")
(define default-max-allow-wait         25)
(define default-port-timeout           15)
(define default-pasv-timeout           15)
(define default-data-timeout           15)
(define default-session-timeout        120)
(define default-max-clients-per-IP     5)
(define default-max-clients            #f)
(define default-retr-transfer-rate     #f)
(define default-stor-transfer-rate     #f)
(define default-anonymous-retr-rate    #f)
(define default-anonymous-stor-rate    #f)
(define default-login-fail-sleep       60)
(define default-max-login-attempts     5)
(define default-passwd-sleep           0)
(define default-hide-dotobjects?       #f)
(define default-text-user&group-names? #t)
(define default-hide-ids?              #f)
(define default-pasv-enable?           #t)
(define default-port-enable?           #t)
(define default-read-only?             #f)
(define default-hide-names             null)
(define default-deny-names             null)
(define default-disable-ftp-commands   null)
(define default-real-user              "ftp")
(define default-allow-foreign-address  #f)
(define default-passive-ip&ports       (make-passive-ip&ports default-ftp-host 40000 40999))
(define default-root-dir               "ftp-dir")
;(define default-log-enable?            #t)
;(define default-log-file               "log/rktftpd.log")
;(define default-users-file             "etc/rktftpd.users")
;(define default-debug-enable?          #f)
;(define default-debug-log-file         "log/rktftpd.debug.log")
(define default-locale-encoding        "UTF-8")

(define smi-srv/enabled                  (make-parameter #f))
(define smi-srv/host                     "127.0.0.1")
(define smi-srv/port                     41234)
(define smi-srv/protocol                 'sslv3)
(define smi-srv/key                      "etc/certs/smi.pem")
(define smi-srv/certificate              "etc/certs/smi.pem")
(define smi-srv/admin-login-fail-sleep   120)
(define smi-srv/admin-max-login-attempts 5)
(define smi-srv/bad-admin-auth           '(0 . 0)) ; (attempts . time) 
(define smi-srv/admin                    (uid->uname (getuid)))

(define default-program-name "rktftpd")
(define default-pidfile (~a "/var/run/" default-program-name ".pid"))

(define pidfile (make-parameter default-pidfile))
(define make-pidfile? (make-parameter #f))

(define allow-unprivileged-server? #f)
(define smi-interactive?           #f)

(define echo-mode  (make-parameter #f))
(define debug-mode (make-parameter #f))

(define password #f)

(define ftp-servers    (make-hash))
(define unpriv-server? (not (get-shadow-password smi-srv/admin)))
(define config-file    "etc/rktftpd.conf")

(define server-custodian #f)

(define-syntax-rule (print-error msg v ...)
  (print-error-message default-program-name msg v ...))

(define-syntax-rule (debug-print msg v ...)
  (when (debug-mode) (print-error msg v ...)))

(define-syntax-rule (debug-print/exc e)
  (debug-print "caught an exception ~a" e))

(define (smi-query? q)
  (and (memq q '(none default-run start stop restart shutdown)) #t))

(define (define-ftp-server id srv)
  (when (srv-contains? id)
    (error 'define-ftp-server "Server '~a' already exists." id))
  (hash-set! ftp-servers id srv))

(define (remove-server id)
  (if (srv-contains? id)
      (hash-remove! ftp-servers id)
      (error 'remove-server "Server '~a' not found." id)))

(define (get-server id)
  (find-srv id))

(define (server-status srv)
  (cond
    [(send srv shutdown?) 'shutdown]
    [(send srv running?) 'running]
    [else 'stopped]))

(define (start-server id)
  (let ([srv (find-srv id)])
    (unless (send srv running?)
      (send srv clear-users-table)
      (send srv load-users/file)
      (send srv serve)
      (when (echo-mode) (displayln (format "Server ~a: ~a!" id (server-status srv)))))))

(define (stop-server id)
  (let ([srv (find-srv id)])
    (when (send srv running?)
      (send srv stop)
      (when (echo-mode) (displayln (format "Server ~a: ~a!" id (server-status srv)))))))

(define (find-srv id)
  (hash-ref ftp-servers id (λ () (error 'find-srv "Server '~a' not found." id))))

(define (srv-contains? id)
  (hash-has-key? ftp-servers id))

(define (start-servers)
  (hash-for-each ftp-servers (λ (id srv) (start-server id))))

(define (stop-servers)
  (hash-for-each ftp-servers (λ (id srv) (stop-server id))))

(define (serve #:show-banner? [show-banner? #f]
               #:read-cmd-line? [read-cmd-line? #f]
               #:echo-mode? [echo-mode? (echo-mode)]
               #:debug-mode? [debug-mode? (debug-mode)]
               #:script-mode? [script-mode? #f]
               #:smi-srv-enabled? [smi-srv-enabled? (smi-srv/enabled)]
               #:config-list [config-list null])
  (parameterize ([echo-mode echo-mode?]
                 [debug-mode debug-mode?]
                 [smi-srv/enabled smi-srv-enabled?]
                 [current-directory (if script-mode?
                                        (current-directory)
                                        (let ([run-dir-path (path-only (find-system-path 'run-file))])
                                          (or run-dir-path (current-directory))))]
                 [make-pidfile? (make-pidfile?)]
                 [pidfile (pidfile)])
    (let ([query (if (and read-cmd-line?
                          (equal? (current-command-line-arguments) #()))
                     'default-run
                     'none)]
          [show-version? #f]
          [run-smi? #f])
      (with-handlers ([any/c (λ (e) (debug-print/exc e))])
        (when read-cmd-line?
          (command-line
           #:program default-program-name
           #:once-any
           [("-r" "--start")
            "Start all servers"
            (set! query 'start)
            (set! run-smi? #t)]
           [("-p" "--stop")
            "Stop all servers"
            (set! query 'stop)
            (set! run-smi? #t)]
           [("-t" "--restart")
            "Restart all server"
            (set! query 'restart)
            (set! run-smi? #t)]
           [("-s" "--shutdown" "-x" "--exit")
            "Shutdown RktFTPd server"  
            (set! query 'shutdown)
            (set! run-smi? #t)]
           #:once-each
           [("-i" "--interactive")
            "Run the RktFTPd Management Interface in interactive mode"  
            (set! smi-interactive? #t)
            (set! run-smi? #t)]
           [("-v" "--version")
            "Shows version" 
            (set! show-version? #t)]
           [("-e" "--echo")
            "Allow the echo prints"
            (echo-mode #t)]
           [("-f" "--config")
            file
            "Use alternative configuration <file>"
            (set! config-file file)
            (set! run-smi? #t)]
           [("-d" "--debug")
            "Run the RktFTPd in debug mode"
            (debug-mode #t)]))
        (cond 
          [show-banner?  (display-lines (list default-server-name&version default-copyright ""))]
          [show-version? (displayln default-server-name&version)])
        (when (or run-smi?
                  (eq? query 'default-run))
          (if (null? config-list)
              (load-config/file config-file)
              (load-config/list config-list))
          (if password
              (begin
                (display "Password: ")
                (let ([pass (read-password #:abort-evt (λ () (displayln "\nAborted by user.")))])
                  (unless (eof-object? pass)
                    (newline)
                    (let* ([salt (let ([l (regexp-split #rx"\\$" password)])
                                   (format "$~a$~a" (second l) (third l)))]
                           [hash (and (string? pass)
                                      (crypt-string pass salt))])
                      (if (and hash (string=? password hash))
                          (server-management-interface query)
                          (begin
                            (sleep 2)
                            (displayln "Incorrect password.")))))))
              (server-management-interface query)))))))

(define (shutdown)
  (when (running?)
    (let ([cust server-custodian])
      (set! server-custodian #f)
      (custodian-shutdown-all cust))))

(define (running?)
  (and server-custodian (custodian? server-custodian)))

(define (serve/wait #:show-banner? [show-banner? #f]
                    #:read-cmd-line? [read-cmd-line? #f]
                    #:echo-mode? [echo-mode? (echo-mode)]
                    #:debug-mode? [debug-mode? (debug-mode)]
                    #:script-mode? [script-mode? #f]
                    #:smi-srv-enabled? [smi-srv-enabled? (smi-srv/enabled)]
                    #:config-list [config-list null])
  (local [(define (dummy-delay)
            (do [] [(not (running?))] (sleep 0.1)))
          
          (define (sighandler sig)
            (case (signum->symbol sig)
              [(SIGINT SIGQUIT SIGTERM) (shutdown)]))]
    (call-with-sighandler (λ ()
                            (serve #:show-banner? show-banner?
                                   #:read-cmd-line? read-cmd-line?
                                   #:echo-mode? echo-mode?
                                   #:debug-mode? debug-mode?
                                   #:script-mode? script-mode?
                                   #:smi-srv-enabled? smi-srv-enabled?
                                   #:config-list config-list)
                            (dummy-delay))
                          sighandler)))

(define (make-default-server)
  (new ftp-server/cfg%
       [welcome-message         default-welcome-message]
       [server-host             default-ftp-host]
       [server-port             default-ftp-port]
       [ssl-protocol            default-ssl-protocol]
       [ssl-key                 default-ssl-key]
       [ssl-certificate         default-ssl-certificate]
       [max-allow-wait          default-max-allow-wait]
       [port-timeout            default-port-timeout]
       [pasv-timeout            default-pasv-timeout]
       [data-timeout            default-data-timeout]
       [session-timeout         default-session-timeout]
       [max-clients-per-IP      default-max-clients-per-IP]
       [max-clients             default-max-clients]
       [retr-transfer-rate      default-retr-transfer-rate]
       [stor-transfer-rate      default-stor-transfer-rate]
       [anonymous-retr-rate     default-anonymous-retr-rate]
       [anonymous-stor-rate     default-anonymous-stor-rate]
       [login-fail-sleep        default-login-fail-sleep]
       [max-login-attempts      default-max-login-attempts]
       [passwd-sleep            default-passwd-sleep]
       [hide-dotobjects?        default-hide-dotobjects?]
       [text-user&group-names?  default-text-user&group-names?]
       [hide-ids?               default-hide-ids?]
       [pasv-enable?            default-pasv-enable?]
       [port-enable?            default-port-enable?]
       [read-only?              default-read-only?]
       [hide-names              default-hide-names]
       [deny-names              default-deny-names]
       [disable-ftp-commands    default-disable-ftp-commands]
       [allow-foreign-address   default-allow-foreign-address]
       [pasv-ip&ports           default-passive-ip&ports]
       [default-root-dir        default-root-dir]
       [default-locale-encoding default-locale-encoding]
       [debug-mode?             (debug-mode)]))

(define (load-config/list config)
  (with-handlers ([any/c (λ (e) 
                           (debug-print/exc e)
                           (error 'load-config/list "Syntax error in rktftpd-config."))])
    (match config
      [`(rktftpd-config ,params ...)
       (let ([srvs null])
         (for [(param params)]
           (match param
             [`(server ,(or (? symbol? srv-id) (? number? srv-id)) ,srv-params ...)
              (set! srvs (cons (cdr param) srvs))]
             [`(smi-server ,params ..1)
              (smi-srv/enabled #t)
              (for [(param params)]
                (match param
                  [`(host&port ,(? host-string? host) ,(? port-number? port))
                   (set!-values (smi-srv/host smi-srv/port) (values host port))]
                  [`(ssl ,params ..1)
                   (for [(param params)]
                     (match param
                       [`(protocol ,(? ssl-protocol? proto))
                        (set! smi-srv/protocol proto)]
                       [`(key ,(? path-string? key))
                        (set! smi-srv/key key)]
                       [`(certificate ,(? path-string? cert))
                        (set! smi-srv/certificate cert)]))]
                  [`(admin-login-fail-sleep ,(? exact-nonnegative-integer? sec))
                   (set! smi-srv/admin-login-fail-sleep sec)]
                  [`(admin-max-login-attempts ,(? exact-positive-integer? num))
                   (set! smi-srv/admin-max-login-attempts num)]))]
             [`(password ,(? password-string? pass))
              (set! password pass)]
             [`(default-locale-encoding ,(? string? encoding))
              (set! default-locale-encoding encoding)]
             [`(allow-unprivileged-server? ,(? boolean? flag))
              (set! allow-unprivileged-server? flag)]
             [`(make-pidfile? ,(? boolean? flag))
              (make-pidfile? flag)]
             [`(pidfile ,(? path-string? file))
              (pidfile file)]))
         (for [(s srvs)]
           (let ([srv (make-default-server)])
             (send srv read-config/list `(ftp-server . ,(cdr s)))
             (define-ftp-server (car s) srv))))])))

(define (load-config/file config-file)
  (with-handlers ([any/c (λ (e) 
                           (debug-print/exc e)
                           (print-error "Please check your RktFTPd-config file."))])
    (call-with-input-file config-file
      (λ (in) (load-config/list (read in))))))

(define (make-pid-file)
  (call-with-output-file (pidfile)
    (λ (out) (write (getpid) out))
    #:exists 'truncate))

(define (server-management-interface query)
  (when (and (not allow-unprivileged-server?)
             unpriv-server?)
    (displayln "You do not have sufficient privileges to work with RktFTPd.")
    (error 'server-management-interface "Permission denied."))
  (set! server-custodian (make-custodian))
  (parameterize ([current-custodian server-custodian])
    (with-handlers ([exn:fail:network? 
                     (λ (e)
                       (case query 
                         [(default-run start restart)
                          (when (positive? (hash-count ftp-servers))
                            (start-servers)
                            (unless (or (echo-mode) (debug-mode))
                              (activate-daemon-mode #t #t)
                              (close-output-port (current-output-port))
                              (close-input-port (current-input-port))
                              (close-output-port (current-error-port)))
                            (when (make-pidfile?)
                              (make-pid-file))
                            (when (smi-srv/enabled)
                              (smi-server)))]
                         [else
                          (shutdown)]))])
      (let*-values ([(hash-pass) (if unpriv-server? 
                                     (crypt-string smi-srv/admin "$1$")
                                     (crypt-string (get-shadow-password smi-srv/admin)
                                                   (string-append "$1$" smi-srv/admin)))]
                    [(ssl-ctx) (let ([ctx (ssl-make-client-context smi-srv/protocol)])
                                 (ssl-load-certificate-chain! ctx smi-srv/certificate default-locale-encoding)
                                 (ssl-load-private-key! ctx smi-srv/key #t #f default-locale-encoding)
                                 ctx)]
                    [(in out) (ssl-connect smi-srv/host smi-srv/port ssl-ctx)])
        (displayln hash-pass out)
        (flush-output out)
        (when (let ([response (read-line in)])
                (and (string? response)
                     (string=? response "Ok")))
          (let ([request (λ (cmd)
                           (displayln cmd out)
                           (flush-output out)
                           (displayln (read-line in)))])
            (case query
              [(start)
               (request "%start")
               (request "%bye")]
              [(stop)
               (request "%stop")
               (request "%bye")]
              [(restart)
               (request "%restart")
               (request "%bye")]
              [(shutdown)
               (request "%shutdown")]
              [else
               (when smi-interactive?
                 (printf "Connected to ~a:~a\n" smi-srv/host smi-srv/port)
                 (displayln "Type 'help' or '?' for help.\n")
                 (let loop ()
                   (display "smi> ")
                   (let ([cmd (read)])
                     (case cmd
                       ((help ?)
                        (display-lines '("(%start ID)      - launching the FTP server by its ID."
                                         "(%stop ID),"
                                         "(%pause ID)      - stop the FTP server by its ID."
                                         "(%restart ID)    - restart the FTP server by its ID."
                                         "%start           - start all the FTP servers."
                                         "%stop, %pause    - stop all the FTP servers."
                                         "%restart         - restart all the FTP-servers."
                                         "%shutdown, %exit - shutdown the RktFTPd server."
                                         "%bye             - close the session."))
                        (loop))
                       (else
                        (request cmd)
                        (unless (memq cmd '(%shutdown %exit %bye))
                          (loop)))))))])))
        (shutdown)))))

(define (smi-server)
  (local [(define (handle-client-request listener)
            (let-values ([(in out) (ssl-accept listener)])
              (thread (λ ()
                        (with-handlers ([any/c (λ (e) (debug-print "caught an exception ~a" e))])
                          (let ([hash-pass (read-line in)])
                            (when (string? hash-pass)
                              (if (and (or ((car smi-srv/bad-admin-auth). < . smi-srv/admin-max-login-attempts)
                                           (> ((current-seconds). - .(cdr smi-srv/bad-admin-auth))
                                              smi-srv/admin-login-fail-sleep))
                                       (string=? hash-pass (if unpriv-server?
                                                               (crypt-string smi-srv/admin "$1$")
                                                               (crypt-string (get-shadow-password smi-srv/admin)
                                                                             (string-append "$1$" smi-srv/admin)))))
                                  (let ([response (λ msg
                                                    (apply fprintf out msg)
                                                    (newline out)
                                                    (flush-output out))])
                                    (set! smi-srv/bad-admin-auth (cons 0 0))
                                    (response "Ok")
                                    (let next ([cmd (read in)])
                                      (unless (eof-object? cmd)
                                        (match cmd
                                          ['%bye
                                           (response "Goodbye")]
                                          ['%start
                                           (start-servers)
                                           (response "Ok")
                                           (next (read in))]
                                          [(or '%stop '%pause)
                                           (stop-servers)
                                           (response "Ok")
                                           (next (read in))]
                                          ['%restart
                                           (stop-servers)
                                           (start-servers)
                                           (response "Ok")
                                           (next (read in))]
                                          [(or '%shutdown '%exit)
                                           (stop-servers)
                                           (response "Goodbye")
                                           (shutdown)]
                                          [`(%start ,id)
                                           (if (hash-ref ftp-servers id #f)
                                               (begin
                                                 (start-server id)
                                                 (response "Ok"))
                                               (response "Server ID not found."))
                                           (next (read in))]
                                          [(or `(%pause ,id) `(%stop ,id))
                                           (if (hash-ref ftp-servers id #f)
                                               (begin
                                                 (stop-server id)
                                                 (response "Ok"))
                                               (response "Server ID not found."))
                                           (response "Ok")
                                           (next (read in))]
                                          [`(%restart ,id)
                                           (if (hash-ref ftp-servers id #f)
                                               (begin
                                                 (stop-server id)
                                                 (start-server id)
                                                 (response "Ok"))
                                               (response "Server ID not found."))
                                           (response "Ok")
                                           (next (read in))]
                                          [_
                                           (if (and (pair? cmd) (list? cmd))
                                               (if (memq (car cmd) '(%start %stop %pause %restart))
                                                   (response "Syntax error.")
                                                   (response "Command '~a' not implemented." (car cmd)))
                                               (response "Command '~a' not implemented." cmd))
                                           (next (read in))]))))
                                  (set! smi-srv/bad-admin-auth (cons (add1 (car smi-srv/bad-admin-auth))
                                                                     (current-seconds)))))))
                        (close-input-port in)
                        (close-output-port out)))))]
    (let ([listener (ssl-listen smi-srv/port (random 123456789) #t smi-srv/host smi-srv/protocol)])
      (ssl-load-certificate-chain! listener smi-srv/certificate default-locale-encoding)
      (ssl-load-private-key! listener smi-srv/key #t #f default-locale-encoding)
      (letrec ([main-loop (λ ()
                            (handle-client-request listener)
                            (main-loop))])
        (void (thread main-loop))))))
