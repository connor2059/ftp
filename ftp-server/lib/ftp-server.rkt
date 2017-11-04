#|
Core FTP Server
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

(require racket/date
         (file "private/openssl.rkt")
         (file "private/net.rkt")
         (file "private/syntax.rkt")
         (file "user.rkt")
         (file "clients-table.rkt")
         (file "default-responses.rkt")
         (file "ftp-session.rkt")
         (file "ftp-commands.rkt")
         (file "extra-ftp-commands.rkt"))

(provide default-server-name&version
         default-copyright
         print-error-message
         password-string?
         ftp-server%
         ftp-server/cfg%)

(define default-server-name&version "RktFTPd v2.7 RC2")
(define default-copyright "Copyright (c) 2011-2015 Mikhail Mosienko <netluxe@gmail.com>")

(define-syntax-rule (print-error-message title msg v ...)
  (eprintf "~a: ~a\n" title (format msg v ...)))

(define (password-string? str)
  (and (string? str)
       (let ([l (regexp-split #rx"\\$" str)])
         (and l ((length l). = . 4)))))

(define (kilobyte? v)
  (and (symbol? v)
       (regexp-match? #rx"^[0-9]+[Kk]$" (symbol->string v))))

(define (megabyte? v)
  (and (symbol? v)
       (regexp-match? #rx"^[0-9]+[Mm]$" (symbol->string v))))

(define (xbyte->number v)
  (string->number (car (regexp-match #rx"[0-9]+" (symbol->string v)))))

(define host/c (or/c host-string? not))
(define ssl-protocol/c (or/c ssl-protocol? not))
(define not-empty-string/c (and/c string? (λ (str) (not (string=? str "")))))
(define path/c (or/c path-string? not))
(define hide-ids/c (or/c boolean? not-empty-string/c))
(define passwd-string/c (or/c not password-string?))
(define false-or-exposint/c (or/c not exact-positive-integer?))

(provide/define-member-key
 ftp-server%_create-ftp-session)

(define/contract ftp-server%
  
  (class/c (init-field [welcome-message         not-empty-string/c]
                       
                       [server-host             host/c]
                       [server-port             port-number?]
                       [ssl-protocol            ssl-protocol/c]
                       [ssl-key                 path/c]
                       [ssl-certificate         path/c]
                       
                       [max-allow-wait          exact-positive-integer?]
                       [port-timeout            exact-positive-integer?]
                       [pasv-timeout            exact-positive-integer?]
                       [data-timeout            exact-positive-integer?]
                       [session-timeout         exact-positive-integer?]
                       [max-clients-per-IP      exact-positive-integer?]
                       [max-clients             false-or-exposint/c]
                       [retr-transfer-rate      false-or-exposint/c]
                       [stor-transfer-rate      false-or-exposint/c]
                       [anonymous-retr-rate     false-or-exposint/c]
                       [anonymous-stor-rate     false-or-exposint/c]
                       [transfer-block-size     exact-positive-integer?]
                       
                       [login-fail-sleep        exact-nonnegative-integer?]
                       [max-login-attempts      exact-positive-integer?]
                       [passwd-sleep            exact-nonnegative-integer?]
                       
                       [hide-dotobjects?        boolean?]
                       [text-user&group-names?  boolean?]
                       [hide-ids?               hide-ids/c]
                       [pasv-enable?            boolean?]
                       [port-enable?            boolean?]
                       [read-only?              boolean?]
                       [hide-names              (listof not-empty-string/c)]
                       [deny-names              (listof not-empty-string/c)]
                       [disable-ftp-commands    (listof symbol?)]
                       
                       [pasv-ip&ports           passive-ip&ports?]
                       [allow-foreign-address   boolean?]
                       
                       [default-real-user       not-empty-string/c]
                       [default-root-dir        path-string?]
                       [default-locale-encoding string?]
                       [log-enable?             boolean?]
                       [log-output-port         log-port?]
                       [debug?                  boolean?])
           [useradd (->*m [not-empty-string/c
                           passwd-string/c]
                          [not-empty-string/c
                           #:anonymous? boolean? 
                           #:hide-ids? hide-ids/c
                           #:hide-dotobjects? boolean?
                           #:mfmt-enable? boolean?
                           #:mff-enable? boolean?
                           #:site-enable? boolean?
                           #:permissions user-permissions?
                           #:root-dir path/c]
                          void?)]
           [clear-users-table (->m void?)]
           [serve (->m void?)]
           [stop (->m void?)]
           [running? (->m boolean?)]
           [shutdown? (->m boolean?)])
  
  (let-member-name ([create-ftp-session ftp-server%_create-ftp-session])
    (class object%
      (super-new)
      ;;
      ;; ---------- Public Fields ----------
      ;;
      (init-field [welcome-message         "Racket FTP Server!"]
                  
                  [server-host             "127.0.0.1"]
                  [server-port             21]
                  [ssl-protocol            #f]
                  [ssl-key                 #f]
                  [ssl-certificate         #f]
                  
                  [max-allow-wait          25]
                  [port-timeout            15]
                  [pasv-timeout            15]
                  [data-timeout            15]
                  [session-timeout         120]
                  [max-clients-per-IP      5]
                  [max-clients             #f]
                  [retr-transfer-rate      #f]
                  [stor-transfer-rate      #f]
                  [anonymous-retr-rate     #f]
                  [anonymous-stor-rate     #f]
                  [transfer-block-size     (* 16 1024)]
                  
                  [login-fail-sleep        60]
                  [max-login-attempts      5]
                  [passwd-sleep            0]
                  
                  [hide-dotobjects?        #f]
                  [text-user&group-names?  #t]
                  [hide-ids?               #f]
                  [pasv-enable?            #t]
                  [port-enable?            #t]
                  [read-only?              #f]
                  [hide-names              null]
                  [deny-names              null]
                  [disable-ftp-commands    null]
                  [allow-foreign-address   #f]
                  
                  [pasv-ip&ports           (make-passive-ip&ports "127.0.0.1" 40000 40999)]
                  
                  [default-real-user       "ftp"]
                  [default-root-dir        "ftp-dir"]
                  [default-locale-encoding "UTF-8"]
                  [log-enable?             #f]
                  [log-output-port         (log-port (make-semaphore 1) (current-output-port))]
                  [free-log-output-port?   #f]
                  [debug?                  #f])
      ;;
      ;; ---------- Private Fields ----------
      ;;
      (define *server-host*    server-host)
      (define *server-port*    server-port)
      (define pasv-cmds        '(PASV EPSV))
      (define port-cmds        '(PORT EPRT))
      (define vfs-modify-cmds  '(APPE DELE MFF MFMT MKD RMD RNFR RNTO SITE STOR STOU XMKD XRMD))
      (define users-table      (make-users-table))
      (define server-custodian #f)
      (define random-gen       (make-pseudo-random-generator))
      (define enabled?         #t)
      (define server-responses (default-server-responses))
      ;;
      ;; ---------- Public Methods ----------
      ;;
      (define/public (useradd login
                              passwd
                              [real-user default-real-user]
                              #:anonymous? [anonymous? #f]
                              #:hide-ids? [hide-ids? #t]
                              #:hide-dotobjects? [hide-dotobjects? #t]
                              #:mfmt-enable? [mfmt-enable? #f]
                              #:mff-enable? [mff-enable? #f]
                              #:site-enable? [site-enable? #f]
                              #:permissions [permissions (make-user-permissions #:read-file #t
                                                                                #:enter-directory #t
                                                                                #:listing-directory #t)]
                              #:root-dir [root-dir "/"])
        (ftp-useradd users-table
                     login
                     passwd
                     real-user
                     anonymous?
                     hide-ids?
                     hide-dotobjects?
                     mfmt-enable?
                     mff-enable?
                     site-enable?
                     permissions
                     root-dir))
      
      (define (shutdown-error src)
        (error src "The ftp-server (~a/~a) is shutdown." *server-host* *server-port*))
      
      (define/public (clear-users-table)
        (when (shutdown?)
          (shutdown-error 'ftp-server%_clear-users-table))
        (clear-users-info users-table))
      
      (define/public (serve)
        (when (shutdown?)
          (shutdown-error 'ftp-server%_serve))
        (unless (running?)
          (set! server-custodian (make-custodian))
          (with-handlers ([any/c (λ (e)
                                   (when (exn:fail:network? e)
                                     (print-debug-message #f
                                                          "[error ~a] The ftp-server (~a/~a) is already running."
                                                          'ftp-server%_serve
                                                          server-host
                                                          server-port))
                                   (stop)
                                   (raise e))])
            (parameterize ([current-custodian server-custodian])
              (unless (directory-exists? default-root-dir)
                (make-directory* default-root-dir))
              (when (and server-host server-port)
                (when (and ssl-protocol
                           (not ssl-available?))
                  (error 'ftp-server%_serve "The OpenSSL library is not available."))
                (set! *server-host* server-host)
                (set! *server-port* server-port)
                (let* ([bad-auth-table (make-hash)]
                       [clients-table (make-clients-table)]
                       [ssl-server-ctx
                        (and/exc ssl-protocol
                                 (let ([ctx (ssl-make-server-context ssl-protocol)])
                                   (ssl-load-default-verify-sources! ctx)
                                   (when (path-string? ssl-certificate)
                                     (ssl-load-certificate-chain! ctx ssl-certificate default-locale-encoding))
                                   (when (path-string? ssl-key)
                                     (ssl-load-private-key! ctx ssl-key #t #f default-locale-encoding))
                                   (ssl-seal-context! ctx)
                                   ctx))]
                       [ssl-client-ctx
                        (and/exc ssl-protocol
                                 (let ([ctx (ssl-make-client-context ssl-protocol)])
                                   (ssl-load-default-verify-sources! ctx)
                                   (ssl-seal-context! ctx)
                                   ctx))]
                       [tcp-listener (tcp-listen *server-port* max-allow-wait #t *server-host*)])
                  (letrec ([main-loop (λ ()
                                        (with-handlers ([any/c (λ (e)
                                                                 (print-debug-message #f
                                                                                      "[error ~a] ~a"
                                                                                      'ftp-server%_serve
                                                                                      (if (exn? e)
                                                                                          (exn-message e)
                                                                                          e)))])
                                          (create-ftp-session tcp-listener
                                                              #:bad-auth-table bad-auth-table
                                                              #:clients-table clients-table
                                                              #:ssl-server-context ssl-server-ctx
                                                              #:ssl-client-context ssl-client-ctx))
                                        (main-loop))])
                    (thread main-loop)
                    (print-log-message "The ftp-server (~a/~a) is running." *server-host* *server-port*))))))))
      
      (define/private (print-log-message fmt . e)
        (when log-enable?
          (dynamic-wind
           (λ ()
             (semaphore-wait (log-port-sema log-output-port)))
           (λ ()
             (let ([out (log-port-out log-output-port)])
               (parameterize ([date-display-format 'iso-8601])
                 (fprintf out "~a : " (date->string (current-date) #t)))
               (apply fprintf out fmt e)
               (newline out)
               (flush-output out)))
           (λ ()
             (semaphore-post (log-port-sema log-output-port))))))
      
      (define/private (print-debug-message client-host fmt . e)
        (when (and debug? log-enable?)
          (dynamic-wind
           (λ ()
             (semaphore-wait (log-port-sema log-output-port)))
           (λ ()
             (let ([out (log-port-out log-output-port)])
               (parameterize ([date-display-format 'iso-8601])
                 (if client-host
                     (fprintf out "~a [~a] ! " (date->string (current-date) #t) client-host)
                     (fprintf out "~a ! " (date->string (current-date) #t))))
               (apply fprintf out fmt e)
               (newline out)
               (flush-output out)))
           (λ ()
             (semaphore-post (log-port-sema log-output-port))))))
      
      (define/private (print-error-response out client-host response-tag)
        (let ([response (cdr (assq 'EN (hash-ref server-responses response-tag)))])
          (print-debug-message client-host response)
          (fprintf out "~a\r\n" response)
          (flush-output out)))
      
      (define/public (create-ftp-session tcp-listener
                                         #:bad-auth-table bad-auth-table
                                         #:clients-table clients-table
                                         #:ssl-server-context ssl-server-ctx
                                         #:ssl-client-context ssl-client-ctx)
        (with-handlers ([exn:fail:network? (λ (e)
                                             (print-debug-message #f
                                                                  "[error ~a] The ftp-server (~a/~a) is shutting down."
                                                                  'ftp-server%_create-ftp-session
                                                                  *server-host*
                                                                  *server-port*)
                                             (shutdown))])
          (let next-accept ()
            (let*-values ([(in out)
                           (let-values ([(in out) (tcp-accept tcp-listener)])
                             (if ssl-server-ctx 
                                 (ports->ssl-ports in out 
                                                   #:mode 'accept
                                                   #:context ssl-server-ctx
                                                   #:close-original? #t
                                                   #:shutdown-on-close? #t) 
                                 (values in out)))]
                          [(server-host server-port client-host client-port)
                           ((if ssl-server-ctx ssl-addresses tcp-addresses) in #t)])
              (cond
                [(and max-clients
                      (>= (clients-table-count clients-table) max-clients))
                 (print-error-response out client-host 'MAX-CLIENTS)
                 (close-output-port out)
                 (close-input-port in)
                 (next-accept)]
                [(and (clients-table-ref? clients-table client-host)
                      (>= (clients-table-ref clients-table client-host) max-clients-per-IP))
                 (print-error-response out client-host 'MAX-CLIENTS-PER-IP)
                 (close-output-port out)
                 (close-input-port in)
                 (next-accept)]
                [else
                 (thread
                  (λ ()
                    (dynamic-wind
                     (λ ()
                       (clients-table-add clients-table client-host))
                     (λ ()
                       (send (new ftp-session%
                                  [:session-timeout         session-timeout]
                                  [:client-host             client-host]
                                  [:client-input-port       in]
                                  [:client-output-port      out]
                                  [:welcome-message         welcome-message]
                                  [:server-host&port        (host&port server-host server-port)]
                                  [:pasv-ip&ports           pasv-ip&ports]
                                  [:ssl-server-context      ssl-server-ctx]
                                  [:ssl-client-context      ssl-client-ctx]
                                  [:users-table             users-table]
                                  [:pasv-random-gen         random-gen]
                                  [:server-responses        server-responses]
                                  [:default-locale-encoding default-locale-encoding]
                                  [:default-root-dir        (if (path? default-root-dir)
                                                                (path->string default-root-dir)
                                                                default-root-dir)]
                                  [:login-fail-sleep        login-fail-sleep]
                                  [:max-login-attempts      max-login-attempts]
                                  [:bad-auth-table          bad-auth-table]
                                  [:port-timeout            port-timeout]
                                  [:pasv-timeout            pasv-timeout]
                                  [:data-timeout            data-timeout]
                                  [:passwd-sleep            passwd-sleep]
                                  [:allow-foreign-address   allow-foreign-address]
                                  [:log-enable?             log-enable?]
                                  [:log-output-port         log-output-port]
                                  [:debug?                  debug?]
                                  [:retr-transfer-rate      retr-transfer-rate]
                                  [:stor-transfer-rate      stor-transfer-rate]
                                  [:anonymous-retr-rate     anonymous-retr-rate]
                                  [:anonymous-stor-rate     anonymous-stor-rate]
                                  [:transfer-block-size     transfer-block-size]
                                  [:hide-dotobjects?        hide-dotobjects?]
                                  [:text-user&group-names?  text-user&group-names?]
                                  [:hide-ids?               hide-ids?]
                                  [:hide-names              hide-names]
                                  [:deny-names              deny-names]
                                  [:disable-commands        (append (if pasv-enable? null pasv-cmds)
                                                                    (if port-enable? null port-cmds)
                                                                    (if read-only? vfs-modify-cmds null)
                                                                    disable-ftp-commands)])
                             open-session #:wait? #t))
                     (λ ()
                       (clients-table-remove clients-table client-host)
                       ;; fix for racket-ssl
                       (when (ssl-port? out)
                         (ssl-abandon-port out))
                       (close-output-port out)
                       (close-input-port in)))))
                 (void)])))))
      
      (define/public (stop)
        (when (shutdown?)
          (shutdown-error 'ftp-server%_stop))
        (when (running?)
          (custodian-shutdown-all server-custodian)
          (set! server-custodian #f)
          (print-log-message "The ftp-server (~a/~a) is stopped." *server-host* *server-port*)))
      
      (define/public (shutdown)
        (when (shutdown?)
          (shutdown-error 'ftp-server%_shutdown))
        (stop)
        (when free-log-output-port?
          (close-output-port (log-port-out log-output-port)))
        (set! enabled? #f)
        (print-log-message "The ftp-server (~a/~a) is shutdown." *server-host* *server-port*))
      
      (define/public (running?)
        (and server-custodian (custodian? server-custodian)))
      
      (define/public (shutdown?)
        (not enabled?)))))

(define/contract ftp-server/cfg%
  
  (class/c [inherit-field welcome-message
                          server-host
                          server-port
                          ssl-protocol
                          ssl-key
                          ssl-certificate
                          max-allow-wait
                          port-timeout
                          pasv-timeout
                          data-timeout
                          session-timeout
                          max-clients-per-IP
                          max-clients
                          retr-transfer-rate
                          stor-transfer-rate
                          anonymous-retr-rate
                          anonymous-stor-rate
                          transfer-block-size
                          login-fail-sleep
                          max-login-attempts
                          passwd-sleep
                          hide-dotobjects?
                          text-user&group-names?
                          hide-ids?
                          pasv-enable?
                          port-enable?
                          read-only?
                          hide-names
                          deny-names
                          disable-ftp-commands
                          allow-foreign-address
                          pasv-ip&ports
                          default-real-user
                          default-root-dir
                          default-locale-encoding
                          log-enable?
                          log-output-port
                          debug?]
           [inherit useradd]
           [init-field (server-name&version string?)
                       (copyright string?)
                       (debug-mode? boolean?)
                       (server-config-file path/c)
                       (ftp-users-file path/c)]
           [load-users/list (->m list? void?)]
           [load-users/file (->*m [] [path/c] void?)]
           [read-config/list (->m list? void?)]
           [read-config/file (->*m [] [path/c] void?)])
  
  (class ftp-server%
    
    (inherit-field welcome-message
                   server-host
                   server-port
                   ssl-protocol
                   ssl-key
                   ssl-certificate
                   max-allow-wait
                   port-timeout
                   pasv-timeout
                   data-timeout
                   session-timeout
                   max-clients-per-IP
                   max-clients
                   retr-transfer-rate
                   stor-transfer-rate
                   anonymous-retr-rate
                   anonymous-stor-rate
                   transfer-block-size
                   login-fail-sleep
                   max-login-attempts
                   passwd-sleep
                   hide-dotobjects?
                   text-user&group-names?
                   hide-ids?
                   pasv-enable?
                   port-enable?
                   read-only?
                   hide-names
                   deny-names
                   disable-ftp-commands
                   allow-foreign-address
                   pasv-ip&ports
                   default-real-user
                   default-root-dir
                   default-locale-encoding
                   log-enable?
                   log-output-port
                   free-log-output-port?
                   debug?)
    
    (inherit useradd
             shutdown?)
    
    (init-field [server-name&version  default-server-name&version]
                [copyright default-copyright]
                [debug-mode? #f]
                [server-config-file #f]
                [ftp-users-file #f])
    
    (super-new [debug? debug-mode?])
    
    (define (check-shutdown src)
      (when (shutdown?)
        (error src "The ftp-server is shutdown.")))
    
    (define/public (load-users/list ftp-users)
      (check-shutdown 'ftp-server/cfg%_load-users/list)
      (with-handlers ([any/c (λ (e) 
                               (debug-print/exc e)
                               (error 'ftp-server/cfg%_load-users/list
                                      "Syntax error in users-config."))])
        (match ftp-users
          [`(ftp-users ,users ..1)
           (for [(user users)]
             (match user
               [`(user ,(? string? login) ,params ...)
                (let ([password          #f]
                      [real-user         #f]
                      [anonymous?        #t]
                      [hide-ids?         #t]
                      [hide-dotobjects?  #t]
                      [mfmt-enable?      #f]
                      [mff-enable?       #f]
                      [site-enable?      #f]
                      [permissions       (make-user-permissions #:read-file #t
                                                                #:enter-directory #t
                                                                #:listing-directory #t)]
                      [local-root-dir    #f])
                  (for [(param params)]
                    (match param
                      [`(password ,(? password-string? pass))
                       (set! password pass)]
                      [`(real-user ,(? string? sys-user))
                       (set! real-user sys-user)]
                      [`(anonymous? ,(? boolean? flag))
                       (set! anonymous? flag)]
                      [`(hide-ids? ,(or (? string? hide?) (? boolean? hide?)))
                       (set! hide-ids? hide?)]
                      [`(hide-dotobjects? ,(? boolean? flag))
                       (set! hide-dotobjects? flag)]
                      [`(mfmt-enable? ,(? boolean? flag))
                       (set! mfmt-enable? flag)]
                      [`(mff-enable? ,(? boolean? flag))
                       (set! mff-enable? flag)]
                      [`(site-enable? ,(? boolean? flag))
                       (set! site-enable? flag)]
                      [`(permissions ,rules ..1)
                       (set! permissions (apply make-user-permissions/rules rules))]
                      [`(local-root-dir ,(? path-string? dir))
                       (set! local-root-dir dir)]))
                  (useradd login
                           password
                           (or real-user
                               default-real-user)
                           #:anonymous? anonymous?
                           #:hide-ids? hide-ids?
                           #:hide-dotobjects? hide-dotobjects?
                           #:mfmt-enable? mfmt-enable?
                           #:mff-enable? mff-enable?
                           #:site-enable? site-enable?
                           #:permissions permissions
                           #:root-dir local-root-dir))]))])))
    
    (define/public (load-users/file [users-file ftp-users-file])
      (check-shutdown 'ftp-server/cfg%_load-users/file)
      (when (and users-file (path-string? users-file))
        (unless (and ftp-users-file (string-ci=? ftp-users-file users-file))
          (set! ftp-users-file users-file))
        (with-handlers ([any/c (λ (e) 
                                 (debug-print/exc e)
                                 (error 'ftp-server/cfg%_load-users/file
                                        "Please check your users-config file."))])
          (call-with-input-file users-file
            (λ (in) (load-users/list (read in)))))))
    
    (define/public (read-config/list config)
      (check-shutdown 'ftp-server/cfg%_read-config/list)
      (with-handlers ([any/c (λ (e)
                               (debug-print/exc e)
                               (error 'ftp-server/cfg%_read-config/list
                                      "Syntax error in server-config."))])
        (match config
          [`(ftp-server ,params ...)
           (let ([users-file #f]
                 [log-file #f])
             (for [(param params)]
               (match param
                 [`(welcome-message ,(? string? msg))
                  (set! welcome-message (format-welcome-msg msg))]
                 [`(host&port ,(? host-string? host) ,(? port-number? port))
                  (set!-values (server-host server-port) (values host port))]
                 [`(ssl ,params ..1)
                  (for [(param params)]
                    (match param
                      [`(protocol ,(? ssl-protocol? proto))
                       (set! ssl-protocol proto)]
                      [`(key ,(? path-string? key))
                       (set! ssl-key key)]
                      [`(certificate ,(? path-string? cert))
                       (set! ssl-certificate cert)]))]
                 [`(passive-ip&ports ,(? IP-addr? ip) ,(? port-number? from) ,(? port-number? to))
                  (set! pasv-ip&ports (make-passive-ip&ports ip from to))]
                 [`(max-allow-wait ,(? exact-positive-integer? sec))
                  (set! max-allow-wait sec)]
                 [`(port-timeout ,(? exact-positive-integer? sec))
                  (set! port-timeout sec)]
                 [`(pasv-timeout ,(? exact-positive-integer? sec))
                  (set! pasv-timeout sec)]
                 [`(data-timeout ,(? exact-positive-integer? sec))
                  (set! data-timeout sec)]
                 [`(session-timeout ,(? exact-positive-integer? sec))
                  (set! session-timeout sec)]
                 [`(max-clients-per-IP ,(? exact-positive-integer? num))
                  (set! max-clients-per-IP num)]
                 [`(max-clients ,(? exact-positive-integer? num))
                  (set! max-clients num)]
                 [`(retr-transfer-rate ,(? exact-positive-integer? num))
                  (set! retr-transfer-rate num)]
                 [`(retr-transfer-rate ,(? kilobyte? num))
                  (set! retr-transfer-rate (* (xbyte->number num) 1000))]
                 [`(retr-transfer-rate ,(? megabyte? num))
                  (set! retr-transfer-rate (* (xbyte->number num) 1000000))]
                 [`(stor-transfer-rate ,(? exact-positive-integer? num))
                  (set! stor-transfer-rate num)]
                 [`(stor-transfer-rate ,(? kilobyte? num))
                  (set! stor-transfer-rate (* (xbyte->number num) 1000))]
                 [`(stor-transfer-rate ,(? megabyte? num))
                  (set! stor-transfer-rate (* (xbyte->number num) 1000000))]
                 [`(anonymous-retr-rate ,(? exact-positive-integer? num))
                  (set! anonymous-retr-rate num)]
                 [`(anonymous-retr-rate ,(? kilobyte? num))
                  (set! anonymous-retr-rate (* (xbyte->number num) 1000))]
                 [`(anonymous-retr-rate ,(? megabyte? num))
                  (set! anonymous-retr-rate (* (xbyte->number num) 1000000))]
                 [`(anonymous-stor-rate ,(? exact-positive-integer? num))
                  (set! anonymous-stor-rate num)]
                 [`(anonymous-stor-rate ,(? kilobyte? num))
                  (set! anonymous-stor-rate (* (xbyte->number num) 1000))]
                 [`(anonymous-stor-rate ,(? megabyte? num))
                  (set! anonymous-stor-rate (* (xbyte->number num) 1000000))]
                 [`(transfer-block-size ,(? exact-positive-integer? num))
                  (set! transfer-block-size num)]
                 [`(transfer-block-size ,(? kilobyte? num))
                  (set! transfer-block-size (* (xbyte->number num) 1024))]
                 [`(transfer-block-size ,(? megabyte? num))
                  (set! transfer-block-size (* (xbyte->number num) (* 1024 1024)))]
                 [`(login-fail-sleep ,(? exact-nonnegative-integer? sec))
                  (set! login-fail-sleep sec)]
                 [`(max-login-attempts ,(? exact-positive-integer? count))
                  (set! max-login-attempts count)]
                 [`(passwd-sleep ,(? exact-nonnegative-integer? sec))
                  (set! passwd-sleep sec)]
                 [`(hide-dotobjects? ,(? boolean? flag))
                  (set! hide-dotobjects? flag)]
                 [`(text-user&group-names? ,(? boolean? flag))
                  (set! text-user&group-names? flag)]
                 [`(hide-ids? ,(or (? string? hide?) (? boolean? hide?)))
                  (set! hide-ids? hide?)]
                 [`(pasv-enable? ,(? boolean? flag))
                  (set! pasv-enable? flag)]
                 [`(port-enable? ,(? boolean? flag))
                  (set! port-enable? flag)]
                 [`(read-only? ,(? boolean? flag))
                  (set! read-only? flag)]
                 [`(hide-names ,(? list? names))
                  (set! hide-names names)]
                 [`(deny-names ,(? list? names))
                  (set! deny-names names)]
                 [`(disable-ftp-commands ,(? list? cmds))
                  (set! disable-ftp-commands cmds)]
                 [`(default-real-user ,(? string? usr))
                  (set! default-real-user usr)]
                 [`(allow-foreign-address ,(? boolean? flag))
                  (set! allow-foreign-address flag)]
                 [`(default-root-dir ,(? path-string? dir))
                  (set! default-root-dir dir)]
                 [`(log-enable? ,(? boolean? flag))
                  (set! log-enable? flag)]
                 [`(log-file ,(? path-string? file))
                  (set! log-file (format-file-name file))]
                 [`(users-file ,(? path-string? file))
                  (set! users-file file)]
                 [`(debug? ,(? boolean? flag))
                  (set! debug? (or debug-mode? flag))]))
             (when log-enable?
               (when free-log-output-port?
                 (close-output-port (log-port-out log-output-port)))
               (set! log-output-port (log-port (make-semaphore 1) (open-log-file log-file)))
               (set! free-log-output-port? #t))
             (when users-file
               (load-users/file users-file)))])))
    
    (define/public (read-config/file [config-file server-config-file])
      (check-shutdown 'ftp-server/cfg%_read-config/file)
      (when (and config-file (path-string? config-file))
        (unless (and server-config-file
                     (string-ci=? server-config-file config-file))
          (set! server-config-file config-file))
        (with-handlers ([any/c (λ (e) 
                                 (debug-print/exc e)
                                 (error 'ftp-server/cfg%_read-config/file
                                        "Please check your server-config file."))])
          (call-with-input-file config-file
            (λ (in) (read-config/list (read in)))))))
    
    (define/private (format-welcome-msg msg)
      (regexp-replace* #rx"%[Vv]|%[Cc]"
                       msg
                       (λ (prefix)
                         (case (string->symbol (string-upcase prefix))
                           [(%V) server-name&version]
                           [(%C) copyright]
                           [else prefix]))))
    
    (define (format-file-name spath)
      (parameterize ([date-display-format 'iso-8601])
        (regexp-replace #rx"\\*" spath (date->string (current-date)))))
    
    (define (open-log-file log-file)
      (if (and log-file (path-string? log-file))
          (begin
            (when (and (not (file-exists? log-file))
                       (path-only log-file))
              (make-directory* (path-only log-file)))
            (open-output-file log-file #:exists 'append))
          (current-output-port)))
    
    (define-syntax-rule (print-error msg v ...)
      (print-error-message 'ftp-server/cfg% msg v ...))
    
    (define-syntax-rule (debug-print msg v ...)
      (when debug-mode? (print-error msg v ...)))
    
    (define-syntax-rule (debug-print/exc e)
      (debug-print "caught an exception ~a" e))))
