#|
RktFTPd Core FTP Session Library
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
         (file "private/syntax.rkt")
         (file "private/net.rkt")
         (file "private/timers.rkt")
         (file "private/openssl.rkt")
         (file "system/filesystem.rkt")
         (file "user.rkt")
         (file "clients-table.rkt"))

(provide ftp-session/timer<%>
         ftp-session/timer%
         ftp-session/encoding<%>
         ftp-session/encoding%
         (struct-out ftp-command-info)
         ftp-session/command<%>
         ftp-session/command%
         ftp-session/path<%>
         ftp-session/path%
         ftp-session/client<%>
         ftp-session/client%
         ftp-session/server<%>
         ftp-session/server%
         ftp-session/auth<%>
         ftp-session/auth%
         (struct-out log-port)
         ftp-session/log<%>
         ftp-session/log%
         ftp-session/responses<%>
         ftp-session/responses%
         ftp-session/print<%>
         ftp-session/print%
         (struct-out host&port)
         (struct-out ftp-DTP/info)
         ftp-session/DTP<%>
         ftp-session/DTP%
         ftp-session/core<%>
         ftp-session/core%
         dummy-ftp-session%)

(provide/define-member-key
 ftp-session/timer<%>_set-session-timer
 ftp-session/timer<%>_reset-session-timer
 ftp-session/timer<%>_stop-session-timer
 ftp-session/timer<%>_cancel-session-timer)

(define-values (ftp-session/timer<%> ftp-session/timer%)
  (let-member-name ([set-session-timer    ftp-session/timer<%>_set-session-timer]
                    [reset-session-timer  ftp-session/timer<%>_reset-session-timer]
                    [stop-session-timer   ftp-session/timer<%>_stop-session-timer]
                    [cancel-session-timer ftp-session/timer<%>_cancel-session-timer])
    (let ([<%> (interface ()
                 set-session-timer
                 reset-session-timer
                 stop-session-timer
                 cancel-session-timer)])
      (values <%>
              (class* object% (<%>)
                (super-new)
                
                (define *session-timer* (make-timer 0 void))
                
                (define/public-final (set-session-timer timer)
                  (set! *session-timer* timer))
                
                (define/public-final (reset-session-timer)
                  (reset-timer *session-timer*))
                
                (define/public-final (stop-session-timer)
                  (stop-timer *session-timer*))
                
                (define/public-final (cancel-session-timer)
                  (cancel-timer *session-timer*)))))))

(provide/define-member-key
 ftp-session/encoding<%>_set-locale-encoding)

(define-values (ftp-session/encoding<%> ftp-session/encoding%)
  (let-member-name ([set-locale-encoding ftp-session/encoding<%>_set-locale-encoding])
    (let ([<%> (interface ()
                 default-locale-encoding
                 locale-encoding
                 set-locale-encoding
                 bytes->bytes/encoding
                 bytes->string/encoding
                 string->bytes/encoding
                 string->bytes/encoding*
                 print/encoding
                 print-crlf/encoding
                 printf-crlf/encoding)])
      (values <%>
              (mixin () (<%>)
                (super-new)
                
                (init [:default-locale-encoding "UTF-8"])
                
                (define *default-locale-encoding* :default-locale-encoding)
                (define *locale-encoding*         :default-locale-encoding)
                
                (define/public-final (default-locale-encoding)
                  *default-locale-encoding*)
                
                (define/public-final (locale-encoding)
                  *locale-encoding*)
                
                (define/public-final (set-locale-encoding enc)
                  (set! *locale-encoding* enc))
                
                (define/public (bytes->bytes/encoding encoding bstr)
                  (let*-values ([(conv) (bytes-open-converter encoding "UTF-8")]
                                [(result len status) (bytes-convert conv bstr)])
                    (bytes-close-converter conv)
                    (unless (eq? status 'complete)
                      (set! conv (bytes-open-converter *default-locale-encoding* "UTF-8"))
                      (set!-values (result len status) (bytes-convert conv bstr))
                      (bytes-close-converter conv))
                    result))
                
                (define/public (bytes->string/encoding encoding bstr)
                  (bytes->string/utf-8 (bytes->bytes/encoding encoding bstr)))
                
                (define/public (string->bytes/encoding encoding str)
                  (bytes->bytes/encoding encoding (string->bytes/utf-8 str)))
                
                (define/public (string->bytes/encoding* str)
                  (string->bytes/encoding *locale-encoding* str))
                
                (define/public (print/encoding encoding str out)
                  (write-bytes (string->bytes/encoding encoding str) out))
                
                (define/public (print-crlf/encoding encoding str out)
                  (print/encoding encoding str out)
                  (write-bytes #"\r\n" out)
                  (flush-output out))
                
                (define/public (printf-crlf/encoding encoding out form . args)
                  (print/encoding encoding (apply format form args) out)
                  (write-bytes #"\r\n" out)
                  (flush-output out)))))))

(define (ftp-command-info/param-type? param-type)
  (pair? (memq param-type '(none required custom))))

(struct ftp-command-info (name autonomous? generic param-type help)
  #:guard (λ (name autonomous? generic param-type help tag)
            (unless (symbol? name)
              (error tag "not a valid cmd-name"))
            (unless (boolean? autonomous?)
              (error tag "not a valid cmd-autonomous?"))
            (unless (generic? generic)
              (error tag "not a valid cmd-generic"))
            (unless (ftp-command-info/param-type? param-type)
              (error tag "not a valid cmd-param-type"))
            (unless (string? help)
              (error tag "not a valid help"))
            (values name autonomous? generic param-type help)))

(provide/define-member-key
 ftp-session/command<%>_use-ftp-command)

(define-values (ftp-session/command<%> ftp-session/command%)
  (let-member-name ([use-ftp-command   ftp-session/command<%>_use-ftp-command])
    (let ([<%> (interface ()
                 disabled-commands
                 used-ftp-commands
                 use-ftp-command
                 disable-ftp-command
                 ftp-command)])
      (values <%>
              (mixin () (<%>)
                (super-new)
                
                (init [:disable-commands null])
                
                (define symbol-upcase (compose string->symbol string-upcase symbol->string))
                
                (define *disabled-commands* (remove-duplicates (map symbol-upcase :disable-commands)))
                (define *ftp-commands*      (make-hasheq))
                
                (define/public-final (disabled-commands)
                  *disabled-commands*)
                
                (define/public-final (used-ftp-commands)
                  (hash-keys *ftp-commands*))
                
                (define/public (use-ftp-command cmd-id autonomous? generic param-type help)
                  (set! cmd-id (symbol-upcase cmd-id))
                  (unless (memq cmd-id *disabled-commands*)
                    (hash-set! *ftp-commands* cmd-id 
                               (ftp-command-info cmd-id autonomous? generic param-type help))))
                
                (define/public (disable-ftp-command cmd-id . cmd-ids)
                  (for [(cmd-id (cons cmd-id cmd-ids))]
                    (set! cmd-id (symbol-upcase cmd-id))
                    (unless (memq cmd-id *disabled-commands*)
                      (set! *disabled-commands* (cons cmd-id *disabled-commands*)))
                    (hash-remove! *ftp-commands* cmd-id)))
                
                (define/public (ftp-command cmd)
                  (hash-ref *ftp-commands* cmd #f)))))))

(provide/define-member-key
 ftp-session/path<%>_set-root-ftp-dir
 ftp-session/path<%>_set-current-ftp-dir)

(define-values (ftp-session/path<%> ftp-session/path%)
  (let-member-name ([set-root-ftp-dir ftp-session/path<%>_set-root-ftp-dir]
                    [set-current-ftp-dir ftp-session/path<%>_set-current-ftp-dir])
    (let ([<%> (interface ()
                 root-ftp-dir
                 set-root-ftp-dir
                 root-ftp-dir?
                 current-ftp-dir
                 set-current-ftp-dir
                 simplify-ftp-path
                 format-ftp-path
                 complete-ftp-path
                 ftp-path->path
                 ftp-path->simplify-path)])
      (values <%>
              (mixin () (<%>)
                (super-new)
                
                (init :default-root-dir)
                
                ;(define *default-root-dir* :default-root-dir)
                (define *root-dir*         :default-root-dir)
                (define *current-dir*      (bytes->path #"/" 'unix))
                
                (define/public-final (root-ftp-dir)
                  *root-dir*)
                
                (define/public-final (set-root-ftp-dir dir)
                  (set! *root-dir* dir))
                
                (define/public (root-ftp-dir? ftp-path)
                  (= (file-or-directory-identity (ftp-path->path ftp-path))
                     (file-or-directory-identity *root-dir*)))
                
                (define/public-final (current-ftp-dir)
                  *current-dir*)
                
                (define/public-final (set-current-ftp-dir dir)
                  (set! *current-dir* dir))
                
                (define/public (simplify-ftp-path path)
                  (simplify-path path #f))
                
                (define/public (format-ftp-path path . paths)
                  (let-values ([(base name dir?)
                                (split-path (simplify-path (apply build-path path paths) #f))])
                    (if (path? base)
                        (build-path base name)
                        name)))
                
                (define/public (complete-ftp-path path)
                  (path->complete-path path (current-ftp-dir)))
                
                (define/public (ftp-path->path path)
                  (reroot-path (complete-ftp-path path) (root-ftp-dir)))
                
                (define/public (ftp-path->simplify-path path)
                  (simplify-path (ftp-path->path path) #f)))))))

(provide/define-member-key
 ftp-session/client<%>_client-input-port
 ftp-session/client<%>_client-output-port)

(define-values (ftp-session/client<%> ftp-session/client%)
  (let-member-name ([client-input-port      ftp-session/client<%>_client-input-port]
                    [client-output-port     ftp-session/client<%>_client-output-port])
    (let ([<%> (interface ()
                 client-host
                 client-input-port
                 client-output-port)])
      (values <%>
              (mixin () (<%>)
                
                (super-new)
                
                (init :client-host
                      :client-input-port
                      :client-output-port)
                
                (define *client-host*        :client-host)
                (define *client-input-port*  :client-input-port)
                (define *client-output-port* :client-output-port)
                
                (define/public-final (client-host)
                  *client-host*)
                
                (define/public-final (client-input-port)
                  *client-input-port*)
                
                (define/public-final (client-output-port)
                  *client-output-port*))))))

(define-values (ftp-session/server<%> ftp-session/server%)
  (let ([<%> (interface ()
               server-host
               server-port)])
    (values <%>
            (mixin () (<%>)
              
              (super-new)
              
              (init :server-host&port)
              
              (define *server-host&port* :server-host&port)
              
              (define/public-final (server-host)
                (host&port-host *server-host&port*))
              
              (define/public-final (server-port)
                (host&port-port *server-host&port*))))))

(provide/define-member-key
 ftp-session/auth<%>_set-login
 ftp-session/auth<%>_set-user-info)

(define-values (ftp-session/auth<%> ftp-session/auth%)
  (let-member-name ([set-login     ftp-session/auth<%>_set-login]
                    [set-user-info ftp-session/auth<%>_set-user-info])
    (let ([<%> (interface ()
                 login
                 set-login
                 user-info
                 set-user-info
                 user-allow?)])
      (values <%>
              (mixin () (<%>)
                (super-new)
                
                (define *login*     #f)
                (define *user-info* #f)
                
                (define/public-final (login)
                  *login*)
                
                (define/public-final (set-login login)
                  (set! *login* login))
                
                (define/public-final (user-info)
                  *user-info*)
                
                (define/public-final (set-user-info info)
                  (set! *user-info* info))
                
                (define/public-final (user-allow? perm)
                  ((case perm
                     [(create-file) user-permissions-create-file]
                     [(append-file) user-permissions-append-file]
                     [(read-file) user-permissions-read-file]
                     [(delete-file) user-permissions-delete-file]
                     [(rename-file) user-permissions-rename-file]
                     [(enter-directory) user-permissions-enter-directory]
                     [(listing-directory) user-permissions-listing-directory]
                     [(create-directory) user-permissions-create-directory]
                     [(rename-directory) user-permissions-rename-directory]
                     [(delete-directory) user-permissions-delete-directory]
                     [else (error 'ftp-session/auth<%>_user-allow? "bad user permission, got ~e" perm)])
                   (ftp-user-permissions *user-info*))))))))

(struct log-port (sema out)
  #:guard (λ (sema out tag)
            (unless (semaphore? sema)
              (error tag "not a valid semaphore"))
            (unless (output-port? out)
              (error tag "not a valid output-port"))
            (values sema out)))

(provide/define-member-key
 ftp-session/log<%>_write-log-msg
 ftp-session/log<%>_debug-log-msg
 ftp-session/log<%>_debug-error)

(define-values (ftp-session/log<%> ftp-session/log%)
  (let-member-name ([write-log-msg   ftp-session/log<%>_write-log-msg]
                    [debug-log-msg   ftp-session/log<%>_debug-log-msg]
                    [debug-error     ftp-session/log<%>_debug-error])
    (let ([<%> (interface ()
                 log-output-port
                 write-log-msg
                 debug-log-msg
                 debug-error)])
      (values <%>
              (mixin (ftp-session/auth<%> ftp-session/client<%>) (<%>)
                (inherit login
                         client-host)
                
                (super-new)
                
                (init [:log-enable?     #f]
                      [:log-output-port (log-port (make-semaphore 1) (current-output-port))]
                      [:debug?          #f])
                
                (define *log-enable?*           :log-enable?)
                (define *debug?*                :debug?)
                (define *log-output-port*       :log-output-port)
                
                (define/public-final (log-output-port)
                  *log-output-port*)
                
                (define/private (print-msg debug? fmt . e)
                  (when *log-enable?*
                    (dynamic-wind
                     (λ ()
                       (semaphore-wait (log-port-sema *log-output-port*)))
                     (λ ()
                       (let ([out (log-port-out *log-output-port*)])
                         (parameterize ([date-display-format 'iso-8601])
                           (fprintf out "~a [~a]" (date->string (current-date) #t) (client-host)))
                         (when (login)
                           (fprintf out " ~a" (login)))
                         (fprintf out " ~a " (if debug? #\! #\:))
                         (apply fprintf out fmt e)
                         (newline out)
                         (flush-output out)))
                     (λ ()
                       (semaphore-post (log-port-sema *log-output-port*))))))
                
                (define/public (write-log-msg fmt . e)
                  (print-msg #f fmt . e))
                
                (define/public (debug-log-msg fmt . e)
                  (when *debug?*
                    (print-msg #t fmt . e)))
                
                (define/public (debug-error src fmt . e)
                  (debug-log-msg (string-append "[error ~s] " fmt) src . e)))))))

(provide/define-member-key
 ftp-session/responses<%>_server-responses
 ftp-session/responses<%>_set-current-language
 ftp-session/responses<%>_print*-response)

(define-values (ftp-session/responses<%> ftp-session/responses%)
  (let-member-name ([client-output-port   ftp-session/client<%>_client-output-port]
                    [debug-log-msg        ftp-session/log<%>_debug-log-msg]
                    [server-responses     ftp-session/responses<%>_server-responses]
                    [set-current-language ftp-session/responses<%>_set-current-language]
                    [print*-response      ftp-session/responses<%>_print*-response])
    (let ([<%> (interface ()
                 server-responses
                 current-language 
                 set-current-language 
                 used-languages
                 print-response
                 print*-response)])
      (values <%>
              (mixin (ftp-session/encoding<%> ftp-session/client<%> ftp-session/log<%>) (<%>)
                (inherit locale-encoding
                         print-crlf/encoding
                         client-output-port
                         debug-log-msg)
                
                (super-new)
                
                (init :server-responses)
                
                (define *server-responses* :server-responses)
                (define *lang-list*        (map car (hash-ref :server-responses 'SYNTAX-ERROR)))
                (define *current-lang*     (car *lang-list*))
                
                (define/public-final (server-responses)
                  *server-responses*)
                
                (define/public-final (current-language)
                  *current-lang*)
                
                (define/public-final (set-current-language lang)
                  (set! *current-lang* lang))
                
                (define/public-final (used-languages)
                  *lang-list*)
                
                (define/public (print-response encoding out current-lang server-responses response-tag . args)
                  (let ([response (apply format (cdr (assq current-lang (hash-ref server-responses response-tag))) args)])
                    (print-crlf/encoding encoding response out)))
                
                (define/public (print*-response response-tag . args)
                  (let ([response (apply format (cdr (assq *current-lang* (hash-ref *server-responses* response-tag))) args)])
                    (debug-log-msg response)
                    (print-crlf/encoding (locale-encoding) response (client-output-port)))))))))

(provide/define-member-key
 ftp-session/print<%>_print-crlf
 ftp-session/print<%>_printf-crlf)

(define-values (ftp-session/print<%> ftp-session/print%)
  (let-member-name ([server-responses   ftp-session/responses<%>_server-responses]
                    [client-output-port ftp-session/client<%>_client-output-port]
                    [print-crlf         ftp-session/print<%>_print-crlf]
                    [printf-crlf        ftp-session/print<%>_printf-crlf])
    (let ([<%> (interface ()
                 print-crlf
                 printf-crlf)])
      (values <%>
              (mixin (ftp-session/client<%> ftp-session/encoding<%>) (<%>)
                (inherit string->bytes/encoding
                         print-crlf/encoding
                         printf-crlf/encoding
                         locale-encoding
                         client-output-port)
                
                (super-new)
                
                (define/public (print-crlf txt)
                  (print-crlf/encoding (locale-encoding) txt (client-output-port)))
                
                (define/public (printf-crlf form . args)
                  (printf-crlf/encoding (locale-encoding) (client-output-port) form . args)))))))

(provide/define-member-key
 ftp-session/DTP<%>_ftp-store-file
 ftp-session/DTP<%>_ftp-data-transfer
 ftp-session/DTP<%>_current-DTP-info
 ftp-session/DTP<%>_net-connect
 ftp-session/DTP<%>_net-accept
 ftp-session/DTP<%>_make-DTP
 ftp-session/DTP<%>_kill-current-DTP
 ftp-session/DTP<%>_retrive-file-evt
 ftp-session/DTP<%>_store-file-evt
 ftp-session/DTP<%>_log-evt/retrive-file
 ftp-session/DTP<%>_log-evt/store-file
 ftp-session/DTP<%>_print-ascii
 ftp-session/DTP<%>_print-close-msg
 ftp-session/DTP<%>_print-connect-msg
 ftp-session/DTP<%>_print-abort-msg
 ftp-session/DTP<%>_set-restart-marker
 ftp-session/DTP<%>_set-representation-type)

(struct host&port (host port)
  #:guard (λ (host port tag)
            (unless (host-string? host)
              (error tag "not a valid host"))
            (unless (port-number? port)
              (error tag "not a valid port"))
            (values host port)))

(define (ftp-DTP-status? status)
  (pair? (memq status '(wait running shutdown))))

(struct ftp-DTP
  ([status #:mutable]
   host&port
   [connect-timer #:mutable]
   [transfer-timer #:mutable]
   custodian
   [infomsg #:mutable])
  #:guard (λ (status host&port connect-timer transfer-timer custodian infomsg tag)
            (unless (ftp-DTP-status? status)
              (error tag "not a valid dtp-status"))
            (unless (host&port? host&port)
              (error tag "not a valid dtp-host&port"))
            (unless (timer? connect-timer)
              (error tag "not a valid dtp-connect-timer"))
            (unless (timer? transfer-timer)
              (error tag "not a valid dtp-transfer-timer"))
            (unless (custodian? custodian)
              (error tag "not a valid dtp-custodian"))
            (unless (string? infomsg)
              (error tag "not a valid dtp-infomsg"))
            (values status host&port connect-timer transfer-timer custodian infomsg)))

(struct ftp-DTP/passive ftp-DTP (listener)
  #:guard (λ (status host&port connect-timer transfer-timer custodian infomsg listener tag)
            (unless (tcp-listener? listener)
              (error tag "not a valid dtp-listener"))
            (values status host&port connect-timer transfer-timer custodian infomsg listener)))

(define (ftp-DTP-type? type)
  (pair? (memq type '(active passive))))

(struct ftp-DTP/info (type host&port infomsg)
  #:transparent
  #:guard (λ (type host&port infomsg tag)
            (unless (ftp-DTP-type? type)
              (error tag "not a valid dtp-type"))
            (unless (host&port? host&port)
              (error tag "not a valid dtp-host&port"))
            (unless (string? infomsg)
              (error tag "not a valid dtp-infomsg"))
            (values type host&port infomsg)))

(define-values (ftp-session/DTP<%> ftp-session/DTP%)
  (let-member-name ([reset-session-timer     ftp-session/timer<%>_reset-session-timer]
                    [write-log-msg           ftp-session/log<%>_write-log-msg]
                    [debug-error             ftp-session/log<%>_debug-error]
                    [print*-response         ftp-session/responses<%>_print*-response]
                    [ftp-store-file          ftp-session/DTP<%>_ftp-store-file]
                    [ftp-data-transfer       ftp-session/DTP<%>_ftp-data-transfer]
                    [current-DTP-info        ftp-session/DTP<%>_current-DTP-info]
                    [net-connect             ftp-session/DTP<%>_net-connect]
                    [net-accept              ftp-session/DTP<%>_net-accept]
                    [make-DTP                ftp-session/DTP<%>_make-DTP]
                    [kill-current-DTP        ftp-session/DTP<%>_kill-current-DTP]
                    [retrive-file-evt        ftp-session/DTP<%>_retrive-file-evt]
                    [store-file-evt          ftp-session/DTP<%>_store-file-evt]
                    [log-evt/retrive-file    ftp-session/DTP<%>_log-evt/retrive-file]
                    [log-evt/store-file      ftp-session/DTP<%>_log-evt/store-file]
                    [print-ascii             ftp-session/DTP<%>_print-ascii]
                    [print-close-msg         ftp-session/DTP<%>_print-close-msg]
                    [print-connect-msg       ftp-session/DTP<%>_print-connect-msg]
                    [print-abort-msg         ftp-session/DTP<%>_print-abort-msg]
                    [set-restart-marker      ftp-session/DTP<%>_set-restart-marker]
                    [set-representation-type ftp-session/DTP<%>_set-representation-type])
    (let ([<%> (interface ()
                 representation-type
                 set-representation-type
                 transfer-mode
                 file-structure
                 kill-current-DTP
                 make-DTP
                 net-accept
                 net-connect
                 current-DTP-info
                 ftp-data-transfer
                 ftp-store-file
                 print-abort-msg
                 print-connect-msg
                 print-close-msg
                 print-ascii
                 store-file-evt
                 log-evt/store-file
                 retrive-file-evt
                 log-evt/retrive-file
                 set-restart-marker)])
      (values <%>
              (mixin
                  (ftp-session/timer<%>
                   ftp-session/responses<%>
                   ftp-session/auth<%>
                   ftp-session/log<%>
                   ftp-session/path<%>
                   ftp-session/encoding<%>)
                
                (<%>)
                
                (inherit print/encoding
                         locale-encoding
                         print*-response
                         user-info
                         root-ftp-dir
                         write-log-msg
                         debug-error
                         ftp-path->path
                         ftp-path->simplify-path
                         reset-session-timer)
                
                (super-new)
                
                (init [:ssl-server-context  #f]
                      [:ssl-client-context  #f]
                      [:retr-transfer-rate  #f]
                      [:stor-transfer-rate  #f]
                      [:anonymous-retr-rate #f]
                      [:anonymous-stor-rate #f]
                      [:restart-marker      #f]
                      [:representation-type 'ASCII]
                      [:transfer-mode       'Stream]
                      [:file-structure      'File]
                      [:port-timeout        15]
                      [:pasv-timeout        15]
                      [:data-timeout        15]
                      [:transfer-block-size (* 16 1024)])
                
                (define *ssl-server-context* :ssl-server-context)
                (define *ssl-client-context* :ssl-client-context)
                
                (define *retr-transfer-rate* :retr-transfer-rate)
                (define *stor-transfer-rate* :stor-transfer-rate)
                
                (define *anonymous-retr-rate* :anonymous-retr-rate)
                (define *anonymous-stor-rate* :anonymous-stor-rate)
                
                (define *restart-marker* :restart-marker)
                
                (define *representation-type* :representation-type)
                (define *transfer-mode*       :transfer-mode)
                (define *file-structure*      :file-structure)
                
                (define *port-timeout* :port-timeout)
                (define *pasv-timeout* :pasv-timeout)
                (define *data-timeout* :data-timeout)
                
                (define *transfer-block-size* :transfer-block-size)
                
                (define curproc
                  (ftp-DTP 'shutdown
                           (host&port "127.0.0.1" 20)
                           (make-timer 0 void)
                           (make-timer 0 void)
                           (make-custodian)
                           "Dummy process."))
                
                (define/public (print-abort-msg)
                  (print*-response 'TRANSFER-ABORTED))
                
                (define/public (print-connect-msg)
                  (print*-response 'OPEN-DATA-CONNECTION (representation-type)))
                
                (define/public (print-close-msg)
                  (print*-response 'TRANSFER-OK))
                
                (define/public (print-ascii data out)
                  (print/encoding (locale-encoding) data out))
                
                (define/public (store-file-evt dtp ftp-path exists-mode)
                  (set-ftp-DTP-infomsg! dtp (format "~a data to file '~a'"
                                                    (if (eq? exists-mode 'append) "Append" "Store")
                                                    (ftp-path->simplify-path ftp-path))))
                
                (define/public (log-evt/store-file ftp-path exists-mode)
                  (write-log-msg "~a data to file '~a'"
                                 (if (eq? exists-mode 'append) "Append" "Store")
                                 (ftp-path->simplify-path ftp-path)))
                
                (define/public (retrive-file-evt dtp ftp-path)
                  (set-ftp-DTP-infomsg! dtp
                                        (format "Read file '~a'" (ftp-path->simplify-path ftp-path))))
                
                (define/public (log-evt/retrive-file ftp-path)
                  (write-log-msg "Read file '~a'" (ftp-path->simplify-path ftp-path)))
                
                (define/public-final (set-restart-marker marker)
                  (set! *restart-marker* marker))
                
                (define/public-final (representation-type)
                  *representation-type*)
                
                (define/public-final (set-representation-type type)
                  (set! *representation-type* type))
                
                (define/public-final (transfer-mode)
                  *transfer-mode*)
                
                (define/public-final (file-structure)
                  *file-structure*)
                
                (define/public (kill-current-DTP)
                  (kill-DTP curproc))
                
                (define (kill-DTP dtp)
                  (unless (eq? (ftp-DTP-status dtp) 'shutdown)
                    (set-ftp-DTP-status! dtp 'shutdown)
                    (cancel-timer (ftp-DTP-connect-timer dtp))
                    (cancel-timer (ftp-DTP-transfer-timer dtp))
                    (custodian-shutdown-all (ftp-DTP-custodian dtp))))
                
                (define/public (make-DTP passive? host port)
                  (parameterize ([current-custodian (make-custodian)])
                    (when (eq? (ftp-DTP-status curproc) 'wait)
                      (kill-DTP curproc))
                    (let ([dtp (apply (if passive?
                                          ftp-DTP/passive
                                          ftp-DTP)
                                      'wait 
                                      (host&port host port)
                                      (make-timer 0 void)
                                      (make-timer 0 void)
                                      (current-custodian)
                                      "Dummy process."
                                      (if passive?
                                          (list (tcp-listen port 1 #t host))
                                          null))])
                      (set-ftp-DTP-connect-timer! dtp
                                                  (make-timer (if passive?
                                                                  *pasv-timeout*
                                                                  *port-timeout*)
                                                              (λ ()
                                                                (with-handlers ([exn?
                                                                                 (λ (e)
                                                                                   (debug-error 'ftp-session/DTP<%>_make-DTP
                                                                                                "connect-timer@exn ~a"
                                                                                                (exn-message e)))]
                                                                                [any/c
                                                                                 (λ (e)
                                                                                   (debug-error 'ftp-session/DTP<%>_make-DTP
                                                                                                "connect-timer@any ~a"
                                                                                                e))])
                                                                  (kill-DTP dtp)))))
                      (set-ftp-DTP-transfer-timer! dtp
                                                   (make-timer *data-timeout*
                                                               (λ ()
                                                                 (with-handlers ([exn?
                                                                                  (λ (e)
                                                                                    (debug-error 'ftp-session/DTP<%>_make-DTP
                                                                                                 "transfer-timer@exn ~a"
                                                                                                 (exn-message e)))]
                                                                                 [any/c
                                                                                  (λ (e)
                                                                                    (debug-error 'ftp-session/DTP<%>_make-DTP
                                                                                                 "transfer-timer@any ~a"
                                                                                                 e))])
                                                                   (print-abort-msg)
                                                                   (kill-DTP dtp)))))
                      (set! curproc dtp)
                      (start-timer (ftp-DTP-connect-timer dtp))
                      dtp)))
                
                (define/public (net-accept tcp-listener)
                  (let-values ([(in out) (tcp-accept tcp-listener)])
                    (if *ssl-server-context* 
                        (ports->ssl-ports in out 
                                          #:mode 'accept
                                          #:context *ssl-server-context*
                                          #:close-original? #t
                                          #:shutdown-on-close? #t) 
                        (values in out))))
                
                (define/public (net-connect host port)
                  (let-values ([(in out) (tcp-connect host port)])
                    (if *ssl-client-context* 
                        (ports->ssl-ports in out 
                                          #:mode 'connect
                                          #:context *ssl-client-context*
                                          #:close-original? #t
                                          #:shutdown-on-close? #t) 
                        (values in out))))
                
                (define/public (current-DTP-info)
                  (and (eq? (ftp-DTP-status curproc) 'running)
                       (ftp-DTP/info (if (ftp-DTP/passive? curproc)
                                         'passive
                                         'active)
                                     (ftp-DTP-host&port curproc)
                                     (ftp-DTP-infomsg curproc))))
                
                (define (transfer/unlimited proc-timer in out)
                  (let ([buf (make-bytes *transfer-block-size*)])
                    (let loop ([n (read-bytes! buf in)])
                      (unless (eof-object? n)
                        (reset-timer proc-timer)
                        (reset-session-timer)
                        (write-bytes buf out 0 n)
                        (flush-output out)
                        (loop (read-bytes! buf in))))))
                
                (define (transfer/limited transfer-rate
                                          proc-timer
                                          in out)
                  (let* ([min-blks 10]
                         [blks (let ([n (if (or (<= transfer-rate *transfer-block-size*)
                                                (<= (quotient transfer-rate *transfer-block-size*) min-blks))
                                            min-blks
                                            (quotient transfer-rate *transfer-block-size*))])
                                 (if (> (quotient transfer-rate n) (remainder transfer-rate n))
                                     n
                                     (add1 n)))]
                         [rest-bytes (remainder transfer-rate blks)]
                         [delay (/ (if (exact-positive-integer? rest-bytes) (add1 blks) blks))]
                         [buf (make-bytes (quotient transfer-rate blks))])
                    (let/cc return
                      (let loop ()
                        (for ([i (in-range blks)])
                          (let ([t0 (current-inexact-milliseconds)]
                                [n (read-bytes! buf in)])
                            (when (eof-object? n)
                              (return))
                            (reset-timer proc-timer)
                            (reset-session-timer)
                            (write-bytes buf out 0 n)
                            (flush-output out)
                            (let ([t1 (/ (- (current-inexact-milliseconds) t0) 1000)])
                              (when (< t1 delay)
                                (sleep (- delay t1))))))
                        (when (exact-positive-integer? rest-bytes)
                          (let ([t0 (current-inexact-milliseconds)]
                                [n (read-bytes! buf in 0 rest-bytes)])
                            (when (eof-object? n)
                              (return))
                            (reset-timer proc-timer)
                            (reset-session-timer)
                            (write-bytes buf out 0 n)
                            (flush-output out)
                            (let ([t1 (/ (- (current-inexact-milliseconds) t0) 1000)])
                              (when (< t1 delay)
                                (sleep (- delay t1))))))
                        (loop)))))
                
                (define (retr-rate)
                  (or (and (ftp-user-anonymous? (user-info))
                           *anonymous-retr-rate*
                           (or (and *retr-transfer-rate*
                                    (if (< *anonymous-retr-rate* *retr-transfer-rate*)
                                        *anonymous-retr-rate*
                                        *retr-transfer-rate*))
                               *anonymous-retr-rate*))
                      *retr-transfer-rate*))
                
                (define (stor-rate)
                  (or (and (ftp-user-anonymous? (user-info))
                           *anonymous-stor-rate*
                           (or (and *stor-transfer-rate*
                                    (if (< *anonymous-stor-rate* *stor-transfer-rate*)
                                        *anonymous-stor-rate*
                                        *stor-transfer-rate*))
                               *anonymous-stor-rate*))
                      *stor-transfer-rate*))
                
                (define (file-transfer transfer-rate proc-timer in out)
                  (if (exact-positive-integer? transfer-rate)
                      (transfer/limited transfer-rate proc-timer in out)
                      (transfer/unlimited proc-timer in out)))
                
                (define-syntax-rule (DTP->host dtp)
                  (host&port-host (ftp-DTP-host&port dtp)))
                
                (define-syntax-rule (DTP->port dtp)
                  (host&port-port (ftp-DTP-host&port dtp)))
                
                (define/public (ftp-data-transfer data [file? #f])
                  (if (eq? (ftp-DTP-status curproc) 'wait)
                      (if (ftp-DTP/passive? curproc)
                          (passive-data-transfer curproc data file?)
                          (active-data-transfer curproc data file?))
                      (print-abort-msg)))
                
                (define/private (active-data-transfer DTP data file?)
                  (parameterize ([current-custodian (ftp-DTP-custodian DTP)])
                    (void
                     (thread
                      (λ ()
                        (set-ftp-DTP-status! DTP 'running)
                        (when file? (retrive-file-evt DTP data))
                        (with-handlers ([any/c (λ (e) (print-abort-msg))])
                          (let-values ([(in out) (net-connect (DTP->host DTP) (DTP->port DTP))])
                            (print-connect-msg)
                            (cancel-timer (ftp-DTP-connect-timer DTP))
                            (start-timer (ftp-DTP-transfer-timer DTP))
                            (if file?
                                (call-with-input-file (ftp-path->path data)
                                  (λ (in)
                                    (log-evt/retrive-file data)
                                    (file-transfer (retr-rate) (ftp-DTP-transfer-timer DTP) in out)))
                                (case *representation-type*
                                  ((ASCII)
                                   (print-ascii data out))
                                  ((Image)
                                   (write-bytes data out))))
                            (flush-output out))
                          (print-close-msg))
                        (kill-DTP DTP))))))
                
                (define/private (passive-data-transfer DTP data file?)
                  (parameterize ([current-custodian (ftp-DTP-custodian DTP)])
                    (void
                     (thread
                      (λ ()
                        (set-ftp-DTP-status! DTP 'running)
                        (when file? (retrive-file-evt DTP data))
                        (with-handlers ([any/c (λ (e) (print-abort-msg))])
                          (let-values ([(in out) (net-accept (ftp-DTP/passive-listener DTP))])
                            (print-connect-msg)
                            (cancel-timer (ftp-DTP-connect-timer DTP))
                            (start-timer (ftp-DTP-transfer-timer DTP))
                            (if file?
                                (call-with-input-file (ftp-path->path data)
                                  (λ (in)
                                    (log-evt/retrive-file data)
                                    (file-transfer (retr-rate) (ftp-DTP-transfer-timer DTP) in out)))
                                (case *representation-type*
                                  ((ASCII)
                                   (print-ascii data out))
                                  ((Image)
                                   (write-bytes data out))))
                            (close-output-port out)) ; required for ssl (fix for FileZilla)
                          (print-close-msg))
                        (kill-DTP DTP))))))
                
                (define/public (ftp-store-file ftp-path exists-mode)
                  (if (eq? (ftp-DTP-status curproc) 'wait)
                      (if (ftp-DTP/passive? curproc)
                          (passive-store-file curproc ftp-path exists-mode)
                          (active-store-file curproc ftp-path exists-mode))
                      (print-abort-msg)))
                
                (define/private (active-store-file DTP ftp-path exists-mode)
                  (parameterize ([current-custodian (ftp-DTP-custodian DTP)])
                    (void
                     (thread
                      (λ ()
                        (set-ftp-DTP-status! DTP 'running)
                        (store-file-evt DTP ftp-path exists-mode)
                        (with-handlers ([any/c (λ (e) (print-abort-msg))])
                          (let-values ([(in out) (net-connect (DTP->host DTP) (DTP->port DTP))])
                            (print-connect-msg)
                            (cancel-timer (ftp-DTP-connect-timer DTP))
                            (start-timer (ftp-DTP-transfer-timer DTP))
                            (let* ([file-path (ftp-path->path ftp-path)]
                                   [exists? (file-exists? file-path)])
                              (call-with-output-file file-path
                                (λ (out)
                                  (unless exists?
                                    (chown file-path (ftp-user-uid (user-info)) (ftp-user-gid (user-info))))
                                  (when *restart-marker*
                                    (file-position out *restart-marker*)
                                    (set! *restart-marker* #f))
                                  (log-evt/store-file ftp-path exists-mode)
                                  (file-transfer (stor-rate) (ftp-DTP-transfer-timer DTP) in out)
                                  (flush-output out))
                                #:mode 'binary
                                #:exists exists-mode)))
                          (print-close-msg))
                        (kill-DTP DTP))))))
                
                (define/private (passive-store-file DTP ftp-path exists-mode)
                  (parameterize ([current-custodian (ftp-DTP-custodian DTP)])
                    (void
                     (thread
                      (λ ()
                        (set-ftp-DTP-status! DTP 'running)
                        (store-file-evt DTP ftp-path exists-mode)
                        (with-handlers ([any/c (λ (e) (print-abort-msg))])
                          (let-values ([(in out) (net-accept (ftp-DTP/passive-listener DTP))])
                            (print-connect-msg)
                            (cancel-timer (ftp-DTP-connect-timer DTP))
                            (start-timer (ftp-DTP-transfer-timer DTP))
                            (let* ([file-path (ftp-path->path ftp-path)]
                                   [exists? (file-exists? file-path)])
                              (call-with-output-file file-path
                                (λ (out)
                                  (unless exists? 
                                    (chown file-path (ftp-user-uid (user-info)) (ftp-user-gid (user-info))))
                                  (when *restart-marker*
                                    (file-position out *restart-marker*)
                                    (set! *restart-marker* #f))
                                  (log-evt/store-file ftp-path exists-mode)
                                  (file-transfer (stor-rate) (ftp-DTP-transfer-timer DTP) in out)
                                  (flush-output out))
                                #:mode 'binary
                                #:exists exists-mode)))
                          (print-close-msg))
                        (kill-DTP DTP)))))))))))

(define-values (ftp-session/core<%> ftp-session/core%)
  (let-member-name ([set-session-timer      ftp-session/timer<%>_set-session-timer]
                    [reset-session-timer    ftp-session/timer<%>_reset-session-timer]
                    [stop-session-timer     ftp-session/timer<%>_stop-session-timer]
                    [cancel-session-timer   ftp-session/timer<%>_cancel-session-timer]
                    [kill-current-DTP       ftp-session/DTP<%>_kill-current-DTP]
                    [debug-log-msg          ftp-session/log<%>_debug-log-msg]
                    [debug-error            ftp-session/log<%>_debug-error]
                    [client-input-port      ftp-session/client<%>_client-input-port]
                    [client-output-port     ftp-session/client<%>_client-output-port]
                    [print-crlf             ftp-session/print<%>_print-crlf]
                    [printf-crlf            ftp-session/print<%>_printf-crlf]
                    [print*-response        ftp-session/responses<%>_print*-response])
    (let ([<%> (interface ()
                 open-session
                 close-session
                 session-closed?
                 max-request-length
                 set-max-request-length)])
      (values <%>
              (mixin
                  (ftp-session/timer<%>
                   ftp-session/print<%>
                   ftp-session/server<%>
                   ftp-session/client<%>
                   ftp-session/responses<%>
                   ftp-session/log<%>
                   ftp-session/auth<%>
                   ftp-session/command<%> 
                   ftp-session/encoding<%> 
                   ftp-session/DTP<%>) 
                
                (<%>)
                
                (inherit set-session-timer
                         reset-session-timer
                         stop-session-timer
                         cancel-session-timer
                         print-crlf
                         printf-crlf
                         print*-response
                         kill-current-DTP
                         locale-encoding
                         bytes->string/encoding
                         ftp-command
                         client-input-port
                         client-output-port
                         user-info
                         debug-log-msg
                         debug-error)
                
                (super-new)
                
                (init [:session-timeout    120]
                      [:welcome-message    "Racket FTP Server!"]
                      [:max-request-length 512])
                
                (define *session-custodian*  (make-custodian))
                (define *session-timeout*    :session-timeout)
                (define *welcome-message*    :welcome-message)
                (define *max-request-length* :max-request-length)
                (define *closed?*            #f)
                
                (define/public-final (max-request-length)
                  *max-request-length*)
                
                (define/public-final (set-max-request-length max-length)
                  (set! *max-request-length* max-length))
                
                (define (check-closed src)
                  (when *closed?*
                    (error src "The ftp-session is closed.")))
                
                (define/public (open-session #:wait? [wait? #f])
                  (check-closed 'open-session)
                  (parameterize ([current-custodian *session-custodian*])
                    ((if wait? thread-wait void)
                     (thread
                      (λ ()
                        (interpret)
                        (close-session))))))
                
                (define/public (close-session)
                  (check-closed 'close-session)
                  (cancel-session-timer)
                  (kill-current-DTP)
                  (custodian-shutdown-all *session-custodian*)
                  (set! *closed?* #t))
                
                (define/public (session-closed?)
                  *closed?*)
                
                (define/private (read-request/bytes in)
                  (let ([out (open-output-bytes)])
                    (for ([i *max-request-length*])
                      #:break (or (eof-object? (peek-byte in))
                                  (= (peek-byte in) 13)
                                  (= (peek-byte in) 10))
                      (write-byte (read-byte in) out))
                    (get-output-bytes out)))
                
                (define/private (read-request in)
                  (if (eof-object? (peek-byte in))
                      eof
                      (let ([req (read-request/bytes in)])
                        (if (< (bytes-length req) *max-request-length*)
                            (and (not (eof-object? (peek-byte in)))
                                 (= (peek-byte in) 13)
                                 (read-byte in)
                                 (not (eof-object? (peek-byte in)))
                                 (= (peek-byte in) 10)
                                 (read-byte in))
                            (do ()
                              [(or (eof-object? (peek-byte in))
                                   (= (peek-byte in) 13)
                                   (= (peek-byte in) 10))
                               (and (not (eof-object? (peek-byte in)))
                                    (= (peek-byte in) 13)
                                    (read-byte in)
                                    (not (eof-object? (peek-byte in)))
                                    (= (peek-byte in) 10)
                                    (read-byte in))]
                              (read-byte in)))
                        (let ([line (string-trim (bytes->string/encoding (locale-encoding) req))])
                          (if (zero? (string-length line))
                              (read-request in)
                              line)))))
                
                (define/private (parse-request req)
                  (let ([matches (regexp-match #rx"^([A-z]+)[ ]+(.+)" req)])
                    (values
                     (string->symbol (string-upcase (if matches (cadr matches) req)))
                     (and matches
                          (string-trim (caddr matches))))))
                
                (define/private (interpret)
                  (with-handlers ([exn? (λ (e)
                                          (debug-error 'ftp-session/core<%>_interpret "top@exn ~a" (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-session/core<%>_interpret "top@any ~a" e))])
                    (do ([p (regexp-split #rx"\n|\r" *welcome-message*) (cdr p)])
                      [(null? (cdr p)) (printf-crlf "220 ~a" (car p))]
                      (printf-crlf "220-~a" (car p)))
                    (let ([session-timer
                           (make-timer *session-timeout*
                                       (λ ()
                                         (with-handlers ([exn?
                                                          (λ (e)
                                                            (debug-error 'ftp-session/core<%>_interpret
                                                                         "session-timer@exn ~a"
                                                                         (exn-message e)))]
                                                         [any/c
                                                          (λ (e)
                                                            (debug-error 'ftp-session/core<%>_interpret
                                                                         "session-timer@any ~a"
                                                                         e))])
                                           (print*-response 'SESSION-TIMEOUT)
                                           (close-session))))])
                      (set-session-timer session-timer)
                      (start-timer session-timer))
                    (let loop ([req (read-request (client-input-port))])
                      (unless (eof-object? req)
                        (stop-session-timer)
                        (let-values ([(cmd arg) (parse-request req)])
                          (if cmd
                              (let ([cmdinfo (ftp-command cmd)])
                                (if arg
                                    (debug-log-msg "~a ~a" cmd arg)
                                    (debug-log-msg (symbol->string cmd)))
                                (if cmdinfo
                                    (with-handlers ([exn:fail:filesystem? 
                                                     (λ (e)
                                                       (debug-error 'ftp-session/core<%>_interpret
                                                                    "cmd@exn:fail:filesystem ~a"
                                                                    (exn-message e))
                                                       (print-crlf "550 System error."))]
                                                    [exn? 
                                                     (λ (e)
                                                       (debug-error 'ftp-session/core<%>_interpret
                                                                    "cmd@exn ~a"
                                                                    (exn-message e))
                                                       (print*-response 'UNKNOWN-ERROR))]
                                                    [any/c 
                                                     (λ (e)
                                                       (debug-error 'ftp-session/core<%>_interpret
                                                                    "cmd@any ~a"
                                                                    e)
                                                       (print*-response 'UNKNOWN-ERROR))])
                                      (if (or (user-info) 
                                              (ftp-command-info-autonomous? cmdinfo))
                                          (case (ftp-command-info-param-type cmdinfo)
                                            [(none)
                                             (if arg
                                                 (print*-response 'INVALID-CMD-SYNTAX)
                                                 (send-generic this (ftp-command-info-generic cmdinfo)))]
                                            [(required)
                                             (if arg
                                                 (send-generic this (ftp-command-info-generic cmdinfo) arg)
                                                 (print*-response 'INVALID-CMD-SYNTAX))]
                                            [(custom)
                                             (send-generic this (ftp-command-info-generic cmdinfo) arg)])
                                          (print*-response 'PLEASE-LOGIN)))
                                    (print*-response 'CMD-NOT-IMPLEMENTED cmd)))
                              (print*-response 'INVALID-CMD-SYNTAX)))
                        (reset-session-timer)
                        (sleep 0.005)
                        (loop (read-request (client-input-port))))))))))))

(define dummy-ftp-session%
  (ftp-session/core%
   (ftp-session/DTP%
    (ftp-session/print%
     (ftp-session/responses%
      (ftp-session/log%
       (ftp-session/auth%
        (ftp-session/server%
         (ftp-session/client%
          (ftp-session/path%
           (ftp-session/command%
            (ftp-session/encoding%
             ftp-session/timer%))))))))))))
