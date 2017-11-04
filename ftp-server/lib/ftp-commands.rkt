#|
RktFTPd Core FTP Commands Library
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
         (only-in (file "system/ffi-core.rkt")
                  exn:posix?
                  exn:posix-errno
                  throw-errno
                  errno->symbol)
         (file "system/filesystem.rkt")
         (file "system/process.rkt")
         (file "system/account.rkt")
         (file "private/net.rkt")
         (file "private/syntax.rkt")
         (file "user.rkt")
         (file "ftp-session.rkt"))

(provide ftp-session/fs<%>
         ftp-session/fs%
         ftp-commands/access-control<%>
         ftp-commands/access-control%
         (struct-out passive-ip&ports)
         (contract-out [make-passive-ip&ports (IP-addr? port-number? port-number? . -> . passive-ip&ports?)])
         ftp-commands/transfer-parameters<%>
         ftp-commands/transfer-parameters%
         ftp-commands/service<%>
         ftp-commands/service%
         ftp-session/rfc959%)

(define-values (ftp-session/fs<%> ftp-session/fs%)
  (let-member-name ()
    (let ([<%> (interface ()
                 hide-ids?
                 text-user&group-names?
                 hide-dotobjects?
                 unhide-name?
                 legal-ftp-obj?
                 file-or-dir-stat
                 stat->user-mode
                 stat->user-mode/root
                 file-or-dir-chmod
                 file-or-dir-chown
                 file-unlink
                 make-dir
                 remove-dir
                 rename-file-or-dir
                 file-or-dir-utime
                 check-dir-access/exn
                 check-file-access/exn
                 file-stat)])
      (values <%>
              (mixin (ftp-session/path<%> ftp-session/auth<%>) (<%>)
                (super-new)
                
                (inherit ftp-path->path
                         user-info)
                
                (init [:deny-names             null]
                      [:hide-names             null]
                      [:text-user&group-names? #t]
                      [:hide-ids?              #f]
                      [:hide-dotobjects?       #f])
                
                (define *deny-names*             :deny-names)
                (define *hide-names*             :hide-names)
                (define *hide-ids?*              :hide-ids?)
                (define *text-user&group-names?* :text-user&group-names?)
                (define *hide-dotobjects?*       :hide-dotobjects?)
                
                (define/public-final (hide-ids?)
                  *hide-ids?*)
                
                (define/public-final (text-user&group-names?)
                  *text-user&group-names?*)
                
                (define/public-final (hide-dotobjects?)
                  (or *hide-dotobjects?* (ftp-user-hide-dotobjects? (user-info))))
                
                (define (deny-ftp-obj? ftp-path deny-names)
                  (and (pair? deny-names)
                       (ormap (λ (name)
                                (and (not (string=? name "")) 
                                     (not (string=? name ".."))
                                     (ormap (λ (mask) (regexp-match? mask name))
                                            deny-names)))
                              (regexp-split #rx"[/\\\\]+" (simplify-path ftp-path #f)))))
                
                (define/public (unhide-name? name)
                  (not (ormap (λ (mask) (regexp-match? mask name))
                              *hide-names*)))
                
                (define/public (legal-ftp-obj? ftp-path)
                  (not (deny-ftp-obj? ftp-path *deny-names*)))
                
                (define-syntax-rule (call/safe fun args ...)
                  (call/eid (λ () (fun args ...))
                            #:egid (ftp-user-gid (user-info))
                            #:euid (ftp-user-uid (user-info))))
                
                (define/public (file-or-dir-stat ftp-path)
                  (call/safe stat (ftp-path->path ftp-path)))
                
                (define/public (stat->user-mode stat)
                  (cond
                    [(= (ftp-user-uid (user-info)) (statinfo-uid stat))
                     (arithmetic-shift (bitwise-and (statinfo-mode stat) #o700) -6)]
                    [(grpmember? (statinfo-gid stat) (ftp-user-uid (user-info)))
                     (arithmetic-shift (bitwise-and (statinfo-mode stat) #o70) -3)]
                    [else 
                     (bitwise-and (statinfo-mode stat) 7)]))
                
                (define/public (stat->user-mode/root stat)
                  (if (grpmember? root-gid (ftp-user-uid (user-info)))
                      7
                      (stat->user-mode stat)))
                
                (define/public (file-or-dir-chmod ftp-path mode)
                  (call/safe chmod (ftp-path->path ftp-path) mode))
                
                (define/public (file-or-dir-chown ftp-path uid gid)
                  (call/safe chown (ftp-path->path ftp-path) uid gid))
                
                (define/public (file-unlink ftp-path)
                  (call/safe unlink (ftp-path->path ftp-path)))
                
                (define/public (make-dir ftp-path)
                  (call/safe mkdir (ftp-path->path ftp-path)))
                
                (define/public (remove-dir ftp-path)
                  (call/safe rmdir (ftp-path->path ftp-path)))
                
                (define/public (rename-file-or-dir old-ftp-path new-ftp-path)
                  (call/safe rename (ftp-path->path old-ftp-path) (ftp-path->path new-ftp-path)))
                
                (define/public (file-or-dir-utime ftp-path actime modtime)
                  (call/safe utime (ftp-path->path ftp-path) actime modtime))
                
                (define/public (check-file-access/exn ftp-path mode #:extra-perm [allow? #t])
                  (let ([stat (file-or-dir-stat ftp-path)])
                    (if (stat-file? stat)
                        (unless (and allow?
                                     (= (bitwise-and (stat->user-mode/root stat) mode) mode)
                                     (legal-ftp-obj? ftp-path))
                          (throw-errno #:id 'check-file-access/exn #:error 'EACCES))
                        (throw-errno #:id 'check-file-access/exn #:error 'ENOENT))))
                
                (define/public (check-dir-access/exn ftp-path mode #:extra-perm [allow? #t])
                  (let ([stat (file-or-dir-stat ftp-path)])
                    (if (stat-directory? stat)
                        (unless (and allow?
                                     (= (bitwise-and (stat->user-mode/root stat) mode) mode)
                                     (legal-ftp-obj? ftp-path))
                          (throw-errno #:id 'check-dir-access/exn #:error 'EACCES))
                        (throw-errno #:id 'check-dir-access/exn #:error 'ENOTDIR))))
                
                (define/public (file-stat ftp-path)
                  (let ([stat (file-or-dir-stat ftp-path)])
                    (if (stat-file? stat)
                        stat
                        (throw-errno #:id 'file-stat #:error 'ENOENT)))))))))

(provide/define-member-key
 ftp-commands/access-control<%>_ftp-cmd/user
 ftp-commands/access-control<%>_ftp-cmd/pass
 ftp-commands/access-control<%>_ftp-cmd/rein
 ftp-commands/access-control<%>_ftp-cmd/cwd
 ftp-commands/access-control<%>_ftp-cmd/cdup
 ftp-commands/access-control<%>_ftp-cmd/quit)

(define-values (ftp-commands/access-control<%> ftp-commands/access-control%)
  (let-member-name ([use-ftp-command     ftp-session/command<%>_use-ftp-command]
                    [print*-response     ftp-session/responses<%>_print*-response]
                    [set-login           ftp-session/auth<%>_set-login]
                    [set-user-info       ftp-session/auth<%>_set-user-info]
                    [set-root-ftp-dir    ftp-session/path<%>_set-root-ftp-dir]
                    [set-current-ftp-dir ftp-session/path<%>_set-current-ftp-dir]
                    [write-log-msg       ftp-session/log<%>_write-log-msg]
                    [debug-error         ftp-session/log<%>_debug-error]
                    [ftp-cmd/user        ftp-commands/access-control<%>_ftp-cmd/user]
                    [ftp-cmd/pass        ftp-commands/access-control<%>_ftp-cmd/pass]
                    [ftp-cmd/rein        ftp-commands/access-control<%>_ftp-cmd/rein]
                    [ftp-cmd/cwd         ftp-commands/access-control<%>_ftp-cmd/cwd]
                    [ftp-cmd/cdup        ftp-commands/access-control<%>_ftp-cmd/cdup]
                    [ftp-cmd/quit        ftp-commands/access-control<%>_ftp-cmd/quit])
    (let ([<%> (interface ()
                 users-table
                 ftp-cmd/user
                 ftp-cmd/pass
                 ftp-cmd/rein
                 ftp-cmd/cwd
                 ftp-cmd/cdup
                 ftp-cmd/quit)])
      (values <%>
              (mixin
                  (ftp-session/responses<%>
                   ftp-session/log<%>
                   ftp-session/command<%>
                   ftp-session/path<%>
                   ftp-session/auth<%>
                   ftp-session/core<%>
                   ftp-session/fs<%>)
                (<%>)
                
                (inherit use-ftp-command
                         print*-response
                         login
                         set-login
                         set-user-info
                         user-allow?
                         root-ftp-dir
                         set-root-ftp-dir
                         current-ftp-dir
                         set-current-ftp-dir
                         complete-ftp-path
                         format-ftp-path
                         write-log-msg
                         debug-error
                         check-dir-access/exn
                         close-session)
                
                (super-new)
                
                (init :users-table
                      :passwd-sleep
                      :bad-auth-table
                      :login-fail-sleep
                      :max-login-attempts)
                
                (define *users-table*        :users-table)
                (define *bad-auth-table*     :bad-auth-table)
                (define *passwd-sleep*       :passwd-sleep)
                (define *login-fail-sleep*   :login-fail-sleep)
                (define *max-login-attempts* :max-login-attempts)
                
                (define/public-final (users-table)
                  *users-table*)
                
                (use-ftp-command 'USER #t (generic this% ftp-cmd/user) 'required "USER <SP> <username>")
                (define/public (ftp-cmd/user params)
                  (let ([name params])
                    (if (and (userinfo/login *users-table* name)
                             (ftp-user-anonymous? (userinfo/login *users-table* name)))
                        (print*-response 'ANONYMOUS-LOGIN)
                        (print*-response 'PASSW-REQUIRED name))
                    (set-login name)
                    (set-user-info #f)))
                
                (use-ftp-command 'PASS #t (generic this% ftp-cmd/pass) 'custom "PASS <SP> <password>")
                (define/public (ftp-cmd/pass params)
                  (sleep *passwd-sleep*)
                  (let ([correct
                         (if (string? (login))
                             (let ([pass params])
                               (cond
                                 ((not (userinfo/login *users-table* (login)))
                                  'login-incorrect)
                                 ((ftp-user-anonymous? (userinfo/login *users-table* (login)))
                                  'anonymous-logged-in)
                                 ((and (hash-ref *bad-auth-table* (login) #f)
                                       (>= (mcar (hash-ref *bad-auth-table* (login)))
                                           *max-login-attempts*)
                                       (<= (- (current-seconds) (mcdr (hash-ref *bad-auth-table* (login))))
                                           *login-fail-sleep*))
                                  (let ([pair (hash-ref *bad-auth-table* (login))])
                                    (set-mcar! pair (add1 (mcar pair)))
                                    (set-mcdr! pair (current-seconds)))
                                  'login-incorrect)
                                 ((not pass)
                                  'login-incorrect)
                                 ((check-user-pass (userinfo/login *users-table* (login)) pass)
                                  (when (hash-ref *bad-auth-table* (login) #f)
                                    (hash-remove! *bad-auth-table* (login)))
                                  'user-logged-in)
                                 (else
                                  (if (hash-ref *bad-auth-table* (login) #f)
                                      (let ([pair (hash-ref *bad-auth-table* (login))])
                                        (set-mcar! pair (add1 (mcar pair)))
                                        (set-mcdr! pair (current-seconds)))
                                      (hash-set! *bad-auth-table* (login) (mcons 1 (current-seconds))))
                                  'passw-incorrect)))
                             'login-incorrect)])
                    (set-user-info #f)
                    (case correct
                      [(user-logged-in)
                       (set-root-ftp-dir (ftp-user-root-dir (userinfo/login *users-table* (login))))
                       (if (directory-exists? (root-ftp-dir))
                           (begin
                             (set-user-info (userinfo/login *users-table* (login)))
                             (write-log-msg "User logged in.")
                             (print*-response 'USER-LOGGED (login)))
                           (begin
                             (write-log-msg "Users-config file incorrect.")
                             (print*-response 'LOGIN-INCORRECT)))]
                      [(anonymous-logged-in)
                       (set-root-ftp-dir (ftp-user-root-dir (userinfo/login *users-table* (login))))
                       (if (directory-exists? (root-ftp-dir))
                           (begin
                             (set-user-info (userinfo/login *users-table* (login)))
                             (write-log-msg "Anonymous user logged in.")
                             (print*-response 'ANONYMOUS-LOGGED))
                           (begin
                             (write-log-msg "Users-config file incorrect.")
                             (print*-response 'LOGIN-INCORRECT)))]
                      [(login-incorrect)
                       (write-log-msg "Login incorrect.")
                       (print*-response 'LOGIN-INCORRECT)]
                      [(passw-incorrect)
                       (write-log-msg "Password incorrect.")
                       (print*-response 'LOGIN-INCORRECT)])))
                
                (use-ftp-command 'REIN #t (generic this% ftp-cmd/rein) 'none "REIN")
                (define/public (ftp-cmd/rein)
                  (set-login #f)
                  (set-user-info #f)
                  (print*-response 'SERVICE-READY))
                
                (use-ftp-command 'CWD #f (generic this% ftp-cmd/cwd) 'required "CWD <SP> <pathname>")
                (use-ftp-command 'XCWD #f (generic this% ftp-cmd/cwd) 'required "XCWD <SP> <pathname>")
                (define/public (ftp-cmd/cwd params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES)
                                                   (print*-response 'PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                   (print*-response 'DIR-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "CWD")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/access-control<%>_ftp-cmd/cwd
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/access-control<%>_ftp-cmd/cwd
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (complete-ftp-path params)])
                      (check-dir-access/exn ftp-path execute-mode #:extra-perm (user-allow? 'enter-directory))
                      (set-current-ftp-dir (format-ftp-path ftp-path))
                      (print*-response 'CMD-SUCCESSFUL 250 "CWD"))))
                
                (use-ftp-command 'CDUP #f (generic this% ftp-cmd/cdup) 'none "CDUP")
                (use-ftp-command 'XCUP #f (generic this% ftp-cmd/cdup) 'none "XCUP")
                (define/public (ftp-cmd/cdup)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES)
                                                   (print*-response 'PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                   (print*-response 'DIR-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "CDUP")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/access-control<%>_ftp-cmd/cdup
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/access-control<%>_ftp-cmd/cdup
                                                        "top@any ~a"
                                                        e))])
                    (let ([path (format-ftp-path (current-ftp-dir) 'up)])
                      (check-dir-access/exn path execute-mode #:extra-perm (user-allow? 'enter-directory))
                      (set-current-ftp-dir path)
                      (print*-response 'CMD-SUCCESSFUL 250 "CDUP"))))
                
                (use-ftp-command 'QUIT #t (generic this% ftp-cmd/quit) 'none "QUIT")
                (define/public (ftp-cmd/quit)
                  (print*-response 'QUIT)
                  (close-session)))))))

(struct passive-ip&ports (ip from to))

(define (make-passive-ip&ports ip from to)
  (and (from . < . to)
       (passive-ip&ports ip from to)))

(provide/define-member-key
 ftp-commands/transfer-parameters<%>_ftp-cmd/type
 ftp-commands/transfer-parameters<%>_ftp-cmd/mode
 ftp-commands/transfer-parameters<%>_ftp-cmd/stru
 ftp-commands/transfer-parameters<%>_ftp-cmd/port
 ftp-commands/transfer-parameters<%>_ftp-cmd/pasv)

(define-values (ftp-commands/transfer-parameters<%> ftp-commands/transfer-parameters%)
  (let-member-name ([use-ftp-command         ftp-session/command<%>_use-ftp-command]
                    [print*-response         ftp-session/responses<%>_print*-response]
                    [set-representation-type ftp-session/DTP<%>_set-representation-type]
                    [make-DTP                ftp-session/DTP<%>_make-DTP]
                    [ftp-cmd/type            ftp-commands/transfer-parameters<%>_ftp-cmd/type]
                    [ftp-cmd/mode            ftp-commands/transfer-parameters<%>_ftp-cmd/mode]
                    [ftp-cmd/stru            ftp-commands/transfer-parameters<%>_ftp-cmd/stru]
                    [ftp-cmd/port            ftp-commands/transfer-parameters<%>_ftp-cmd/port]
                    [ftp-cmd/pasv            ftp-commands/transfer-parameters<%>_ftp-cmd/pasv])
    (let ([<%> (interface ()
                 allow-foreign-address
                 pasv-random-gen
                 pasv-ip&ports
                 ftp-cmd/type
                 ftp-cmd/mode
                 ftp-cmd/stru
                 ftp-cmd/port
                 ftp-cmd/pasv)])
      (values <%>
              (mixin
                  (ftp-session/responses<%>
                   ftp-session/command<%>
                   ftp-session/client<%>
                   ftp-session/server<%>
                   ftp-session/DTP<%>)
                (<%>)
                
                (inherit use-ftp-command
                         print*-response
                         set-representation-type
                         make-DTP
                         client-host
                         server-host)
                
                (super-new)
                
                (init :allow-foreign-address
                      :pasv-random-gen
                      :pasv-ip&ports)
                
                (define *allow-foreign-address* :allow-foreign-address)
                (define *pasv-random-gen*       :pasv-random-gen)
                (define *pasv-ip&ports*         :pasv-ip&ports)
                
                (define/public-final (allow-foreign-address)
                  *allow-foreign-address*)
                
                (define/public-final (pasv-random-gen)
                  *pasv-random-gen*)
                
                (define/public-final (pasv-ip&ports)
                  *pasv-ip&ports*)
                
                (use-ftp-command 'TYPE #f (generic this% ftp-cmd/type) 'required "TYPE <SP> <type-code>")
                (define/public (ftp-cmd/type params)
                  (case (string->symbol (string-upcase (car (regexp-split #rx" +" params))))
                    ((A)
                     (set-representation-type 'ASCII)
                     (print*-response 'SET-CMD 200 "TYPE" 'ASCII))
                    ((I)
                     (set-representation-type 'Image)
                     (print*-response 'SET-CMD 200 "TYPE" 'Image))
                    ((E L)
                     (print*-response 'MISSING-PARAMS))
                    (else
                     (print*-response 'UNSUPTYPE))))
                
                (use-ftp-command 'MODE #f (generic this% ftp-cmd/mode) 'required "MODE <SP> <mode-code>")
                (define/public (ftp-cmd/mode params)
                  (case (string->symbol (string-upcase params))
                    ((S)
                     (print*-response 'SET-CMD 200 "MODE" 'Stream))
                    ((B C)
                     (print*-response 'MISSING-PARAMS))
                    (else
                     (print*-response 'UNKNOWN-TYPE "MODE"))))
                
                (use-ftp-command 'STRU #f (generic this% ftp-cmd/stru) 'required "STRU <SP> <structure-code>")
                (define/public (ftp-cmd/stru params)
                  (case (string->symbol (string-upcase params))
                    ((F)
                     (print*-response 'SET-CMD 200 "FILE STRUCTURE" 'File))
                    ((R P)
                     (print*-response 'MISSING-PARAMS))
                    (else
                     (print*-response 'UNKNOWN-TYPE "FILE STRUCTURE"))))
                
                (use-ftp-command 'PORT #f (generic this% ftp-cmd/port) 'required "PORT <SP> <host-port>")
                (define/public (ftp-cmd/port params)
                  (if (regexp-match #rx"^[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+$" params)
                      (let* ([l (for/list ([n (regexp-split #rx"," params)])
                                  (cadr (regexp-match #rx"0*(.+)" n)))]
                             [host (string-join (take l 4) ".")]
                             [port (+ (* (string->number (fifth l)) 256)
                                      (string->number (sixth l)))])
                        (if (and (IPv4? host) (port-number? port))
                            (if (and (port . > . 1024)
                                     (or *allow-foreign-address*
                                         (private-IPv4? host)
                                         (string=? host (client-host))))
                                (begin
                                  (make-DTP #f (if (private-IPv4? host) (client-host) host) port)
                                  (print*-response 'CMD-SUCCESSFUL 200 "PORT"))
                                (print*-response 'FORBIDDEN-ADDR-PORT))
                            (print*-response 'SYNTAX-ERROR "")))
                      (print*-response 'SYNTAX-ERROR "")))
                
                (use-ftp-command 'PASV #f (generic this% ftp-cmd/pasv) 'none "PASV")
                (define/public (ftp-cmd/pasv)
                  (let*-values ([(psv-port) (+ passive-ports-from
                                               (random (- passive-ports-to passive-ports-from -1)
                                                       *pasv-random-gen*))]
                                [(h1 h2 h3 h4) (apply values (regexp-split #rx"\\." passive-ip))]
                                [(p1 p2) (quotient/remainder psv-port 256)])
                    (make-DTP #t (server-host) psv-port)
                    (print*-response 'PASV h1 h2 h3 h4 p1 p2)))
                
                (define-syntax (passive-ip stx)
                  #'(passive-ip&ports-ip *pasv-ip&ports*))
                
                (define-syntax (passive-ports-from stx)
                  #'(passive-ip&ports-from *pasv-ip&ports*))
                
                (define-syntax (passive-ports-to stx)
                  #'(passive-ip&ports-to *pasv-ip&ports*)))))))

(define-syntax-rule (when/str test exprs ...)
  (if test
      (begin exprs ...)
      ""))

(provide/define-member-key
 ftp-commands/service<%>_ftp-cmd/noop
 ftp-commands/service<%>_ftp-cmd/syst
 ftp-commands/service<%>_ftp-cmd/stat
 ftp-commands/service<%>_ftp-cmd/help
 ftp-commands/service<%>_ftp-cmd/site
 ftp-commands/service<%>_ftp-cmd/abor
 ftp-commands/service<%>_ftp-cmd/rest
 ftp-commands/service<%>_ftp-cmd/allo
 ftp-commands/service<%>_ftp-cmd/appe
 ftp-commands/service<%>_ftp-cmd/stor
 ftp-commands/service<%>_ftp-cmd/stou
 ftp-commands/service<%>_ftp-cmd/retr
 ftp-commands/service<%>_ftp-cmd/dele
 ftp-commands/service<%>_ftp-cmd/rnfr
 ftp-commands/service<%>_ftp-cmd/rnto
 ftp-commands/service<%>_ftp-cmd/pwd
 ftp-commands/service<%>_ftp-cmd/mkd
 ftp-commands/service<%>_ftp-cmd/rmd
 ftp-commands/service<%>_ftp-cmd/list
 ftp-commands/service<%>_ftp-cmd/nlst)

(define-values (ftp-commands/service<%> ftp-commands/service%)
  (let-member-name ([use-ftp-command     ftp-session/command<%>_use-ftp-command]
                    [print*-response     ftp-session/responses<%>_print*-response]
                    [print-crlf          ftp-session/print<%>_print-crlf]
                    [printf-crlf         ftp-session/print<%>_printf-crlf]
                    [write-log-msg       ftp-session/log<%>_write-log-msg]
                    [debug-log-msg       ftp-session/log<%>_debug-log-msg]
                    [debug-error         ftp-session/log<%>_debug-error]
                    [set-restart-marker  ftp-session/DTP<%>_set-restart-marker]
                    [current-DTP-info    ftp-session/DTP<%>_current-DTP-info]
                    [kill-current-DTP    ftp-session/DTP<%>_kill-current-DTP]
                    [ftp-data-transfer   ftp-session/DTP<%>_ftp-data-transfer]
                    [ftp-store-file      ftp-session/DTP<%>_ftp-store-file]
                    [ftp-cmd/noop        ftp-commands/service<%>_ftp-cmd/noop]
                    [ftp-cmd/syst        ftp-commands/service<%>_ftp-cmd/syst]
                    [ftp-cmd/stat        ftp-commands/service<%>_ftp-cmd/stat]
                    [ftp-cmd/help        ftp-commands/service<%>_ftp-cmd/help]
                    [ftp-cmd/site        ftp-commands/service<%>_ftp-cmd/site]
                    [ftp-cmd/abor        ftp-commands/service<%>_ftp-cmd/abor]
                    [ftp-cmd/rest        ftp-commands/service<%>_ftp-cmd/rest]
                    [ftp-cmd/allo        ftp-commands/service<%>_ftp-cmd/allo]
                    [ftp-cmd/appe        ftp-commands/service<%>_ftp-cmd/appe]
                    [ftp-cmd/stor        ftp-commands/service<%>_ftp-cmd/stor]
                    [ftp-cmd/stou        ftp-commands/service<%>_ftp-cmd/stou]
                    [ftp-cmd/retr        ftp-commands/service<%>_ftp-cmd/retr]
                    [ftp-cmd/dele        ftp-commands/service<%>_ftp-cmd/dele]
                    [ftp-cmd/rnfr        ftp-commands/service<%>_ftp-cmd/rnfr]
                    [ftp-cmd/rnto        ftp-commands/service<%>_ftp-cmd/rnto]
                    [ftp-cmd/pwd         ftp-commands/service<%>_ftp-cmd/pwd]
                    [ftp-cmd/mkd         ftp-commands/service<%>_ftp-cmd/mkd]
                    [ftp-cmd/rmd         ftp-commands/service<%>_ftp-cmd/rmd]
                    [ftp-cmd/list        ftp-commands/service<%>_ftp-cmd/list]
                    [ftp-cmd/nlst        ftp-commands/service<%>_ftp-cmd/nlst])
    (let ([<%> (interface ()
                 ftp-cmd/noop
                 ftp-cmd/syst
                 ftp-cmd/stat
                 ftp-cmd/help
                 ftp-cmd/site
                 ftp-cmd/abor
                 ftp-cmd/rest
                 ftp-cmd/allo
                 ftp-cmd/appe
                 ftp-cmd/stor
                 ftp-cmd/stou
                 ftp-cmd/retr
                 ftp-cmd/dele
                 ftp-cmd/rnfr
                 ftp-cmd/rnto
                 ftp-cmd/pwd
                 ftp-cmd/mkd
                 ftp-cmd/rmd
                 ftp-cmd/list
                 ftp-cmd/nlst)])
      (values <%>
              (mixin
                  (ftp-session/encoding<%>
                   ftp-session/responses<%>
                   ftp-session/print<%>
                   ftp-session/command<%>
                   ftp-session/path<%>
                   ftp-session/auth<%>
                   ftp-session/log<%>
                   ftp-session/client<%>
                   ftp-session/DTP<%>
                   ftp-session/fs<%>)
                (<%>)
                
                (inherit use-ftp-command
                         used-ftp-commands
                         ftp-command
                         print-crlf
                         printf-crlf
                         print*-response
                         string->bytes/encoding*
                         unhide-name?
                         login
                         client-host
                         user-info
                         user-allow?
                         write-log-msg
                         debug-log-msg
                         debug-error
                         set-restart-marker
                         representation-type
                         transfer-mode
                         file-structure
                         kill-current-DTP
                         current-DTP-info
                         root-ftp-dir?
                         complete-ftp-path
                         ftp-path->path
                         format-ftp-path
                         ftp-path->simplify-path
                         root-ftp-dir
                         current-ftp-dir
                         hide-ids?
                         text-user&group-names?
                         hide-dotobjects?
                         legal-ftp-obj?
                         file-or-dir-stat
                         file-or-dir-chmod
                         file-or-dir-chown
                         file-unlink
                         make-dir
                         remove-dir
                         rename-file-or-dir
                         check-file-access/exn
                         check-dir-access/exn
                         ftp-store-file
                         ftp-data-transfer)
                
                (super-new)
                
                (define *rename-path* #f)
                
                (use-ftp-command 'NOOP #t (generic this% ftp-cmd/noop) 'none "NOOP")
                (define/public (ftp-cmd/noop)
                  (print*-response 'CMD-SUCCESSFUL 200 "NOOP"))
                
                (use-ftp-command 'SYST #t (generic this% ftp-cmd/syst) 'none "SYST")
                (define/public (ftp-cmd/syst)
                  (print*-response 'SYSTEM))
                
                (use-ftp-command 'STAT #f (generic this% ftp-cmd/stat) 'custom "STAT [<SP> <pathname>]")
                (define/public (ftp-cmd/stat params)
                  (if params
                      (begin
                        (print*-response 'STATUS-LIST params)
                        (dir-list params #f #t)
                        (print*-response 'END 213))
                      (begin
                        (print*-response 'STATUS-INFO-HEAD)
                        (print*-response 'STATUS-INFO-HOST (client-host))
                        (print*-response 'STATUS-INFO-LOGIN (login))
                        (print*-response 'STATUS-INFO-PARAM
                                         (representation-type)
                                         (file-structure)
                                         (transfer-mode))
                        ; Print status of the operation in progress
                        (let ([dtp (current-DTP-info)])
                          (when dtp
                            (print*-response 'STATUS-INFO-PROC
                                             (format "[~a-DTP;~a:~a] ~a"
                                                     (ftp-DTP/info-type dtp)
                                                     (host&port-host (ftp-DTP/info-host&port dtp))
                                                     (host&port-port (ftp-DTP/info-host&port dtp))
                                                     (ftp-DTP/info-infomsg dtp)))))
                        (print*-response 'END 211))))
                
                (use-ftp-command 'HELP #t (generic this% ftp-cmd/help) 'custom "HELP [<SP> <string>]")
                (define/public (ftp-cmd/help params)
                  (if params
                      (with-handlers ([any/c (λ (e) (print*-response 'UNKNOWN-CMD params))])
                        (print*-response 'HELP (ftp-command-info-help 
                                                (ftp-command (string->symbol (string-upcase params))))))
                      (begin
                        (print*-response 'HELP-LISTING)
                        (for [(cmd (sort (map symbol->string (used-ftp-commands)) string<?))]
                          (printf-crlf " ~a" cmd))
                        (print*-response 'END 214))))
                
                (use-ftp-command 'SITE #f (generic this% ftp-cmd/site) 'required "SITE <SP> <string>")
                (define/public (ftp-cmd/site params)
                  (local
                    [(define (chmod mode ftp-path)
                       (with-handlers ([exn:posix? (λ (e)
                                                     (case (errno->symbol (exn:posix-errno e))
                                                       [(EPERM EACCES) 
                                                        (print*-response 'PERM-DENIED)]
                                                       [(ENOENT ENOTDIR ENAMETOOLONG) 
                                                        (print*-response 'FILE-DIR-NOT-FOUND)]
                                                       [else 
                                                        (print*-response 'CMD-FAILED 550 "SITE CHMOD")]))]
                                       [exn? (λ (e)
                                               (debug-error 'ftp-commands/service<%>_ftp-cmd/site
                                                            "chmod@exn ~a"
                                                            (exn-message e)))]
                                       [any/c (λ (e)
                                                (debug-error 'ftp-commands/service<%>_ftp-cmd/site
                                                             "chmod@any ~a"
                                                             e))])
                         (check-dir-access/exn (format-ftp-path ftp-path 'up) 
                                               wx-mode
                                               #:extra-perm (and (not (ftp-user-anonymous? (user-info)))
                                                                 (ftp-user-site-enable? (user-info))
                                                                 (legal-ftp-obj? ftp-path)))
                         (if (root-ftp-dir? ftp-path)
                             (print*-response 'FILE-DIR-NOT-FOUND)
                             (begin
                               (file-or-dir-chmod ftp-path mode)
                               (write-log-msg "Change the permissions of a ~a" 
                                              (ftp-path->simplify-path ftp-path))
                               (print*-response 'CMD-SUCCESSFUL 200 "SITE CHMOD")))))
                     
                     (define (chown owner ftp-path)
                       (let ([uid (if (regexp-match? #rx"^[0-9]+$" owner)
                                      (let ([uid (string->number owner)])
                                        (and (>= uid 0)
                                             (< uid #xffffffff)
                                             uid))
                                      (login->uid owner))])
                         (if uid
                             (with-handlers ([exn:posix? (λ (e)
                                                           (case (errno->symbol (exn:posix-errno e))
                                                             [(EPERM EACCES) 
                                                              (print*-response 'PERM-DENIED)]
                                                             [(ENOENT ENOTDIR ENAMETOOLONG) 
                                                              (print*-response 'FILE-DIR-NOT-FOUND)]
                                                             [else 
                                                              (print*-response 'CMD-FAILED 550 "SITE CHOWN")]))]
                                             [exn? (λ (e)
                                                     (debug-error 'ftp-commands/service<%>_ftp-cmd/site
                                                                  "chown@exn ~a"
                                                                  (exn-message e)))]
                                             [any/c (λ (e)
                                                      (debug-error 'ftp-commands/service<%>_ftp-cmd/site
                                                                   "chown@any ~a"
                                                                   e))])
                               (check-dir-access/exn (format-ftp-path ftp-path 'up) 
                                                     wx-mode
                                                     #:extra-perm (and (not (ftp-user-anonymous? (user-info)))
                                                                       (ftp-user-site-enable? (user-info))
                                                                       (legal-ftp-obj? ftp-path)))
                               (if (root-ftp-dir? ftp-path)
                                   (print*-response 'FILE-DIR-NOT-FOUND)
                                   (begin
                                     (file-or-dir-chown ftp-path uid #xffffffff)
                                     (write-log-msg "Change the owner of a ~a" 
                                                    (ftp-path->simplify-path ftp-path))
                                     (print*-response 'CMD-SUCCESSFUL 200 "SITE CHOWN"))))
                             (print*-response 'PERM-DENIED))))
                     
                     (define (chgrp group ftp-path)
                       (let ([gid (if (regexp-match? #rx"^[0-9]+$" group)
                                      (let ([gid (string->number group)])
                                        (and (>= gid 0)
                                             (< gid #xffffffff)
                                             gid))
                                      (gname->gid group))])
                         (if gid
                             (with-handlers ([exn:posix? (λ (e)
                                                           (case (errno->symbol (exn:posix-errno e))
                                                             [(EPERM EACCES) 
                                                              (print*-response 'PERM-DENIED)]
                                                             [(ENOENT ENOTDIR ENAMETOOLONG) 
                                                              (print*-response 'FILE-DIR-NOT-FOUND)]
                                                             [else 
                                                              (print*-response 'CMD-FAILED 550 "SITE CHGRP")]))]
                                             [exn? (λ (e)
                                                     (debug-error 'ftp-commands/service<%>_ftp-cmd/site
                                                                  "chgrp@exn ~a"
                                                                  (exn-message e)))]
                                             [any/c (λ (e)
                                                      (debug-error 'ftp-commands/service<%>_ftp-cmd/site
                                                                   "chgrp@any ~a"
                                                                   e))])
                               (check-dir-access/exn (format-ftp-path ftp-path 'up) 
                                                     wx-mode
                                                     #:extra-perm (and (not (ftp-user-anonymous? (user-info)))
                                                                       (ftp-user-site-enable? (user-info))
                                                                       (legal-ftp-obj? ftp-path)))
                               (if (root-ftp-dir? ftp-path)
                                   (print*-response 'FILE-DIR-NOT-FOUND)
                                   (begin
                                     (file-or-dir-chown ftp-path #xffffffff gid)
                                     (write-log-msg "Change the group of a ~a" 
                                                    (ftp-path->simplify-path ftp-path))
                                     (print*-response 'CMD-SUCCESSFUL 200 "SITE CHGRP"))))
                             (print*-response 'PERM-DENIED))))]
                    
                    (let ([cmd (string->symbol (string-upcase (car (regexp-match #rx"[^ ]+" params))))])
                      (case cmd
                        [(CHMOD)
                         (let ([args (regexp-match #rx"[A-z]+[ ]+([0-7]?[0-7][0-7][0-7])[ ]+(.+)" params)])
                           (if args
                               (chmod (string->number (cadr args) 8) (caddr args))
                               (print*-response 'SYNTAX-ERROR "CHMOD:")))]
                        [(CHOWN)
                         (let ([args (regexp-match #rx"[A-z]+[ ]+([^ ]+)[ ]+(.+)" params)])
                           (if args
                               (chown (cadr args) (caddr args))
                               (print*-response 'SYNTAX-ERROR "CHOWN:")))]
                        [(CHGRP)
                         (let ([args (regexp-match #rx"[A-z]+[ ]+([^ ]+)[ ]+(.+)" params)])
                           (if args
                               (chgrp (cadr args) (caddr args))
                               (print*-response 'SYNTAX-ERROR "CHGRP:")))]
                        (else
                         (print*-response 'MISSING-PARAMS))))))
                
                (use-ftp-command 'ABOR #f (generic this% ftp-cmd/abor) 'none "ABOR")
                (define/public (ftp-cmd/abor)
                  (kill-current-DTP)
                  (print*-response 'ABORT))
                
                (use-ftp-command 'REST #f (generic this% ftp-cmd/rest) 'required "REST <SP> <marker>")
                (define/public (ftp-cmd/rest params)
                  (if (regexp-match? #rx"^[0-9]+$" params)
                      (begin
                        (set-restart-marker (string->number params))
                        (print*-response 'RESTART))
                      (print*-response 'SYNTAX-ERROR "")))
                
                (use-ftp-command 'ALLO #f (generic this% ftp-cmd/allo) 'required "ALLO <SP> <decimal-integer>")
                (define/public (ftp-cmd/allo params)
                  (if (regexp-match? #rx"^[0-9]+$" params)
                      (print*-response 'CMD-SUCCESSFUL 200 "ALLO")
                      (print*-response 'SYNTAX-ERROR "")))
                
                (use-ftp-command 'APPE #f (generic this% ftp-cmd/appe) 'required "APPE <SP> <pathname>")
                (define/public (ftp-cmd/appe params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES)
                                                   (print*-response 'APPEND-FILE-PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                   (print*-response 'FILE-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "APPE")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/appe
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/appe
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (complete-ftp-path params)])
                      (check-file-access/exn ftp-path write-mode #:extra-perm (user-allow? 'append-file))
                      (ftp-store-file ftp-path 'append))))
                
                (use-ftp-command 'STOR #f (generic this% ftp-cmd/stor) 'required "STOR <SP> <pathname>")
                (define/public (ftp-cmd/stor params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES)
                                                   (print*-response 'STORE-FILE-PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                   (print*-response 'FILE-DIR-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "STOR")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/stor
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/stor
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (complete-ftp-path params)])
                      (check-dir-access/exn (format-ftp-path ftp-path 'up) wx-mode #:extra-perm (user-allow? 'create-file))
                      (ftp-store-file ftp-path 'truncate))))
                
                (use-ftp-command 'STOU #f (generic this% ftp-cmd/stou) 'none "STOU")
                (define/public (ftp-cmd/stou)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES)
                                                   (print*-response 'STORE-FILE-PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                   (print*-response 'FILE-DIR-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "STOU")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/stou
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/stou
                                                        "top@any ~a"
                                                        e))])
                    (check-dir-access/exn (current-ftp-dir) wx-mode #:extra-perm (user-allow? 'create-file))
                    (let loop ([file-path (ftp-path->path (gensym "noname"))])
                      (if (file-exists? file-path)
                          (loop (ftp-path->path (gensym "noname")))
                          (ftp-store-file file-path 'truncate)))))
                
                (use-ftp-command 'RETR #f (generic this% ftp-cmd/retr) 'required "RETR <SP> <pathname>")
                (define/public (ftp-cmd/retr params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES)
                                                   (print*-response 'PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                   (print*-response 'FILE-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "RETR")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/retr
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/retr
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (complete-ftp-path params)])
                      (check-file-access/exn ftp-path read-mode #:extra-perm (user-allow? 'read-file))
                      (ftp-data-transfer ftp-path #t))))
                
                (use-ftp-command 'DELE #f (generic this% ftp-cmd/dele) 'required "DELE <SP> <pathname>")
                (define/public (ftp-cmd/dele params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES) 
                                                   (print*-response 'DELFILE-PERM-DENIED)]
                                                  [(ENOENT ENOTDIR EISDIR EROFS ENAMETOOLONG) 
                                                   (print*-response 'FILE-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "DELE")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/dele
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/dele
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (complete-ftp-path params)])
                      (if (and (user-allow? 'delete-file)
                               (legal-ftp-obj? ftp-path))
                          (begin
                            (file-unlink ftp-path)
                            (write-log-msg "Delete a file ~a" (ftp-path->simplify-path ftp-path))
                            (print*-response 'CMD-SUCCESSFUL 250 "DELE"))
                          (print*-response 'DELFILE-PERM-DENIED)))))
                
                (use-ftp-command 'RNFR #f (generic this% ftp-cmd/rnfr) 'required "RNFR <SP> <pathname>")
                (define/public (ftp-cmd/rnfr params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (set! *rename-path* #f)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES) 
                                                   (print*-response 'PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG) 
                                                   (print*-response 'FILE-DIR-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "RNFR")]))]
                                  [exn:fail:filesystem? (λ (e)
                                                          (set! *rename-path* #f)
                                                          (print*-response 'FILE-DIR-NOT-FOUND))]
                                  [exn? (λ (e)
                                          (set! *rename-path* #f)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/rnfr
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (set! *rename-path* #f)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/rnfr
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (complete-ftp-path params)])
                      (check-dir-access/exn (format-ftp-path ftp-path 'up) wx-mode)
                      (if (root-ftp-dir? ftp-path)
                          (print*-response 'FILE-DIR-NOT-FOUND)
                          (let ([stat (file-or-dir-stat ftp-path)]
                                [rename/perm (λ (perm)
                                               (cond
                                                 [(user-allow? perm)
                                                  (set! *rename-path* ftp-path)
                                                  (print*-response 'RENAME-OK)]
                                                 [else
                                                  (set! *rename-path* #f)
                                                  (print*-response 'PERM-DENIED)]))])
                            (cond
                              [(stat-file? stat)
                               (rename/perm 'rename-file)]
                              [(stat-directory? stat)
                               (rename/perm 'rename-directory)]
                              [else
                               (set! *rename-path* #f)
                               (print*-response 'FILE-DIR-NOT-FOUND)]))))))
                
                (use-ftp-command 'RNTO #f (generic this% ftp-cmd/rnto) 'required "RNTO <SP> <pathname>")
                (define/public (ftp-cmd/rnto params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES)
                                                   (print*-response 'RENAME-PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG)
                                                   (print*-response 'FILE-DIR-NOT-FOUND)]
                                                  [(EISDIR ENOTEMPTY EBUSY EEXIST EINVAL EMLINK EFAULT)
                                                   (print*-response 'CANT-RENAME)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "RNTO")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/rnto
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/rnto
                                                        "top@any ~a"
                                                        e))])
                    (if *rename-path*
                        (let ([new-path (format-ftp-path (complete-ftp-path params))])
                          (if (access? (ftp-path->path new-path))
                              (print*-response 'CANT-RENAME-EXIST)
                              (begin
                                (check-dir-access/exn (path-only new-path) 
                                                      wx-mode 
                                                      #:extra-perm (legal-ftp-obj? new-path))
                                (rename-file-or-dir *rename-path* new-path)
                                (write-log-msg "Rename the file or directory from ~a to ~a"
                                               (ftp-path->simplify-path *rename-path*)
                                               (ftp-path->simplify-path new-path))
                                (print*-response 'CMD-SUCCESSFUL 250 "RNTO"))))
                        (print*-response 'CMD-BAD-SEQ)))
                  (set! *rename-path* #f))
                
                (use-ftp-command 'PWD #f (generic this% ftp-cmd/pwd) 'none "PWD")
                (use-ftp-command 'XPWD #f (generic this% ftp-cmd/pwd) 'none "XPWD")
                (define/public (ftp-cmd/pwd)
                  (print*-response 'CURRENT-DIR (path->string (current-ftp-dir))))
                
                (use-ftp-command 'MKD #f (generic this% ftp-cmd/mkd) 'required "MKD <SP> <pathname>")
                (define/public (ftp-cmd/mkd params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES) 
                                                   (print*-response 'CREATE-DIR-PERM-DENIED)]
                                                  [(EEXIST) 
                                                   (print*-response 'DIR-EXIST)]
                                                  [(ENOENT ENOTDIR) 
                                                   (print*-response 'FILE-DIR-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CANT-CREATE-DIR)]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/mkd
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/mkd
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (format-ftp-path (complete-ftp-path params))])
                      (if (and (user-allow? 'create-directory)
                               (legal-ftp-obj? ftp-path))
                          (begin
                            (make-dir ftp-path)
                            (write-log-msg "Make directory ~a" (ftp-path->path ftp-path))
                            (print*-response 'DIR-CREATED ftp-path))
                          (print*-response 'CREATE-DIR-PERM-DENIED)))))
                
                (use-ftp-command 'RMD #f (generic this% ftp-cmd/rmd) 'required "RMD <SP> <pathname>")
                (define/public (ftp-cmd/rmd params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES) 
                                                   (print*-response 'DELDIR-PERM-DENIED)]
                                                  [(ENOTEMPTY EEXIST) 
                                                   (print*-response 'DELDIR-NOT-EMPTY)]
                                                  [(ENOENT ENOTDIR) 
                                                   (print*-response 'DIR-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CANT-RMDIR)]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/service<%>_ftp-cmd/rmd
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/service<%>_ftp-cmd/rmd
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (format-ftp-path (complete-ftp-path params))])
                      (if (and (user-allow? 'delete-directory)
                               (legal-ftp-obj? ftp-path))
                          (if (root-ftp-dir? ftp-path)
                              (print*-response 'DIR-NOT-FOUND)
                              (begin
                                (remove-dir ftp-path)
                                (write-log-msg "Remove a directory ~a" (ftp-path->path ftp-path))
                                (print*-response 'CMD-SUCCESSFUL 250 "RMD")))
                          (print*-response 'DELDIR-PERM-DENIED)))))
                
                (define/private (dir-list params [short? #f][status #f])
                  (local
                    [(define (month->string m)
                       (case m
                         [(1) "Jan"][(2)  "Feb"][(3)  "Mar"][(4)  "Apr"]
                         [(5) "May"][(6)  "Jun"][(7)  "Jul"][(8)  "Aug"]
                         [(9) "Sep"][(10) "Oct"][(11) "Nov"][(12) "Dec"]))
                     
                     (define (date-time->string dte)
                       (string-append (month->string (date-month dte)) " " (number->string (date-day dte)) " "
                                      (if ((date-year (current-date)). = .(date-year dte))
                                          (format "~a~a:~a~a"
                                                  (when/str (< (date-hour dte) 10) "0")
                                                  (date-hour dte)
                                                  (when/str (< (date-minute dte) 10) "0")
                                                  (date-minute dte))
                                          (number->string (date-year dte)))))
                     
                     (define (read-stat ftp-path)
                       (let* ([full-path (ftp-path->path ftp-path)]
                              [stat (lstat full-path)]
                              [mode (statinfo-mode stat)]
                              [owner (cond
                                       [(ftp-user-hide-ids? (user-info))
                                        (if (string? (ftp-user-hide-ids? (user-info)))
                                            (ftp-user-hide-ids? (user-info))
                                            "ftp")]
                                       [(hide-ids?)
                                        (if (string? (hide-ids?)) (hide-ids?) "ftp")]
                                       [else
                                        (and (text-user&group-names?) (uid->uname (statinfo-uid stat)))])]
                              [group (cond
                                       [(ftp-user-hide-ids? (user-info))
                                        (if (string? (ftp-user-hide-ids? (user-info)))
                                            (ftp-user-hide-ids? (user-info))
                                            "ftp")]
                                       [(hide-ids?)
                                        (if (string? (hide-ids?)) (hide-ids?) "ftp")]
                                       [else
                                        (and (text-user&group-names?) (gid->gname (statinfo-gid stat)))])])
                         (string-append
                          (cond
                            [(bitwise-bit-set? mode 15)
                             (cond
                               [(bitwise-bit-set? mode 13) "l"]
                               [(bitwise-bit-set? mode 14) "s"]
                               [else "-"])]
                            [(bitwise-bit-set? mode 14)
                             (if (bitwise-bit-set? mode 13) "b" "d")]
                            [(bitwise-bit-set? mode 13) "c"]
                            [(bitwise-bit-set? mode 12) "p"]
                            [else "-"])
                          (string (if (bitwise-bit-set? mode 8) #\r #\-)
                                  (if (bitwise-bit-set? mode 7) #\w #\-)
                                  (if (bitwise-bit-set? mode 11)
                                      (if (bitwise-bit-set? mode 6) #\s #\S)
                                      (if (bitwise-bit-set? mode 6) #\x #\-)))
                          (string (if (bitwise-bit-set? mode 5) #\r #\-)
                                  (if (bitwise-bit-set? mode 4) #\w #\-)
                                  (if (bitwise-bit-set? mode 10)
                                      (if (bitwise-bit-set? mode 3) #\s #\S)
                                      (if (bitwise-bit-set? mode 3) #\x #\-)))
                          (string (if (bitwise-bit-set? mode 2) #\r #\-)
                                  (if (bitwise-bit-set? mode 1) #\w #\-)
                                  (if (bitwise-bit-set? mode 9)
                                      (if (bitwise-bit-set? mode 0) #\t #\T)
                                      (if (bitwise-bit-set? mode 0) #\x #\-)))
                          " "
                          (~a (statinfo-nlink stat) #:min-width 3 #:align 'right)
                          " "
                          (~a (or owner (statinfo-uid stat)) #:min-width 8 #:align 'right)
                          " "
                          (~a (or group (statinfo-gid stat)) #:min-width 8 #:align 'right)
                          " "
                          (~a (statinfo-size stat) #:min-width 14 #:align 'right)
                          " "
                          (date-time->string (seconds->date (statinfo-mtime stat))))))
                     
                     (define (dlst ftp-dir show-dotobjects?)
                       (with-handlers ([exn:posix? (λ (e)
                                                     (if status
                                                         (print-crlf "")
                                                         (case (errno->symbol (exn:posix-errno e))
                                                           [(EPERM EACCES)
                                                            (print*-response 'PERM-DENIED)]
                                                           [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                            (print*-response 'DIR-NOT-FOUND)]
                                                           [else 
                                                            (print*-response 'CMD-FAILED 550 (if short? "NLST" "LIST"))])))]
                                       [exn? (λ (e)
                                               (debug-error 'ftp-commands/service<%>_dir-list
                                                            "dlst@exn ~a"
                                                            (exn-message e)))]
                                       [any/c (λ (e)
                                                (debug-error 'ftp-commands/service<%>_dir-list
                                                             "dlst@any ~a"
                                                             e))])
                         (check-dir-access/exn ftp-dir rx-mode #:extra-perm (user-allow? 'listing-directory))
                         (let ([dirlist 
                                (string-append*
                                 (when/str show-dotobjects?
                                           (if short?
                                               ".\r\n"
                                               (string-append (read-stat ftp-dir) " .\r\n")))
                                 (when/str show-dotobjects?
                                           (if short?
                                               "..\r\n"
                                               (string-append (if (root-ftp-dir? ftp-dir)
                                                                  (read-stat ftp-dir)
                                                                  (read-stat (format-ftp-path ftp-dir 'up)))
                                                              " ..\r\n")))
                                 (for/list ([name (directory-list (ftp-path->path ftp-dir))])
                                   (let ([ftp-path (build-path ftp-dir name)])
                                     (when/str (and (unhide-name? name)
                                                    (when (regexp-match? #rx"^\\." name)
                                                      show-dotobjects?))
                                               (if short?
                                                   (format "~a\r\n" name)
                                                   (format "~a ~a\r\n" (read-stat ftp-path) name))))))])
                           (if status
                               (print-crlf dirlist)
                               (begin
                                 (debug-log-msg "Listing of the '~a'\n\n~a" ftp-dir dirlist)
                                 (ftp-data-transfer (case (representation-type)
                                                      ((ASCII) dirlist)
                                                      ((Image) (string->bytes/encoding* dirlist)))))))))]
                    
                    (let ([dir (if (and params (eq? (string-ref params 0) #\-))
                                   (let ([d (regexp-match #px"[^-\\w][^ \t-]+.*" params)])
                                     (and d (substring (car d) 1)))
                                   params)]
                          [show-dotobjects? (and (not (hide-dotobjects?))
                                                 params 
                                                 (regexp-match? #rx"^-[A-z]*[Aa][A-z]*" params))])
                      (if dir
                          (dlst (complete-ftp-path dir) show-dotobjects?)
                          (dlst (current-ftp-dir) show-dotobjects?)))))
                
                (use-ftp-command 'LIST #f (generic this% ftp-cmd/list) 'custom "LIST [<SP> <pathname>]")
                (define/public (ftp-cmd/list params)
                  (dir-list params))
                
                (use-ftp-command 'NLST #f (generic this% ftp-cmd/nlst) 'custom "NLST [<SP> <pathname>]")
                (define/public (ftp-cmd/nlst params)
                  (dir-list params #t)))))))

(define ftp-session/rfc959%
  (ftp-commands/service%
   (ftp-commands/transfer-parameters%
    (ftp-commands/access-control%
     (ftp-session/fs% dummy-ftp-session%)))))
