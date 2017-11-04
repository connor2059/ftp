#|
RktFTPd Extra FTP Commands Library
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
                  errno->symbol)
         (file "system/filesystem.rkt")
         (file "system/account.rkt")
         (file "private/net.rkt")
         (file "private/syntax.rkt")
         (file "user.rkt")
         (file "ftp-session.rkt")
         (file "ftp-commands.rkt"))

(provide ftp-commands/IPv6<%>
         ftp-commands/IPv6%
         ftp-commands/ext<%>
         ftp-commands/ext%
         ftp-session%)

(define-syntax-rule (when/str test exprs ...)
  (if test
      (begin exprs ...)
      ""))

(provide/define-member-key
 ftp-commands/IPv6<%>_ftp-cmd/eprt
 ftp-commands/IPv6<%>_ftp-cmd/epsv)

(define-values (ftp-commands/IPv6<%> ftp-commands/IPv6%)
  (let-member-name ([use-ftp-command   ftp-session/command<%>_use-ftp-command]
                    [print*-response   ftp-session/responses<%>_print*-response]
                    [client-input-port ftp-session/client<%>_client-input-port]
                    [make-DTP          ftp-session/DTP<%>_make-DTP]
                    [ftp-cmd/pasv      ftp-commands/transfer-parameters<%>_ftp-cmd/pasv]
                    [ftp-cmd/eprt      ftp-commands/IPv6<%>_ftp-cmd/eprt]
                    [ftp-cmd/epsv      ftp-commands/IPv6<%>_ftp-cmd/epsv])
    (let ([<%> (interface ()
                 current-protocol
                 ftp-cmd/eprt
                 ftp-cmd/epsv)])
      (values <%>
              (mixin
                  (ftp-session/responses<%>
                   ftp-session/command<%>
                   ftp-session/client<%>
                   ftp-session/server<%>
                   ftp-session/DTP<%>
                   ftp-commands/transfer-parameters<%>)
                (<%>)
                
                (inherit use-ftp-command
                         print*-response
                         client-host
                         client-input-port
                         server-host
                         make-DTP
                         allow-foreign-address
                         pasv-random-gen
                         pasv-ip&ports)
                
                (super-new)
                
                (define *current-protocol* (if (IPv4? (server-host)) '|1| '|2|))
                
                (define/public-final (current-protocol)
                  *current-protocol*)
                
                (use-ftp-command 'EPRT #f (generic this% ftp-cmd/eprt) 'required "EPRT <SP> <d> <address-family> <d> <ip-addr> <d> <port> <d>")
                (define/public (ftp-cmd/eprt params)
                  (with-handlers ([any/c (λ (e) (print*-response 'SYNTAX-ERROR "EPRT:"))])
                    (let*-values ([(t1 net-prt ip tcp-port t2) (apply values (regexp-split #rx"\\|" params))]
                                  [(prt port) (values (string->number net-prt) (string->number tcp-port))])
                      (unless (and (string=? t1 t2 "")
                                   (or (and (= prt 1) (IPv4? ip))
                                       (and (= prt 2) (IPv6? ip)))
                                   (port-number? port))
                        (raise 'syntax))
                      (if (and (port . > . 1024)
                               (or (allow-foreign-address)
                                   (and (= prt 1)
                                        (private-IPv4? ip))
                                   (string=? ip (client-host))))
                          (begin
                            (make-DTP #f 
                                      (if (and (= prt 1) 
                                               (private-IPv4? ip))
                                          (client-host) 
                                          ip)
                                      port)
                            (print*-response 'CMD-SUCCESSFUL 200 "EPRT"))
                          (print*-response 'FORBIDDEN-ADDR-PORT)))))
                
                (use-ftp-command 'EPSV #f (generic this% ftp-cmd/epsv) 'custom "EPSV [<SP> (<address-family> | ALL)]")
                (define/public (ftp-cmd/epsv params)
                  (local [(define (set-psv)
                            (let ([psv-port (+ (passive-ip&ports-from (pasv-ip&ports))
                                               (random (- (passive-ip&ports-to (pasv-ip&ports))
                                                          (passive-ip&ports-from (pasv-ip&ports))
                                                          -1)
                                                       (pasv-random-gen)))])
                              (make-DTP #t (server-host) psv-port)
                              (print*-response 'EPSV psv-port)))
                          (define (epsv-1)
                            (if (eq? *current-protocol* '|1|)
                                (set-psv)
                                (print*-response 'BAD-PROTOCOL)))
                          (define (epsv-2)
                            (if (eq? *current-protocol* '|2|)
                                (set-psv)
                                (print*-response 'BAD-PROTOCOL)))
                          (define (epsv)
                            (if (eq? *current-protocol* '|1|) (epsv-1) (epsv-2)))]
                    (if params
                        (with-handlers ([any/c (λ (e) (print*-response 'SYNTAX-ERROR "EPSV:"))])
                          (case (string->symbol (string-upcase (car (regexp-match #rx"^1|2|[aA][lL][lL]$" params))))
                            [(|1|) (epsv-1)]
                            [(|2|) (epsv-2)]
                            [(ALL) (epsv)]))
                        (epsv))))
                
                (use-ftp-command 'PASV #f (generic this% ftp-cmd/pasv) 'none "PASV")
                (define/override (ftp-cmd/pasv)
                  (if (eq? (current-protocol) '|1|)
                      (super ftp-cmd/pasv)
                      (print*-response 'BAD-PROTOCOL))))))))

(provide/define-member-key
 ftp-commands/ext<%>_ftp-cmd/feat
 ftp-commands/ext<%>_ftp-cmd/clnt
 ftp-commands/ext<%>_ftp-cmd/prot
 ftp-commands/ext<%>_ftp-cmd/pbsz
 ftp-commands/ext<%>_ftp-cmd/lang
 ftp-commands/ext<%>_ftp-cmd/opts
 ftp-commands/ext<%>_ftp-cmd/mlsd
 ftp-commands/ext<%>_ftp-cmd/mlst
 ftp-commands/ext<%>_ftp-cmd/size
 ftp-commands/ext<%>_ftp-cmd/mdtm
 ftp-commands/ext<%>_ftp-cmd/mfmt
 ftp-commands/ext<%>_ftp-cmd/mff)

(define-values (ftp-commands/ext<%> ftp-commands/ext%)
  (let-member-name ([use-ftp-command      ftp-session/command<%>_use-ftp-command]
                    [set-locale-encoding  ftp-session/encoding<%>_set-locale-encoding]
                    [set-current-language ftp-session/responses<%>_set-current-language]
                    [print-crlf           ftp-session/print<%>_print-crlf]
                    [printf-crlf          ftp-session/print<%>_printf-crlf]
                    [print*-response      ftp-session/responses<%>_print*-response]
                    [write-log-msg        ftp-session/log<%>_write-log-msg]
                    [debug-log-msg        ftp-session/log<%>_debug-log-msg]
                    [debug-error          ftp-session/log<%>_debug-error]
                    [ftp-data-transfer    ftp-session/DTP<%>_ftp-data-transfer]
                    [ftp-cmd/feat         ftp-commands/ext<%>_ftp-cmd/feat]
                    [ftp-cmd/clnt         ftp-commands/ext<%>_ftp-cmd/clnt]
                    [ftp-cmd/prot         ftp-commands/ext<%>_ftp-cmd/prot]
                    [ftp-cmd/pbsz         ftp-commands/ext<%>_ftp-cmd/pbsz]
                    [ftp-cmd/lang         ftp-commands/ext<%>_ftp-cmd/lang]
                    [ftp-cmd/opts         ftp-commands/ext<%>_ftp-cmd/opts]
                    [ftp-cmd/mlsd         ftp-commands/ext<%>_ftp-cmd/mlsd]
                    [ftp-cmd/mlst         ftp-commands/ext<%>_ftp-cmd/mlst]
                    [ftp-cmd/size         ftp-commands/ext<%>_ftp-cmd/size]
                    [ftp-cmd/mdtm         ftp-commands/ext<%>_ftp-cmd/mdtm]
                    [ftp-cmd/mfmt         ftp-commands/ext<%>_ftp-cmd/mfmt]
                    [ftp-cmd/mff          ftp-commands/ext<%>_ftp-cmd/mff])
    (let ([<%> (interface ()
                 ftp-cmd/feat
                 ftp-cmd/clnt
                 ftp-cmd/prot
                 ftp-cmd/pbsz
                 ftp-cmd/lang
                 ftp-cmd/opts
                 ftp-cmd/mlsd
                 ftp-cmd/mlst
                 ftp-cmd/size
                 ftp-cmd/mdtm
                 ftp-cmd/mfmt
                 ftp-cmd/mff)])
      (values <%>
              (mixin
                  (ftp-session/encoding<%>
                   ftp-session/responses<%>
                   ftp-session/auth<%>
                   ftp-session/responses<%>
                   ftp-session/print<%>
                   ftp-session/command<%>
                   ftp-session/path<%>
                   ftp-session/log<%>
                   ftp-session/DTP<%>
                   ftp-session/fs<%>)
                (<%>)
                
                (inherit use-ftp-command
                         default-locale-encoding
                         locale-encoding
                         set-locale-encoding
                         set-current-language
                         ftp-command
                         current-language
                         used-languages
                         print-crlf
                         printf-crlf
                         print*-response
                         string->bytes/encoding*
                         write-log-msg
                         debug-log-msg
                         debug-error
                         user-info
                         user-allow?
                         root-ftp-dir?
                         format-ftp-path
                         ftp-path->simplify-path
                         complete-ftp-path
                         ftp-path->path
                         root-ftp-dir
                         current-ftp-dir
                         file-or-dir-utime
                         file-or-dir-chmod
                         file-or-dir-chown
                         check-dir-access/exn
                         file-stat
                         unhide-name?
                         legal-ftp-obj?
                         hide-ids?
                         text-user&group-names?
                         hide-dotobjects?
                         representation-type
                         ftp-data-transfer)
                
                (super-new)
                
                (struct mlst-facts 
                  (type? size? modify? perm? unique? charset?
                         unix-mode? unix-owner? unix-group?)
                  #:mutable)
                
                (define *mlst-facts* (mlst-facts #t #t #t #t #t #t #t #t #t))
                
                (use-ftp-command 'FEAT #t (generic this% ftp-cmd/feat) 'none "FEAT")
                (define/public (ftp-cmd/feat)
                  (print*-response 'FEAT-LIST)
                  (and (ftp-command 'CLNT)
                       (print-crlf " CLNT"))
                  (and (ftp-command 'LANG)
                       (print-crlf (string-append
                                    " LANG "
                                    (string-join (for/list ([l (used-languages)])
                                                   (string-append (symbol->string l)
                                                                  (when/str (eq? l (current-language)) "*")))
                                                 ";"))))
                  (and (ftp-command 'EPRT)
                       (print-crlf " EPRT"))
                  (and (ftp-command 'EPSV)
                       (print-crlf " EPSV"))
                  (print-crlf " UTF8")
                  (and (ftp-command 'REST)
                       (print-crlf " REST STREAM"))
                  (and (ftp-command 'MLST)
                       (printf-crlf " MLST Type~a;Size~a;Modify~a;Perm~a;Unique~a;Charset~a;UNIX.mode~a;UNIX.owner~a;UNIX.group~a;"
                                    (when/str (mlst-facts-type? *mlst-facts*) "*")
                                    (when/str (mlst-facts-size? *mlst-facts*) "*")
                                    (when/str (mlst-facts-modify? *mlst-facts*) "*")
                                    (when/str (mlst-facts-perm? *mlst-facts*) "*")
                                    (when/str (mlst-facts-unique? *mlst-facts*) "*")
                                    (when/str (mlst-facts-charset? *mlst-facts*) "*")
                                    (when/str (mlst-facts-unix-mode? *mlst-facts*) "*")
                                    (when/str (mlst-facts-unix-owner? *mlst-facts*) "*")
                                    (when/str (mlst-facts-unix-group? *mlst-facts*) "*")))
                  (and (ftp-command 'MLSD)
                       (print-crlf " MLSD"))
                  (and (ftp-command 'SIZE)
                       (print-crlf " SIZE"))
                  (and (ftp-command 'MDTM)
                       (print-crlf " MDTM"))
                  (and (ftp-command 'MFMT)
                       (print-crlf " MFMT"))
                  (and (ftp-command 'MFF)
                       (print-crlf " MFF Modify;UNIX.mode;UNIX.owner;UNIX.group;"))
                  (print-crlf " TVFS")
                  (print*-response 'END 211))
                
                (use-ftp-command 'CLNT #t (generic this% ftp-cmd/clnt) 'required "CLNT <SP> <client-name>")
                (define/public (ftp-cmd/clnt params)
                  (print*-response 'CLNT))
                ;; Warning: this is only emulation of PROT.
                (use-ftp-command 'PROT #f (generic this% ftp-cmd/prot) 'required "PROT <SP> <code>")
                (define/public (ftp-cmd/prot params)
                  (print-crlf "200 Protection level set to P"))
                ;; Warning: this is only emulation of PBSZ.
                (use-ftp-command 'PBSZ #f (generic this% ftp-cmd/pbsz) 'required "PBSZ <SP> <num>")
                (define/public (ftp-cmd/pbsz params)
                  (print-crlf "200 PBSZ=0"))
                
                (use-ftp-command 'LANG #t (generic this% ftp-cmd/lang) 'required "LANG <SP> <lang-tag>")
                (define/public (ftp-cmd/lang params)
                  (let ([lang (string->symbol (string-upcase params))])
                    (if (memq lang (used-languages))
                        (begin
                          (set-current-language lang)
                          (print*-response 'SET-CMD 200 "LANG" lang))
                        (print*-response 'MISSING-PARAMS))))
                
                (use-ftp-command 'OPTS #f (generic this% ftp-cmd/opts) 'required "OPTS <SP> <command-name> [<SP> <command-options>]")
                (define/public (ftp-cmd/opts params)
                  (let ([cmd (string->symbol (string-upcase (car (regexp-match #rx"[^ ]+" params))))])
                    (case cmd
                      ((UTF8)
                       (let ([matches (regexp-match #rx"^(?i:UTF8)[ ]+((?i:ON)|(?i:OFF))$" params)])
                         (if matches
                             (case (string->symbol (string-upcase (cadr matches)))
                               ((ON)
                                (set-locale-encoding "UTF-8")
                                (print*-response 'UTF8-ON))
                               ((OFF)
                                (set-locale-encoding (default-locale-encoding))
                                (print*-response 'UTF8-OFF))
                               (else
                                (print*-response 'SYNTAX-ERROR "UTF8:")))
                             (print*-response 'SYNTAX-ERROR "UTF8:"))))
                      ((MLST)
                       (let ([matches (regexp-match #rx"^(?i:MLST)[ ]+([A-z;.]+)$" params)])
                         (if matches
                             (let ([mlst (map string->symbol (string-split (string-upcase (cadr matches)) ";"))])
                               (if (andmap (λ (mode) (memq mode '(TYPE SIZE MODIFY PERM UNIQUE CHARSET 
                                                                       UNIX.MODE UNIX.OWNER UNIX.GROUP)))
                                           mlst)
                                   (begin
                                     (set! *mlst-facts* (mlst-facts #f #f #f #f #f #f #f #f #f))
                                     (for [(mode mlst)]
                                       (case mode
                                         [(TYPE) (set-mlst-facts-type?! *mlst-facts* #t)]
                                         [(SIZE) (set-mlst-facts-size?! *mlst-facts* #t)]
                                         [(MODIFY) (set-mlst-facts-modify?! *mlst-facts* #t)]
                                         [(PERM) (set-mlst-facts-perm?! *mlst-facts* #t)]
                                         [(UNIQUE) (set-mlst-facts-unique?! *mlst-facts* #t)]
                                         [(CHARSET) (set-mlst-facts-charset?! *mlst-facts* #t)]
                                         [(UNIX.MODE) (set-mlst-facts-unix-mode?! *mlst-facts* #t)]
                                         [(UNIX.OWNER) (set-mlst-facts-unix-owner?! *mlst-facts* #t)]
                                         [(UNIX.GROUP) (set-mlst-facts-unix-group?! *mlst-facts* #t)]))
                                     (print*-response 'MLST-ON))
                                   (print*-response 'SYNTAX-ERROR "MLST:")))
                             (print*-response 'SYNTAX-ERROR "MLST:"))))
                      (else
                       (print*-response 'SYNTAX-ERROR "")))))
                
                (use-ftp-command 'MLST #f (generic this% ftp-cmd/mlst) 'custom "MLST [<SP> <pathname>]")
                (define/public (ftp-cmd/mlst params)
                  (local [(define (mlst ftp-path)
                            (with-handlers ([exn:posix? (λ (e)
                                                          (case (errno->symbol (exn:posix-errno e))
                                                            [(EPERM EACCES)
                                                             (print*-response 'PERM-DENIED)]
                                                            [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                             (print*-response 'FILE-DIR-NOT-FOUND)]
                                                            [else 
                                                             (print*-response 'CMD-FAILED 550 "MLST")]))]
                                            [exn:fail:filesystem? (λ (e)
                                                                    (print*-response 'FILE-DIR-NOT-FOUND))]
                                            [exn? (λ (e)
                                                    (debug-error 'ftp-commands/ext<%>_ftp-cmd/mlst
                                                                 "mlst@exn ~a"
                                                                 (exn-message e)))]
                                            [any/c (λ (e)
                                                     (debug-error 'ftp-commands/ext<%>_ftp-cmd/mlst
                                                                  "mlst@any ~a"
                                                                  e))])
                              (check-dir-access/exn (format-ftp-path ftp-path 'up)
                                                    rx-mode
                                                    #:extra-perm (user-allow? 'listing-directory))
                              (if (root-ftp-dir? ftp-path)
                                  (print*-response 'FILE-DIR-NOT-FOUND)
                                  (if (legal-ftp-obj? ftp-path)
                                      (begin
                                        (print*-response 'MLST-LISTING)
                                        (printf-crlf " ~a" (mlst-info ftp-path))
                                        (print*-response 'END 250))
                                      (print*-response 'PERM-DENIED)))))]
                    (if params
                        (mlst (complete-ftp-path params))
                        (mlst (current-ftp-dir)))))
                
                (define/private (seconds->mdtm-time-format seconds)
                  (let ([dte (seconds->date seconds #f)])
                    (format "~a~a~a~a~a~a~a~a~a~a~a"
                            (date-year dte)
                            (when/str (< (date-month dte) 10) "0")
                            (date-month dte)
                            (when/str (< (date-day dte) 10) "0")
                            (date-day dte)
                            (when/str (< (date-hour dte) 10) "0")
                            (date-hour dte)
                            (when/str (< (date-minute dte) 10) "0")
                            (date-minute dte)
                            (when/str (< (date-second dte) 10) "0")
                            (date-second dte))))
                
                (define/private (mlst-info ftp-path [full-path? #t][special-path #f])
                  (let* ([ftp-path (format-ftp-path ftp-path)]
                         [path (ftp-path->path ftp-path)]
                         [parent-path (and (file-name-from-path ftp-path)
                                           (path-only path))]
                         [parent-stat (and parent-path (stat parent-path))]
                         [stat (lstat path)]
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
                                   (and (text-user&group-names?) (gid->gname (statinfo-gid stat)))])]
                         [file? (bitwise-bit-set? mode 15)]
                         [uid (ftp-user-uid (user-info))]
                         [gid (ftp-user-gid (user-info))])
                    (string-append (when/str (mlst-facts-type? *mlst-facts*)
                                             (format "Type=~a;"
                                                     (cond
                                                       [(bitwise-bit-set? mode 15)
                                                        (cond
                                                          [(bitwise-bit-set? mode 13) "OS.unix=slink"]
                                                          [(bitwise-bit-set? mode 14) "OS.unix=socket"]
                                                          [else "file"])]
                                                       [(bitwise-bit-set? mode 14)
                                                        (if (bitwise-bit-set? mode 13)
                                                            "OS.unix=blkdev"
                                                            (case special-path
                                                              [(same) "cdir"]
                                                              [(up) "pdir"]
                                                              [else "dir"]))]
                                                       [(bitwise-bit-set? mode 13) "OS.unix=chrdev"]
                                                       [(bitwise-bit-set? mode 12) "OS.unix=pipe"]
                                                       [else "file"])))
                                   (when/str (and file? (mlst-facts-size? *mlst-facts*))
                                             (format "Size=~a;" (statinfo-size stat)))
                                   (when/str (mlst-facts-modify? *mlst-facts*)
                                             (format "Modify=~a;" (seconds->mdtm-time-format (statinfo-mtime stat))))
                                   (when/str (mlst-facts-perm? *mlst-facts*)
                                             (format "Perm=~a;"
                                                     (if (grpmember? root-gid uid)
                                                         (if file? "rawfd" "elcmfd")
                                                         (let ([i (cond
                                                                    ((= (statinfo-uid stat) uid) 8)
                                                                    ((= (statinfo-gid stat) gid) 5)
                                                                    (else 2))]
                                                               [p (and parent-stat
                                                                       (cond
                                                                         ((= (statinfo-uid parent-stat) uid) 8)
                                                                         ((= (statinfo-gid parent-stat) gid) 5)
                                                                         (else 2)))])
                                                           (string-append
                                                            (when/str (and (not file?)
                                                                           (user-allow? 'enter-directory)
                                                                           (bitwise-bit-set? mode (- i 2)))
                                                                      "e")
                                                            (if file?
                                                                (when/str (and (user-allow? 'read-file)
                                                                               (bitwise-bit-set? mode i))
                                                                          "r")
                                                                (when/str (and (user-allow? 'listing-directory)
                                                                               (bitwise-bit-set? mode i))
                                                                          "l"))
                                                            (if file?
                                                                (when/str (and (user-allow? 'append-file)
                                                                               (bitwise-bit-set? mode (sub1 i)))
                                                                          "a")
                                                                (when/str (and (user-allow? 'create-file)
                                                                               (bitwise-bit-set? mode (sub1 i)))
                                                                          "c"))
                                                            (when/str (and (not file?)
                                                                           (user-allow? 'create-directory)
                                                                           (bitwise-bit-set? mode (sub1 i)))
                                                                      "m")
                                                            (when/str (and parent-stat
                                                                           (bitwise-bit-set? (statinfo-mode parent-stat) (sub1 p)))
                                                                      (string-append
                                                                       (when/str (and file? (user-allow? 'create-file))
                                                                                 "w")
                                                                       (when/str (user-allow? (if file?
                                                                                                  'rename-file
                                                                                                  'rename-directory))
                                                                                 "f")
                                                                       (when/str (user-allow? (if file?
                                                                                                  'delete-file
                                                                                                  'delete-directory))
                                                                                 "d"))))))))
                                   (when/str (mlst-facts-unique? *mlst-facts*)
                                             (format "Unique=~x;" (statinfo-unique stat)))
                                   (when/str (mlst-facts-charset? *mlst-facts*)
                                             (format "Charset=~a;" (locale-encoding)))
                                   (when/str (mlst-facts-unix-mode? *mlst-facts*)
                                             (format "UNIX.mode=~a;"
                                                     (let ([perm (number->string (bitwise-and (statinfo-mode stat) #o7777) 8)])
                                                       (string-append (make-string (- 4 (string-length perm)) #\0) perm))))
                                   (when/str (mlst-facts-unix-owner? *mlst-facts*)
                                             (format "UNIX.owner=~a;" (or owner (statinfo-uid stat))))
                                   (when/str (mlst-facts-unix-group? *mlst-facts*)
                                             (format "UNIX.group=~a;" (or group (statinfo-gid stat))))
                                   (format " ~a" (if special-path
                                                     (build-path special-path)
                                                     (if full-path? ftp-path (or (file-name-from-path ftp-path) ftp-path)))))))
                
                (use-ftp-command 'MLSD #f (generic this% ftp-cmd/mlsd) 'custom "MLSD [<SP> <pathname>]")
                (define/public (ftp-cmd/mlsd params)
                  (local [(define (mlsd ftp-path)
                            (let ([show-dotobjects? (not (hide-dotobjects?))])
                              (with-handlers ([exn:posix? (λ (e)
                                                            (case (errno->symbol (exn:posix-errno e))
                                                              [(EPERM EACCES)
                                                               (print*-response 'PERM-DENIED)]
                                                              [(ENOENT ENOTDIR ENAMETOOLONG ELOOP) 
                                                               (print*-response 'DIR-NOT-FOUND)]
                                                              [else
                                                               (print*-response 'CMD-FAILED 550 "MLSD")]))]
                                              [exn? (λ (e)
                                                      (debug-error 'ftp-commands/ext<%>_ftp-cmd/mlsd
                                                                   "mlsd@exn ~a"
                                                                   (exn-message e)))]
                                              [any/c (λ (e)
                                                       (debug-error 'ftp-commands/ext<%>_ftp-cmd/mlsd
                                                                    "mlsd@any ~a"
                                                                    e))])
                                (check-dir-access/exn ftp-path rx-mode #:extra-perm (user-allow? 'listing-directory))
                                (let ([dirlist
                                       (string-append*
                                        (when/str show-dotobjects?
                                                  (string-append (mlst-info ftp-path #f 'same) "\r\n"))
                                        (when/str show-dotobjects?
                                                  (string-append (if (root-ftp-dir? ftp-path)
                                                                     (mlst-info ftp-path #f 'up)
                                                                     (mlst-info (build-path ftp-path 'up) #f 'up))
                                                                 "\r\n"))
                                        (for/list ([name (directory-list (ftp-path->path ftp-path))])
                                          (let ([ftp-path (build-path ftp-path name)])
                                            (when/str (and (unhide-name? name)
                                                           (when (regexp-match? #rx"^\\." name)
                                                             show-dotobjects?))
                                                      (string-append (mlst-info ftp-path #f) "\r\n")))))])
                                  (debug-log-msg "Listing of the '~a'\n\n~a" ftp-path dirlist)
                                  (ftp-data-transfer (case (representation-type)
                                                       ((ASCII) dirlist)
                                                       ((Image) (string->bytes/encoding* dirlist))))))))]
                    (if params
                        (mlsd (complete-ftp-path params))
                        (mlsd (current-ftp-dir)))))
                
                (use-ftp-command 'SIZE #f (generic this% ftp-cmd/size) 'required "SIZE <SP> <pathname>")
                (define/public (ftp-cmd/size params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES) 
                                                   (print*-response 'PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG) 
                                                   (print*-response 'FILE-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "SIZE")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/ext<%>_ftp-cmd/size
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/ext<%>_ftp-cmd/size
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (complete-ftp-path params)])
                      (if (and (user-allow? 'listing-directory)
                               (legal-ftp-obj? ftp-path))
                          (printf-crlf "213 ~a" (statinfo-size (file-stat ftp-path)))
                          (print*-response 'PERM-DENIED)))))
                
                (use-ftp-command 'MDTM #f (generic this% ftp-cmd/mdtm) 'required "MDTM <SP> <pathname>")
                (define/public (ftp-cmd/mdtm params)
                  (with-handlers ([exn:posix? (λ (e)
                                                (case (errno->symbol (exn:posix-errno e))
                                                  [(EPERM EACCES) 
                                                   (print*-response 'PERM-DENIED)]
                                                  [(ENOENT ENOTDIR ENAMETOOLONG) 
                                                   (print*-response 'FILE-NOT-FOUND)]
                                                  [else 
                                                   (print*-response 'CMD-FAILED 550 "MDTM")]))]
                                  [exn? (λ (e)
                                          (debug-error 'ftp-commands/ext<%>_ftp-cmd/mdtm
                                                       "top@exn ~a"
                                                       (exn-message e)))]
                                  [any/c (λ (e)
                                           (debug-error 'ftp-commands/ext<%>_ftp-cmd/mdtm
                                                        "top@any ~a"
                                                        e))])
                    (let ([ftp-path (complete-ftp-path params)])
                      (if (and (user-allow? 'listing-directory)
                               (legal-ftp-obj? ftp-path))
                          (printf-crlf "213 ~a" (seconds->mdtm-time-format (statinfo-mtime (file-stat ftp-path))))
                          (print*-response 'PERM-DENIED)))))
                
                (define/private (mdtm-time-format->seconds mdtm)
                  (and (= (string-length mdtm) 14)
                       (regexp-match? #rx"^[0-9]+$" mdtm)
                       (let ([year (string->number (substring mdtm 0 4))]
                             [month (string->number (substring mdtm 4 6))]
                             [day (string->number (substring mdtm 6 8))]
                             [hour (string->number (substring mdtm 8 10))]
                             [minute (string->number (substring mdtm 10 12))]
                             [second (string->number (substring mdtm 12 14))])
                         (and (>= year 1970) (<= year 2037)
                              (>= month 1) (<= month 12)
                              (>= day 1) (<= day 31)
                              (when (= year 1970) (>= hour 3))
                              (<= hour 23)
                              (<= minute 59)
                              (<= second 59)
                              (find-seconds second minute hour day month year #f)))))
                
                (use-ftp-command 'MFMT #f (generic this% ftp-cmd/mfmt) 'required "MFMT <SP> time <SP> <pathname>")
                (define/public (ftp-cmd/mfmt params)
                  (let ([args (regexp-match #rx"^([0-9]+)[ ]+(.+)" params)])
                    (if args
                        (let ([modtime (mdtm-time-format->seconds (cadr args))]
                              [ftp-path (format-ftp-path (complete-ftp-path (caddr args)))])
                          (if modtime
                              (with-handlers ([exn:posix? (λ (e)
                                                            (case (errno->symbol (exn:posix-errno e))
                                                              [(EPERM EACCES) 
                                                               (print*-response 'PERM-DENIED)]
                                                              [(ENOENT ENOTDIR ENAMETOOLONG) 
                                                               (print*-response 'FILE-DIR-NOT-FOUND)]
                                                              [else 
                                                               (print*-response 'CANT-MFMT)]))]
                                              [exn:fail:filesystem? (λ (e) 
                                                                      (print*-response 'FILE-DIR-NOT-FOUND))]
                                              [exn? (λ (e)
                                                      (debug-error 'ftp-commands/ext<%>_ftp-cmd/mfmt
                                                                   "top@exn ~a"
                                                                   (exn-message e)))]
                                              [any/c (λ (e)
                                                       (debug-error 'ftp-commands/ext<%>_ftp-cmd/mfmt
                                                                    "top@any ~a"
                                                                    e))])
                                (check-dir-access/exn (format-ftp-path ftp-path 'up) 
                                                      wx-mode
                                                      #:extra-perm (and (not (ftp-user-anonymous? (user-info)))
                                                                        (ftp-user-mfmt-enable? (user-info))
                                                                        (legal-ftp-obj? ftp-path)))
                                (if (root-ftp-dir? ftp-path)
                                    (print*-response 'FILE-DIR-NOT-FOUND)
                                    (begin
                                      (file-or-dir-utime ftp-path modtime modtime)
                                      (write-log-msg "Modify a last modification time of ~s in ~a"
                                                     (ftp-path->path ftp-path) (cadr args))
                                      (print*-response 'MFMT-OK (cadr args) ftp-path))))
                              (print*-response 'SYNTAX-ERROR "")))
                        (print*-response 'SYNTAX-ERROR ""))))
                
                (use-ftp-command 'MFF #f (generic this% ftp-cmd/mff) 'required "MFF <SP> (<mff-fact> = <value> ;)+ <SP> <pathname>")
                (define/public (ftp-cmd/mff params)
                  (local [(define (change-mtime mt ftp-path)
                            (let ([modtime (mdtm-time-format->seconds mt)])
                              (unless modtime
                                (raise (print*-response 'SYNTAX-ERROR "MODIFY:")))
                              (file-or-dir-utime ftp-path modtime modtime)
                              (write-log-msg "Modify a last modification time of ~s in ~a"
                                             (ftp-path->simplify-path ftp-path) mt)))
                          
                          (define (change-mode mode ftp-path)
                            (file-or-dir-chmod ftp-path mode)
                            (write-log-msg "Change the permissions of a ~a" 
                                           (ftp-path->simplify-path ftp-path)))
                          
                          (define (change-owner owner ftp-path)
                            (let ([uid (if (regexp-match? #rx"^[0-9]+$" owner)
                                           (let ([uid (string->number owner)])
                                             (and (>= uid 0)
                                                  (< uid #xffffffff)
                                                  uid))
                                           (login->uid owner))])
                              (unless uid
                                (raise (print*-response 'PERM-DENIED)))
                              (file-or-dir-chown ftp-path uid #xffffffff)
                              (write-log-msg "Change the owner of a ~a" 
                                             (ftp-path->simplify-path ftp-path))))
                          
                          (define (change-group group ftp-path)
                            (let ([gid (if (regexp-match? #rx"^[0-9]+$" group)
                                           (let ([gid (string->number group)])
                                             (and (>= gid 0)
                                                  (< gid #xffffffff)
                                                  gid))
                                           (gname->gid group))])
                              (unless gid
                                (raise (print*-response 'PERM-DENIED)))
                              (file-or-dir-chown ftp-path #xffffffff gid)
                              (write-log-msg "Change the group of a ~a" 
                                             (ftp-path->simplify-path ftp-path))))]
                    (let ([modify #f]
                          [unix-mode #f]
                          [unix-owner #f]
                          [unix-group #f]
                          [args (regexp-match #rx"^([A-z\\.]+=[^ \t]+;)+[ ]+(.+)" params)])
                      (if args
                          (with-handlers ([exn:posix? (λ (e)
                                                        (case (errno->symbol (exn:posix-errno e))
                                                          [(EPERM EACCES) 
                                                           (print*-response 'PERM-DENIED)]
                                                          [(ENOENT ENOTDIR ENAMETOOLONG) 
                                                           (print*-response 'FILE-DIR-NOT-FOUND)]
                                                          [else 
                                                           (print*-response 'CMD-FAILED 550 "MFF")]))]
                                          [exn:fail:filesystem? (λ (e) 
                                                                  (print*-response 'FILE-DIR-NOT-FOUND))]
                                          [exn? (λ (e)
                                                  (debug-error 'ftp-commands/ext<%>_ftp-cmd/mff
                                                               "top@exn ~a"
                                                               (exn-message e)))]
                                          [any/c (λ (e)
                                                   (debug-error 'ftp-commands/ext<%>_ftp-cmd/mff
                                                                "top@any ~a"
                                                                e))])
                            (let ([facts (string-split (cadr args) ";")]
                                  [ftp-path (complete-ftp-path (caddr args))])
                              (for [(f facts)]
                                (let ([l (string-split f "=")])
                                  (case (string->symbol (string-upcase (car l)))
                                    [(MODIFY)
                                     (if (and (not modify) (regexp-match? #rx"^[0-9]+$" (cadr l)))
                                         (set! modify (cadr l))
                                         (raise (print*-response 'SYNTAX-ERROR "MODIFY:")))]
                                    [(UNIX.MODE) 
                                     (if (and (not unix-mode) (regexp-match? #rx"^[0-7]?[0-7][0-7][0-7]$" (cadr l)))
                                         (set! unix-mode (cadr l))
                                         (raise (print*-response 'SYNTAX-ERROR "UNIX.MODE:")))]
                                    [(UNIX.OWNER) 
                                     (if (and (not unix-owner) (regexp-match? #rx"^[^ ]+$" (cadr l)))
                                         (set! unix-owner (cadr l))
                                         (raise (print*-response 'SYNTAX-ERROR "UNIX.OWNER:")))]
                                    [(UNIX.GROUP) 
                                     (if (and (not unix-group) (regexp-match? #rx"^[^ ]+$" (cadr l)))
                                         (set! unix-group (cadr l))
                                         (raise (print*-response 'SYNTAX-ERROR "UNIX.GROUP:")))]
                                    [else 
                                     (raise (print*-response 'SYNTAX-ERROR ""))])))
                              (check-dir-access/exn (format-ftp-path ftp-path 'up) 
                                                    wx-mode
                                                    #:extra-perm (and (not (ftp-user-anonymous? (user-info)))
                                                                      (ftp-user-mff-enable? (user-info))
                                                                      (legal-ftp-obj? ftp-path)))
                              (if (root-ftp-dir? ftp-path)
                                  (print*-response 'FILE-DIR-NOT-FOUND)
                                  (begin
                                    (when modify (change-mtime modify ftp-path))
                                    (when unix-mode (change-mode (string->number unix-mode 8) ftp-path))
                                    (when unix-owner (change-owner unix-owner ftp-path))
                                    (when unix-group (change-group unix-group ftp-path))
                                    (print*-response 'MFF-OK
                                                     (string-append (when/str modify (format "Modify=~a;" modify))
                                                                    (when/str unix-mode (format "UNIX.mode=~a;" unix-mode))
                                                                    (when/str unix-owner (format "UNIX.owner=~a;" unix-owner))
                                                                    (when/str unix-group (format "UNIX.group=~a;" unix-group)))
                                                     (format-ftp-path ftp-path))))))
                          (print*-response 'SYNTAX-ERROR ""))))))))))

(define ftp-session%
  (ftp-commands/ext% (ftp-commands/IPv6% ftp-session/rfc959%)))
