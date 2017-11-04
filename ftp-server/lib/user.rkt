#|
RktFTPd User Library
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

(require (only-in (file "system/ffi-core.rkt")
                  exn:posix?)
         (file "system/process.rkt")
         (file "system/filesystem.rkt")
         (file "system/account.rkt")
         (file "system/crypt.rkt"))

(provide (except-out (all-defined-out)
                     (struct-out ftp-users)))

(struct ftp-user (login
                  passwd
                  real-user
                  anonymous? 
                  hide-ids?
                  hide-dotobjects?
                  mfmt-enable?
                  mff-enable?
                  site-enable?
                  uid
                  gid
                  permissions
                  root-dir))

(struct ftp-users (logins uids) #:mutable)

(define (make-users-table)
  (ftp-users (make-hash) (make-hash)))

(define (clear-users-info users)
  (set-ftp-users-logins! users (make-hash))
  (set-ftp-users-uids! users (make-hash)))

(define (userinfo/login users login)
  (hash-ref (ftp-users-logins users) login #f))

(define (ftp-useradd users
                     login
                     passwd
                     real-user
                     anonymous?
                     hide-ids?
                     hide-dotobjects?
                     mfmt-enable?
                     mff-enable?
                     site-enable?
                     [permissions (make-user-permissions/all #t)]
                     [root-dir "/"])
  (let* ([root-dir (and root-dir (string-trim root-dir))]
         [sysusr (with-handlers ([exn:posix?
                                  (λ (e)
                                    (error 'ftp-useradd "real-user '~a' not found." real-user))])
                   (get-user-info real-user))]
         [user (ftp-user login
                         passwd
                         real-user
                         anonymous?
                         hide-ids?
                         hide-dotobjects?
                         mfmt-enable?
                         mff-enable?
                         site-enable?
                         (usrinfo-uid sysusr) 
                         (usrinfo-gid sysusr)
                         permissions
                         (or root-dir (usrinfo-home sysusr)))])
    (hash-set! (ftp-users-logins users) login user)
    (hash-set! (ftp-users-uids users) (usrinfo-uid sysusr) user)
    (and root-dir
         (unless (directory-exists? root-dir)
           (call/eid (λ () (mkdir root-dir))
                     #:euid (usrinfo-uid sysusr)
                     #:egid (usrinfo-gid sysusr))))
    (void)))

(define (check-user-pass userstruct pass)
  (if (ftp-user-passwd userstruct)
      (let* ([passwd (ftp-user-passwd userstruct)]
             [salt (let ([l (regexp-split #rx"\\$" passwd)])
                     (format "$~a$~a" (second l) (third l)))]
             [pass (crypt-string pass salt)])
        (and pass (string=? passwd pass)))
      (with-handlers ([exn:posix? not])
        (let ([passwd (usrinfo-passwd (get-user-info (ftp-user-real-user userstruct)))])
          (and (not (string=? passwd "*"))
               (let* ([salt (let ([l (regexp-split #rx"\\$" passwd)])
                              (format "$~a$~a" (second l) (third l)))]
                      [pass (crypt-string pass salt)])
                 (and pass (string=? passwd pass))))))))

(struct user-permissions
  (create-file
   append-file
   read-file
   delete-file
   rename-file
   enter-directory
   listing-directory
   create-directory
   rename-directory
   delete-directory)
  #:mutable)

(define (make-user-permissions #:create-file [create-file #f]
                               #:append-file [append-file #f]
                               #:read-file [read-file #f]
                               #:delete-file [delete-file #f]
                               #:rename-file [rename-file #f]
                               #:enter-directory [enter-directory #f]
                               #:listing-directory [listing-directory #f]
                               #:create-directory [create-directory #f]
                               #:rename-directory [rename-directory #f]
                               #:delete-directory [delete-directory #f])
  (user-permissions create-file
                    append-file
                    read-file
                    delete-file
                    rename-file
                    enter-directory
                    listing-directory
                    create-directory
                    rename-directory
                    delete-directory))

(define (make-user-permissions/all allow?)
  (user-permissions allow? allow? allow? allow? allow? allow? allow? allow? allow? allow?))

(define (make-user-permissions/rules . rules)
  (let* ([permissions (make-user-permissions/all #f)]
         [set-perms (λ (allow? perms)
                      (for [(perm perms)]
                        (case perm
                          [(all)
                           (set! permissions (make-user-permissions/all allow?))]
                          [(create-file)
                           (set-user-permissions-create-file! permissions allow?)]
                          [(append-file)
                           (set-user-permissions-append-file! permissions allow?)]
                          [(read-file)
                           (set-user-permissions-read-file! permissions allow?)]
                          [(delete-file)
                           (set-user-permissions-delete-file! permissions allow?)]
                          [(rename-file)
                           (set-user-permissions-rename-file! permissions allow?)]
                          [(enter-directory)
                           (set-user-permissions-enter-directory! permissions allow?)]
                          [(listing-directory)
                           (set-user-permissions-listing-directory! permissions allow?)]
                          [(create-directory)
                           (set-user-permissions-create-directory! permissions allow?)]
                          [(rename-directory)
                           (set-user-permissions-rename-directory! permissions allow?)]
                          [(delete-directory)
                           (set-user-permissions-delete-directory! permissions allow?)]
                          [else
                           (error 'make-user-permissions/rules "syntax error.")])))])
    (for [(rule rules)]
      (match rule
        [`(allow ,(? symbol? perms) ..1)
         (set-perms #t perms)]
        [`(deny ,(? symbol? perms) ..1)
         (set-perms #f perms)]
        [else
         (error 'make-user-permissions/rules "syntax error.")]))
    permissions))
