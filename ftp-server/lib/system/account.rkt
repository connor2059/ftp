#|
RktFTPd Account Library
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

(require ffi/unsafe
         (rename-in racket/contract/base [-> -->])
         (file "ffi-core.rkt"))

(provide uid-or-gid?
         _uid_t
         _gid_t
         root-uid
         root-gid
         (contract-out [struct usrinfo ([name string?]
                                        [passwd string?]
                                        [uid uid-or-gid?]
                                        [gid uid-or-gid?]
                                        [home string?])]
                       [get-user-info (string? . --> . usrinfo?)]
                       [get-shadow-password (string? . --> . [or/c string? not])]
                       [uid->uname (uid-or-gid? . --> . [or/c string? not])]
                       [login->uid (string? . --> . [or/c uid-or-gid? not])]
                       [gid->gname (uid-or-gid? . --> . [or/c string? not])]
                       [gname->gid (string? . --> . [or/c uid-or-gid? not])]
                       [grpmember? ([or/c uid-or-gid? string?] [or/c uid-or-gid? string?]. --> . boolean?)]))

(define-lib libc "libc.so.6")

(typedef _uid_t _uint)
(typedef _gid_t _uint)

(define-cstruct _Passwd ([name   _string]
                         [passwd _string]
                         [uid    _uid_t]
                         [gid    _gid_t]
                         [gecos  _string]
                         [home   _string]
                         [shell  _string]))

(define-cstruct _Group ([name    _string]
                        [passwd  _string]
                        [gid     _gid_t]
                        [members _pointer]))

(define-cstruct _Spwd ([name   _string]
                       [passwd _string]
                       [lstchg _long]
                       [min    _long]
                       [max    _long]
                       [warn   _long]
                       [inact  _long]
                       [expire _long]
                       [flag   _ulong]))

(defun-libc getgrgid (_gid_t -> (_or-null _Group-pointer)))
(defun-libc getgrnam (_string -> (_or-null _Group-pointer)))

(defun-libc getpwuid (_uid_t -> (_or-null _Passwd-pointer)))
(defun-libc getpwnam (_string -> (_or-null _Passwd-pointer)))
(defun-libc getspnam (_string -> (_or-null _Spwd-pointer)))

(define (uid-or-gid? v)
  (and (fixnum? v)
       (not (negative? v))))

(defconst root-uid 0)
(defconst root-gid 0)

(struct usrinfo (name
                 passwd
                 uid 
                 gid 
                 home))

(define (get-user-info login)
  (let ([pwd (call/result getpwnam login)]
        [spwd (call/result getspnam login)])
    (usrinfo (Passwd-name pwd)
             (Spwd-passwd spwd)
             (Passwd-uid pwd)
             (Passwd-gid pwd)
             (Passwd-home pwd))))

(define (get-shadow-password login)
  (let ([spwd (getspnam login)])
    (and spwd (Spwd-passwd spwd))))

(define (uid->uname uid)
  (let ([pwd (getpwuid uid)])
    (and pwd
         (Passwd-name pwd))))

(define (login->uid login)
  (let ([pwd (getpwnam login)])
    (and pwd
         (Passwd-uid pwd))))

(define (gid->gname gid)
  (let ([grp (getgrgid gid)])
    (and grp
         (Group-name grp))))

(define (gname->gid gname)
  (let ([grp (getgrnam gname)])
    (and grp
         (Group-gid grp))))

(define (grpmember? gid-or-grpname uid-or-usrname)
  (let ([grp (if (uid-or-gid? gid-or-grpname)
                 (getgrgid gid-or-grpname)
                 (getgrnam gid-or-grpname))]
        [uname (if (uid-or-gid? uid-or-usrname)
                   (uid->uname uid-or-usrname)
                   uid-or-usrname)])
    (and grp uname
         (or (string=? (Group-name grp) uname)
             (let ([*mem (Group-members grp)])
               (let loop ([offs 0])
                 (and (ptr-ref *mem _string offs)
                      (or (string=? (ptr-ref *mem _string offs) uname)
                          (loop (add1 offs))))))))))
