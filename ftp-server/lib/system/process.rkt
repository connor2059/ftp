#|
RktFTPd Process Library
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
         (file "ffi-core.rkt")
         (only-in (file "account.rkt")
                  _uid_t
                  _gid_t
                  uid-or-gid?))

(provide getpid
         getppid
         getuid
         getgid
         geteuid
         getegid
         (contract-out [call/eid ([--> any] #:euid uid-or-gid? #:egid uid-or-gid? . --> . any)]))

(define-lib libc "libc.so.6")

(typedef _pid_t _int)

(defun-libc getpid (-> _pid_t))
(defun-libc getppid (-> _pid_t))

(defun-libc getuid (-> _uid_t))
(defun-libc getgid (-> _gid_t))
(defun-libc geteuid (-> _uid_t))
(defun-libc getegid (-> _gid_t))

(defun-libc setuid (_uid_t -> _int))
(defun-libc setgid (_gid_t -> _int))
(defun-libc seteuid (_uid_t -> _int))
(defun-libc setegid (_gid_t -> _int))
(defun-libc setfsuid (_uid_t -> _int))
(defun-libc setfsgid (_gid_t -> _int))

(define chids/sem (make-semaphore 1))

(define (call/eid fun #:euid [uid #f] #:egid [gid #f])
  (let ([euid (geteuid)]
        [egid (getegid)])
    (dynamic-wind
     (λ ()
       (with-handlers ([any/c (λ (e) (exit 1))])
         (semaphore-wait chids/sem)
         (when gid (call/0 setegid gid))
         (when uid (call/0 seteuid uid))))
     fun
     (λ ()
       (with-handlers ([any/c (λ (e) (exit 1))])
         (when uid (call/0 seteuid euid))
         (when gid (call/0 setegid egid))
         (semaphore-post chids/sem))))))
