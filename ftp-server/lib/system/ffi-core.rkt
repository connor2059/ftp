#|
RktFTPd Core FFI Library
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
         ffi/cvector
         racket/splicing
         (rename-in racket/contract/base [-> -->])
         (for-syntax racket/syntax
                     racket/base))

(provide (except-out (all-defined-out)
                     throw-errno
                     errno->symbol
                     symbol->errno)
         (contract-out [throw-errno (#:id symbol? #:error [or/c exact-integer? symbol?]. --> . any)]
                       [errno->symbol (exact-integer? . --> . [or/c symbol? exact-integer?])]
                       [symbol->errno (symbol? . --> . exact-integer?)])
         (for-syntax word-size))

(define-syntax (define-lib stx)
  (syntax-case stx ()
    [(_ lib path)
     (with-syntax ([deflib (format-id stx "defun-~a" #'lib)])
       #'(begin
           (define lib (ffi-lib path))
           (define-syntax-rule (deflib fun type)
             (define-c fun lib (_fun #:save-errno 'posix . type)))))]))

(define-syntax-rule (defconst name value)
  (define-syntax (name so) #'value))

(define-syntax-rule (typedef newtype type)
  (defconst newtype type))

(define-syntax (?if stx)
  (syntax-case stx ()
    [(_ test then else)
     #'(splicing-let-syntax ([noname (lambda (stx)
                                       (syntax-case stx ()
                                         [(_ a b) (if test #'a #'b)]))])
         (noname then else))]))

(define-syntax-rule (unsupported msg)
  (raise (exn:fail:unsupported msg (current-continuation-marks))))

(define-syntax-rule (not-implemented)
  (unsupported "Not yet implemented."))

(struct exn:posix exn (id errno) #:transparent)

(define (throw-errno #:id [id 'unspecified] #:error [error (saved-errno)])
  (let ([errno (cond
                 [(exact-positive-integer? error) error]
                 [(symbol? error) (symbol->errno error)]
                 [else -1])])
    (raise (exn:posix (format "~a: errno=~a" id errno)
                      (current-continuation-marks)
                      id errno))))

(define-syntax-rule (call/0 fun args ...)
  (unless (zero? (fun args ...))
    (throw-errno #:id 'fun)))

(define-syntax-rule (call/result fun args ...)
  (or (fun args ...)
      (throw-errno #:id 'fun)))

(define-for-syntax word-size (system-type 'word))

(define word-size (system-type 'word))

(define (make-carray type count)
  (ptr-ref (cvector-ptr (make-cvector type count))
           (_array type count)))

(define (errno->symbol errno)
  (case errno
    [(99) 'EGENERIC]     ;; generic error                    
    [( 1) 'EPERM]        ;; operation not permitted
    [( 2) 'ENOENT]       ;; no such file or directory
    [( 3) 'ESRCH]        ;; no such process
    [( 4) 'EINTR]        ;; interrupted function call
    [( 5) 'EIO]          ;; input/output error
    [( 6) 'ENXIO]        ;; no such device or address
    [( 7) 'E2BIG]        ;; arg list too long
    [( 8) 'ENOEXEC]      ;; exec format error
    [( 9) 'EBADF]        ;; bad file descriptor
    [(10) 'ECHILD]       ;; no child process
    [(11) 'EAGAIN]       ;; resource temporarily unavailable
    [(12) 'ENOMEM]       ;; not enough space
    [(13) 'EACCES]       ;; permission denied
    [(14) 'EFAULT]       ;; bad address
    [(15) 'ENOTBLK]      ;; Extension: not a block special file
    [(16) 'EBUSY]        ;; resource busy
    [(17) 'EEXIST]       ;; file exists
    [(18) 'EXDEV]        ;; improper link
    [(19) 'ENODEV]       ;; no such device
    [(20) 'ENOTDIR]      ;; not a directory
    [(21) 'EISDIR]       ;; is a directory
    [(22) 'EINVAL]       ;; invalid argument
    [(23) 'ENFILE]       ;; too many open files in system
    [(24) 'EMFILE]       ;; too many open files
    [(25) 'ENOTTY]       ;; inappropriate I/O control operation
    [(26) 'ETXTBSY]      ;; no longer used
    [(27) 'EFBIG]        ;; file too large
    [(28) 'ENOSPC]       ;; no space left on device
    [(29) 'ESPIPE]       ;; invalid seek
    [(30) 'EROFS]        ;; read-only file system
    [(31) 'EMLINK]       ;; too many links
    [(32) 'EPIPE]        ;; broken pipe
    [(33) 'EDOM]         ;; domain error       (from ANSI C std)
    [(34) 'ERANGE]       ;; result too large   (from ANSI C std)
    [(35) 'EDEADLK]      ;; resource deadlock avoided
    [(36) 'ENAMETOOLONG] ;; file name too long
    [(37) 'ENOLCK]       ;; no locks available
    [(38) 'ENOSYS]       ;; function not implemented
    [(39) 'ENOTEMPTY]    ;; directory not empty
    [(40) 'ELOOP]        ;; too many levels of symlinks detected
    [else errno]))

(define (symbol->errno error)
  (case error
    [(EGENERIC)     99]
    [(EPERM)         1]
    [(ENOENT)        2]
    [(ESRCH)         3]
    [(EINTR)         4]
    [(EIO)           5]
    [(ENXIO)         6]
    [(E2BIG)         7]
    [(ENOEXEC)       8]
    [(EBADF)         9]
    [(ECHILD)       10]
    [(EAGAIN)       11]
    [(ENOMEM)       12]
    [(EACCES)       13]
    [(EFAULT)       14]
    [(ENOTBLK)      15]
    [(EBUSY)        16]
    [(EEXIST)       17]
    [(EXDEV)        18]
    [(ENODEV)       19]
    [(ENOTDIR)      20]
    [(EISDIR)       21]
    [(EINVAL)       22]
    [(ENFILE)       23]
    [(EMFILE)       24]
    [(ENOTTY)       25]
    [(ETXTBSY)      26]
    [(EFBIG)        27]
    [(ENOSPC)       28]
    [(ESPIPE)       29]
    [(EROFS)        30]
    [(EMLINK)       31]
    [(EPIPE)        32]
    [(EDOM)         33]
    [(ERANGE)       34]
    [(EDEADLK)      35]
    [(ENAMETOOLONG) 36]
    [(ENOLCK)       37]
    [(ENOSYS)       38]
    [(ENOTEMPTY)    39]
    [(ELOOP)        40]
    [else           -1]))
