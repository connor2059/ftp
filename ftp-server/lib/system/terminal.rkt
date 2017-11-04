#|
RktFTPd Terminal Library
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
         (file "signals.rkt"))

(provide (contract-out [read-password (->* [] [input-port? #:abort-evt (--> any)] [or/c string? eof-object?])]))

(define-lib libc "libc.so.6")

(typedef _cc_t      _ubyte)
(typedef _speed_t   _uint)
(typedef _tcflag_t  _uint)

(defconst NCCS 32)

(define-cstruct _Termios ([iflag  _tcflag_t]
                          [oflag  _tcflag_t]
                          [cflag  _tcflag_t]
                          [lflag  _tcflag_t]
                          [line   _cc_t]
                          [cc     (_array _cc_t NCCS)]
                          [ispeed _speed_t]
                          [ospeed _speed_t]))

(define-syntax-rule (make-empty-Termios)
  (make-Termios 0 0 0 0 0 (make-carray _cc_t NCCS) 0 0))

;; Standard input value, stdin.
;;
;;;; If necessary, you can get it:
;;;;  (defun-libc fileno (_pointer -> _int))
;;;;  
;;;;  (define stdin (make-c-parameter "stdin" libc _pointer))	
;;;;  (displayln (fileno (stdin)))
(defconst STDIN_FILENO 0)
;; The change occurs immediately.
(defconst TCSANOW 0)
;; The change occurs after all output written to the file descriptor has been transmitted, 
;; and all input so far received but not read is discarded before the change is made.
(defconst TCSAFLUSH 2)
;; Enable echo
(defconst ECHO #o10)

(defun-libc tcgetattr (_int _Termios-pointer -> _int))
(defun-libc tcsetattr (_int _int _Termios-pointer -> _int))

(define (read-password [in (current-input-port)] #:abort-evt [abort-evt void])
  (let ([cust (make-custodian)]
        [oldTermios (make-empty-Termios)]
        [password eof])
    (dynamic-wind
     (λ ()
       (tcgetattr STDIN_FILENO oldTermios)
       (let ([newTermios (struct-copy Termios oldTermios)])
         (set-Termios-lflag! newTermios 
                             (bitwise-and (Termios-lflag newTermios)
                                          (bitwise-not ECHO)))
         (tcsetattr STDIN_FILENO TCSAFLUSH newTermios)))
     (λ ()
       (call-with-sighandler (λ ()
                               (with-handlers ([exn:break? (λ (e) (abort-evt))])
                                 (parameterize ([current-custodian cust])
                                   (thread-wait (thread (λ () (set! password (read-line))))))))
                             (λ (sig)
                               (abort-evt)
                               (custodian-shutdown-all cust))
                             #:signals '(SIGINT SIGTSTP)))
     (λ ()
       (tcsetattr STDIN_FILENO TCSANOW oldTermios)))
    password))
