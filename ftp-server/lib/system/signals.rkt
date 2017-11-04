#|
RktFTPd Signals Library
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

(provide (contract-out [symbol->signum (symbol? . --> . exact-integer?)]
                       [signum->symbol (exact-integer? . --> . [or/c symbol? exact-integer?])]
                       [block-signals ((non-empty-listof signal?) . --> . backup-block-signals?)]
                       [restore-block-signals (backup-block-signals? . --> . void?)]
                       [set-sighandler (->* [(--> integer? any)] [#:signals (non-empty-listof signal?)] backup-sighandlers?)]
                       [restore-sighandlers (backup-sighandlers? . --> . void?)]
                       [call-with-sighandler (->* [(--> any) (--> integer? any)] [#:signals (non-empty-listof signal?)] void?)]))

(define-lib libc "libc.so.6")

(defconst SIGSET_NWORDS (quotient 1024 (* 8 (ctype-sizeof _ulong))))

(define-cstruct _Sigset_t  ([val (_array _ulong SIGSET_NWORDS)]))

(define-syntax-rule (make-empty-Sigset_t)
  (let ([sigset (make-Sigset_t (make-carray _ulong SIGSET_NWORDS))])
    (sigemptyset sigset)
    sigset))

(define (apply-thunk thunk) (thunk))

(define-cstruct _Sigaction ([handler  (_fun #:async-apply apply-thunk _int -> _void)]
                            [mask     _Sigset_t]
                            [flags    _int]
                            [restorer (_fun #:atomic? #t -> _void)]))

(define-syntax-rule (make-empty-Sigaction)
  (make-Sigaction #f (make-empty-Sigset_t) 0 #f))

;; Add signals to the set of blocked signals.
(defconst SIG_BLOCK 0)
;; Set of blocked signals is given.
(defconst SIG_SETMASK 2)

(defun-libc sigaction (_int (_or-null _Sigaction-pointer) (_or-null _Sigaction-pointer) -> _int))
(defun-libc sigemptyset (_Sigset_t-pointer -> _int))
(defun-libc sigaddset (_Sigset_t-pointer _int -> _int))
(defun-libc sigprocmask (_int _Sigset_t-pointer (_or-null _Sigset_t-pointer) -> _int))
(defun-libc sigwait (_Sigaction-pointer _intptr -> _int))

(define (symbol->signum sig)
  (case sig
    [(SIGHUP)     1] ; Hangup (POSIX).
    [(SIGINT)     2] ; Interrupt (ANSI). (ctrl-c)
    [(SIGQUIT)    3] ; Quit (POSIX).
    [(SIGILL)     4] ; Illegal instruction (ANSI).
    [(SIGTRAP)    5] ; Trace trap (POSIX).
    [(SIGABRT        ; Abort (ANSI).
      SIGIOT)     6] ; IOT trap (4.2 BSD).
    [(SIGBUS)     7] ; BUS error (4.2 BSD).
    [(SIGFPE)     8] ; Floating-point exception (ANSI).
    [(SIGKILL)    9] ; Kill, unblockable (POSIX).
    [(SIGUSR1)   10] ; User-defined signal 1 (POSIX).
    [(SIGSEGV)   11] ; Segmentation violation (ANSI).
    [(SIGUSR2)   12] ; User-defined signal 2 (POSIX).
    [(SIGPIPE)   13] ; Broken pipe (POSIX).
    [(SIGALRM)   14] ; Alarm clock (POSIX).
    [(SIGTERM)   15] ; Termination (ANSI).
    [(SIGSTKFLT) 16] ; Stack fault.
    [(SIGCLD         ; Same as SIGCHLD (System V).
      SIGCHLD)   17] ; Child status has changed (POSIX).
    [(SIGCONT)   18] ; Continue (POSIX).
    [(SIGSTOP)   19] ; Stop, unblockable (POSIX).
    [(SIGTSTP)   20] ; Keyboard stop (POSIX). (ctrl-z)
    [(SIGTTIN)   21] ; Background read from tty (POSIX).
    [(SIGTTOU)   22] ; Background write to tty (POSIX).
    [(SIGURG)    23] ; Urgent condition on socket (4.2 BSD).
    [(SIGXCPU)   24] ; CPU limit exceeded (4.2 BSD).
    [(SIGXFSZ)   25] ; File size limit exceeded (4.2 BSD).
    [(SIGVTALRM) 26] ; Virtual alarm clock (4.2 BSD).
    [(SIGPROF)   27] ; Profiling alarm clock (4.2 BSD).
    [(SIGWINCH)  28] ; Window size change (4.3 BSD, Sun).
    [(SIGPOLL        ; Pollable event occurred (System V).
      SIGIO)     29] ; I/O now possible (4.2 BSD).
    [(SIGPWR)    30] ; Power failure restart (System V).
    [(SIGSYS)    31] ; Bad system call.
    [(SIGUNUSED) 31]
    [else -1]))

(define (signum->symbol sig)
  (case sig
    [( 1) 'SIGHUP]
    [( 2) 'SIGINT]         
    [( 3) 'SIGQUIT]
    [( 4) 'SIGILL]
    [( 5) 'SIGTRAP]
    [( 6) 'SIGABRT]
    [( 8) 'SIGFPE]
    [( 9) 'SIGKILL]
    [(10) 'SIGUSR1]
    [(11) 'SIGSEGV]
    [(12) 'SIGUSR2]
    [(13) 'SIGPIPE]
    [(14) 'SIGALRM]
    [(15) 'SIGTERM]
    [(16) 'SIGSTKFLT]
    [(17) 'SIGCHLD]
    [(18) 'SIGCONT]
    [(19) 'SIGSTOP]
    [(20) 'SIGTSTP]
    [(21) 'SIGTTIN]
    [(22) 'SIGTTOU]
    [(31) 'SIGSYS]
    [else sig]))

(define (signal? sig)
  (exact-positive-integer? (if (symbol? sig)
                               (symbol->signum sig)
                               sig)))

(define default-signals '(SIGINT SIGTERM SIGQUIT))

(struct backup-block-signals [sigset])

(define (block-signals signals)
  (let ([newSigset (make-empty-Sigset_t)]
        [oldSigset (make-empty-Sigset_t)])
    (for [(sig signals)]
      (sigaddset newSigset (if (symbol? sig)
                               (symbol->signum sig)
                               sig)))
    (sigprocmask SIG_BLOCK newSigset oldSigset)
    (backup-block-signals oldSigset)))

(define (restore-block-signals backup)
  (void (sigprocmask SIG_SETMASK (backup-block-signals-sigset backup) #f)))

(struct backup-sighandlers [sigacts])

(define (set-sighandler handler #:signals [signals default-signals])
  (let ([sigacts (for/list ([sig signals])
                   (cons (if (symbol? sig)
                             (symbol->signum sig)
                             sig)
                         (make-empty-Sigaction)))]
        [SigAct (make-empty-Sigaction)])
    (set-Sigaction-handler! SigAct handler)
    (for [(p sigacts)]
      (sigaction (car p) SigAct (cdr p)))
    (backup-sighandlers sigacts)))

(define (restore-sighandlers backup)
  (void (for [(p (backup-sighandlers-sigacts backup))]
          (sigaction (car p) (cdr p) #f))))

(define (call-with-sighandler proc handler #:signals [signals default-signals])
  (let ([backup #f])
    (dynamic-wind
     (λ () (set! backup (set-sighandler handler #:signals signals)))
     proc
     (λ () (when backup (restore-sighandlers backup))))))
