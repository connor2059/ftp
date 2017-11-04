#|
RktFTPd Timers Library
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

(require racket/async-channel
         (file "syntax.rkt"))

(struct timer (event
               expire-milliseconds
               interval-milliseconds
               action)
  #:mutable)

(provide/contract [timer? (-> any/c boolean?)]
                  [make-timer/msecs (-> real? [-> any] timer?)]
                  [make-timer (-> real? [-> any] timer?)]
                  [start-timer (-> timer? void?)]
                  [reset-timer/msecs (->* [timer?] [real?] void?)]
                  [reset-timer (->* [timer?] [real?] void?)]
                  [increment-timer/msecs (-> timer? real? void?)]
                  [increment-timer (-> timer? real? void?)]
                  [revise-timer (-> timer? real? [-> any] void?)]
                  [stop-timer (-> timer? void?)]
                  [cancel-timer (-> timer? void?)])

(define *timers-channel* (make-async-channel))

(define *active-timers* #f)

(define *timers-thread*
  (thread (λ ()
            (let loop ([timers null])
              (set! *active-timers* timers)
              (apply sync
                     (handle-evt *timers-channel*
                                 (λ (req) (loop (req timers))))
                     (for/list ([timer timers])
                       (handle-evt (timer-event timer)
                                   (λ (_)
                                     (with-handlers ([any/c void])
                                       ((timer-action timer)))
                                     (loop (remq timer timers))))))))))

(define (make-timer/msecs msecs thunk)
  (timer never-evt (current-inexact-milliseconds) msecs thunk))

(define (make-timer secs thunk)
  (make-timer/msecs (* 1000 secs) thunk))

(define (add-timer timer)
  (async-channel-put *timers-channel* 
                     (λ (timers) (list* timer timers))))

(define (start-timer timer)
  (let ([expire-milliseconds (+ (current-inexact-milliseconds) (timer-interval-milliseconds timer))])
    (set-timer-event! timer (alarm-evt expire-milliseconds))
    (set-timer-expire-milliseconds! timer expire-milliseconds))
  (add-timer timer))

(define (reset-timer/msecs timer [msecs (timer-interval-milliseconds timer)])
  (revise-timer timer msecs (timer-action timer)))

(define (reset-timer timer [secs (/ (timer-interval-milliseconds timer) 1000)])
  (reset-timer/msecs timer (* 1000 secs)))

(define (increment-timer/msecs timer msecs)
  (revise-timer timer
                (+ (- (timer-expire-milliseconds timer) (current-inexact-milliseconds))
                   msecs)
                (timer-action timer)))

(define (increment-timer timer secs)
  (increment-timer/msecs timer (* 1000 secs)))

(define (revise-timer timer msecs thunk)
  (let ([expire-milliseconds (+ (current-inexact-milliseconds) msecs)])
    (async-channel-put *timers-channel* 
                       (λ (timers)
                         (set-timer-event! timer (alarm-evt expire-milliseconds))
                         (set-timer-expire-milliseconds! timer expire-milliseconds)
                         (set-timer-interval-milliseconds! timer msecs)
                         (set-timer-action! timer thunk)
                         timers))))

(define (stop-timer timer)
  (async-channel-put *timers-channel* 
                     (λ (timers)
                       (set-timer-event! timer never-evt)
                       timers)))

(define (cancel-timer timer)
  (async-channel-put *timers-channel*
                     (λ (timers) (remq timer timers))))
