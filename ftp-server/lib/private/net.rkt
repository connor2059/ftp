#|
RktFTPd Net Library
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

(require racket/string
         openssl
         (file "syntax.rkt"))

(provide host-string?
         port-number?
         ssl-protocol?
         IP-addr?
         IPv4?
         IPv6?
         private-IPv4?)

(define byte-pattern "0*(?:25[0-5]|2[0-4]\\d|1\\d\\d|\\d\\d|\\d)")

(define IPv4-pattern 
  (pregexp (string-replace "(?![0.]*$)^b(?:\\.b){3}$" "b" byte-pattern)))

(define (IPv4? ip)
  (and (string? ip)
       (regexp-match? IPv4-pattern ip)))

(define IPv6-pattern
  (let* ([h  "[[:xdigit:]]{1,4}"]
         [h? "[[:xdigit:]]{0,4}"]
         [r4 (regexp-replaces "(?::h)?:h?" `([#rx"h\\?" ,h?] [#rx"h" ,h]))]
         [p  "(?::h){0,n}:h?|h:(?:r)"]
         [r3 (regexp-replaces p `([#rx"n" "2"][#rx"r" ,r4][#rx"h\\?" ,h?][#rx"h" ,h]))]
         [r2 (regexp-replaces p `([#rx"n" "3"][#rx"r" ,r3][#rx"h\\?" ,h?][#rx"h" ,h]))]
         [r1 (regexp-replaces p `([#rx"n" "4"][#rx"r" ,r2][#rx"h\\?" ,h?][#rx"h" ,h]))]
         [r0 (regexp-replaces p `([#rx"n" "5"][#rx"r" ,r1][#rx"h\\?" ,h?][#rx"h" ,h]))])
    (pregexp (regexp-replaces "(?![:0]*$)^h?:(?:r)$" `([#rx"h\\?" ,h?][#rx"r" ,r0])))))

(define (IPv6? ip)
  (and (string? ip)
       (regexp-match? IPv6-pattern ip)))

(define private-IPv4-pattern 
  (let ([r1 (format "0*(?:10|127)(?:\\.~a){3}" byte-pattern)]
        [r2 (format "0*169\\.0*254(?:\\.~a){2}" byte-pattern)]
        [r3 (format "0*192\\.0*168(?:\\.~a){2}" byte-pattern)]
        [r4 (format "0*172\\.0*16\\.0*31\\.~a" byte-pattern)])
    (pregexp (format "^(?:~a)$" (string-join (list r1 r2 r3 r4) "|")))))

(define (private-IPv4? ip)
  (and (IPv4? ip)
       (regexp-match? private-IPv4-pattern ip)))

(define (IP-addr? ip)
  (or (IPv4? ip) (IPv6? ip)))

(define (host-string? host)
  (and (string? host)
       (or (IP-addr? host) (string-ci=? host "localhost"))))

(define (port-number? port)
  (and (exact-nonnegative-integer? port) (port . <= . #xffff)))

(define (ssl-protocol? prt)
  (pair? (memq prt (supported-server-protocols))))
