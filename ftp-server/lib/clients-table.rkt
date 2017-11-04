#|
RktFTPd Clients Table Library
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

(provide (all-defined-out))

(struct hash&count (table [count #:mutable]))

(define-syntax-rule (make-hash&count)
  (hash&count (make-hash) 0))

(define-syntax-rule (hash&count-ref? h&c key)
  (hash-ref (hash&count-table h&c) key #f))

(define-syntax-rule (hash&count-ref h&c key )
  (hash-ref (hash&count-table h&c) key))

(define-syntax-rule (hash&count-set! h&c key val)
  (hash-set! (hash&count-table h&c) key val))

(define-syntax-rule (hash&count-remove! h&c key)
  (hash-remove! (hash&count-table h&c) key))

(define-syntax-rule (make-clients-table)
  (make-hash&count))

(define-syntax-rule (clients-table-count tbl)
  (hash&count-count tbl))

(define-syntax-rule (clients-table-ref? tbl ip)
  (hash&count-ref? tbl ip))

(define-syntax-rule (clients-table-ref tbl ip)
  (hash&count-ref tbl ip))

(define-syntax-rule (clients-table-add tbl ip)
  (let ([count (clients-table-ref? tbl ip)])
    (hash&count-set! tbl ip (if count (add1 count) 1))
    (set-hash&count-count! tbl (add1 (clients-table-count tbl)))))

(define-syntax-rule (clients-table-remove tbl ip)
  (let ([count (clients-table-ref? tbl ip)])
    (if (> count 1)
        (hash&count-set! tbl ip (sub1 count))
        (hash&count-remove! tbl ip))
    (set-hash&count-count! tbl (sub1 (clients-table-count tbl)))))

