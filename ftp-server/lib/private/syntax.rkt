#|
RktFTPd Syntax Library
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

(provide and/exc
         define-member-key
         let-member-name
         provide/define-member-key)

(define-syntax-rule (and/exc expr ...)
  (with-handlers ([any/c (λ (e) #f)])
    (and expr ...)))

(define-syntax-rule (define-member-key key keys ...)
  (begin (define key (generate-member-key))
         (define keys (generate-member-key)) ...))

(define-syntax-rule (let-member-name ([name key] ...) e1 e2 ...)
  (let ()
    (begin (define-member-name name key) ...)
    e1 e2 ...))

(define-syntax-rule (provide/define-member-key key keys ...)
  (begin (provide key keys ...)
         (define-member-key key keys ...)))
