#|
RktFTPd OpenSSL Library
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

(require openssl)

(provide (except-out (all-from-out openssl)
                     ssl-load-certificate-chain!
                     ssl-load-verify-root-certificates!
                     ssl-load-suggested-certificate-authorities!
                     ssl-load-private-key!)
         (rename-out [ssl-load*-certificate-chain! ssl-load-certificate-chain!]
                     [ssl-load*-verify-root-certificates! ssl-load-verify-root-certificates!]
                     [ssl-load*-suggested-certificate-authorities! ssl-load-suggested-certificate-authorities!]
                     [ssl-load*-private-key! ssl-load-private-key!]))

(define-syntax-rule (path-converter path locale-encoding)
  (let ([conv (bytes-open-converter "UTF-8" locale-encoding)])
    (let-values ([(bstr len result) 
                  (bytes-convert conv 
                                 (path->bytes
                                  (path->complete-path (cleanse-path path)
                                                       (current-directory))))])
      (bytes-close-converter conv)
      (bytes->path bstr 'unix))))

(define (ssl-load*-certificate-chain! ssl-context-or-listener path [path-locale-encoding "UTF-8"])
  (ssl-load-certificate-chain! ssl-context-or-listener (path-converter path path-locale-encoding)))

(define (ssl-load*-verify-root-certificates! ssl-context-or-listener path [path-locale-encoding "UTF-8"])
  (ssl-load-verify-root-certificates! ssl-context-or-listener (path-converter path path-locale-encoding)))

(define (ssl-load*-suggested-certificate-authorities! ssl-listener path [path-locale-encoding "UTF-8"])
  (ssl-load-suggested-certificate-authorities! ssl-listener (path-converter path path-locale-encoding)))

(define (ssl-load*-private-key! ssl-context-or-listener path [rsa? #t] [asn1? #f] [path-locale-encoding "UTF-8"])
  (ssl-load-private-key! ssl-context-or-listener (path-converter path path-locale-encoding) rsa? asn1?))
