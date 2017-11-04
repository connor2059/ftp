#|
RktFTPd File System Library
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
                  uid-or-gid?)
         (for-syntax ffi/unsafe
                     racket/base
                     (only-in (file "ffi-core.rkt")
                              word-size)))

(provide read-mode
         write-mode
         execute-mode
         rx-mode
         wx-mode
         mode?
         read-ok
         write-ok
         execute-ok
         find-ok
         stat-mode-ifmt
         stat-mode-ifsock
         stat-mode-iflnk
         stat-mode-ifreg
         stat-mode-ifblk
         stat-mode-ifdir
         stat-mode-ifchr
         stat-mode-ififo
         stat-is?
         stat-link?
         stat-file?
         stat-directory?
         stat-character-device?
         stat-block-device?
         stat-fifo?
         stat-socket?
         (contract-out [make-mode (->* [#:owner (integer-in 0 7) #:group (integer-in 0 7) #:others (integer-in 0 7)]
                                       [#:set-uid? boolean? #:set-gid? boolean? #:sticky-bit? boolean?] 
                                       mode?)]
                       [access? (->* [path-string?] [#:mode (integer-in 0 7)] boolean?)]
                       [struct statinfo ([unique exact-nonnegative-integer?]
                                         [mode mode?]
                                         [nlink exact-nonnegative-integer?]
                                         [uid uid-or-gid?]
                                         [gid uid-or-gid?]
                                         [size exact-nonnegative-integer?]
                                         [mtime exact-nonnegative-integer?])]
                       [stat (path-string? . --> . statinfo?)]
                       [lstat (path-string? . --> . statinfo?)]
                       [rename chmod/s chmod (path-string? mode? . --> . void?)]
                       [rename chown/s chown (path-string? uid-or-gid? uid-or-gid? . --> . void?)]
                       [rename unlink/s unlink (path-string? . --> . void?)]
                       [rename mkdir/s mkdir (->* [path-string?] [#:mode mode?] void?)]
                       [rename rmdir/s rmdir (path-string? . --> . void?)]
                       [rename rename/s rename (path-string? path-string? . --> . void?)]
                       [rename utime/s utime (path-string? exact-nonnegative-integer? exact-nonnegative-integer? . --> . void?)]))

(define-lib libc "libc.so.6")

(typedef _mode_t     _uint)
(typedef _nlink_t    (?if (= word-size 32) _uint _ulong))
(typedef _ino_t      _ulong)
(typedef _ino64_t    (?if (= word-size 32) _uint64 _ulong))
(typedef _time_t     _long)
(typedef _off_t      _long)
(typedef _off64_t    (?if (= word-size 32) _int64 _long))
(typedef _blksize_t  _long)
(typedef _blkcnt_t   _long)
(typedef _blkcnt64_t (?if (= word-size 32) _int64 _long))
(typedef _dev_t      (?if (= word-size 32) _uint64 _ulong))

(?if (= word-size 32)
     (define-cstruct _Stat ([dev       _dev_t]
                            [__pad1    _ushort]
                            [ino       _ino_t]
                            [mode      _mode_t]
                            [nlink     _nlink_t]
                            [uid       _uid_t]
                            [gid       _gid_t]
                            [rdev      _dev_t]
                            [__pad2    _ushort]
                            [size      _off_t]
                            [blksize   _blksize_t]
                            [blocks    _blkcnt_t]
                            [atime     _time_t]
                            [atimensec _ulong]
                            [mtime     _time_t]
                            [mtimensec _ulong]
                            [ctime     _time_t]
                            [ctimensec _ulong]
                            [__unused  _uint64]))
     (define-cstruct _Stat ([dev       _dev_t]
                            [ino       _ino_t]
                            [nlink     _nlink_t]
                            [mode      _mode_t]
                            [uid       _uid_t]
                            [gid       _gid_t]
                            [__pad0    _int]
                            [rdev      _dev_t]
                            [size      _off_t]
                            [blksize   _blksize_t]
                            [blocks    _blkcnt_t]
                            [atime     _time_t]
                            [atimensec _ulong]
                            [mtime     _time_t]
                            [mtimensec _ulong]
                            [ctime     _time_t]
                            [ctimensec _ulong]
                            [__unused  (_array _long 3)])))

(?if (= word-size 32)
     (define-syntax-rule (make-empty-Stat)
       (make-Stat 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
     (define-syntax-rule (make-empty-Stat)
       (make-Stat 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (make-carray _long 3))))

(?if (= word-size 32)
     (define-cstruct _Stat64 ([dev       _dev_t]
                              [__pad1    _uint]
                              [__ino     _ino_t]
                              [mode      _mode_t]
                              [nlink     _nlink_t]
                              [uid       _uid_t]
                              [gid       _gid_t]
                              [rdev      _dev_t]
                              [__pad2    _uint]
                              [size      _off64_t]
                              [blksize   _blksize_t]
                              [blocks    _blkcnt64_t]
                              [atime     _time_t]
                              [atimensec _ulong]
                              [mtime     _time_t]
                              [mtimensec _ulong]
                              [ctime     _time_t]
                              [ctimensec _ulong]
                              [ino       _ino64_t]))
     (define-cstruct _Stat64 ([dev       _dev_t]
                              [ino       _ino64_t]
                              [nlink     _nlink_t]
                              [mode      _mode_t]
                              [uid       _uid_t]
                              [gid       _gid_t]
                              [__pad0    _int]
                              [rdev      _dev_t]
                              [size      _off_t]
                              [blksize   _blksize_t]
                              [blocks    _blkcnt64_t]
                              [atime     _time_t]
                              [atimensec _ulong]
                              [mtime     _time_t]
                              [mtimensec _ulong]
                              [ctime     _time_t]
                              [ctimensec _ulong]
                              [__unused  (_array _long 3)])))

(?if (= word-size 32)
     (define-syntax-rule (make-empty-Stat64)
       (make-Stat64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
     (define-syntax-rule (make-empty-Stat64)
       (make-Stat64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (make-carray _long 3))))

(define-cstruct _Utimbuf ([actime  _time_t]
                          [modtime _time_t]))

(defun-libc access (_string _int -> _int))
(defun-libc eaccess (_string _int -> _int))
(defun-libc euidaccess (_string _int -> _int))

(defconst STAT-VER-LINUX (?if (= word-size 32) 3 1))
(defun-libc __xstat (_int _string _Stat-pointer -> _int))
(defun-libc __lxstat (_int _string _Stat-pointer -> _int))
(defun-libc __xstat64 (_int _string _Stat64-pointer -> _int))
(defun-libc __lxstat64 (_int _string _Stat64-pointer -> _int))

(defun-libc utime (_string _Utimbuf-pointer -> _int))

(defun-libc mkdir (_string _mode_t -> _int))
(defun-libc rmdir (_string -> _int))

(defun-libc rename (_string _string -> _int))

(defun-libc unlink (_string -> _int))

(defun-libc chown (_string _uid_t _gid_t -> _int))
(defun-libc lchown (_string _uid_t _gid_t -> _int))
(defun-libc chmod (_string _mode_t -> _int))

(defconst read-mode    4)
(defconst write-mode   2)
(defconst execute-mode 1)
(defconst rx-mode      5)
(defconst wx-mode      3)

(define (mode? m)
  (and (exact-nonnegative-integer? m)
       (<= m #xFFFF)))

(define (make-mode #:owner  owner
                   #:group  group
                   #:others others
                   #:set-uid? [set-uid? #f]
                   #:set-gid? [set-gid? #f]
                   #:sticky-bit? [sticky-bit? #f])
  (+ (and set-uid? #o4000)
     (and set-gid? #o2000)
     (and sticky-bit? #o1000)
     (arithmetic-shift owner 6)
     (arithmetic-shift group 3)
     others))

;; Values for the second argument to access.
;; These may be OR'd together.
(defconst read-ok    4) ;; Test for read permission.
(defconst write-ok   2) ;; Test for write permission.
(defconst execute-ok 1) ;; Test for execute permission.
(defconst find-ok    0) ;; Test for existence.

(define (access? path #:mode [mode find-ok])
  (zero? (eaccess path mode)))

(struct statinfo [unique mode nlink uid gid size mtime])

(defconst stat-mode-ifmt   #o170000)
(defconst stat-mode-ifsock #o140000)
(defconst stat-mode-iflnk  #o120000)
(defconst stat-mode-ifreg  #o100000)
(defconst stat-mode-ifblk  #o060000)
(defconst stat-mode-ifdir  #o040000)
(defconst stat-mode-ifchr  #o020000)
(defconst stat-mode-ififo  #o010000)

(define-syntax-rule (stat-is? stat fmt)
  (= (bitwise-and (statinfo-mode stat) stat-mode-ifmt) fmt))

(define-syntax-rule (stat-link? stat) (stat-is? stat stat-mode-iflnk))
(define-syntax-rule (stat-file? stat) (stat-is? stat stat-mode-ifreg))
(define-syntax-rule (stat-directory? stat) (stat-is? stat stat-mode-ifdir))
(define-syntax-rule (stat-character-device? stat) (stat-is? stat stat-mode-ifchr))
(define-syntax-rule (stat-block-device? stat) (stat-is? stat stat-mode-ifblk))
(define-syntax-rule (stat-fifo? stat) (stat-is? stat stat-mode-ififo))
(define-syntax-rule (stat-socket? stat) (stat-is? stat stat-mode-ifsock))

(define-syntax-rule (call-stat fun path)
  (let ([st (make-empty-Stat64)])
    (call/0 fun STAT-VER-LINUX path st)
    (statinfo (bitwise-xor (Stat64-dev st) (Stat64-ino st))
              (Stat64-mode st)
              (Stat64-nlink st)
              (Stat64-uid st)
              (Stat64-gid st)
              (Stat64-size st)
              (Stat64-mtime st))))

(define (stat path)
  (call-stat __xstat64 path))

(define (lstat path)
  (call-stat __lxstat64 path))

(define (chmod/s path mode)
  (call/0 chmod path mode))

(define (chown/s path uid gid)
  (call/0 chown path uid gid))

(define (unlink/s path)
  (call/0 unlink path))

(define (mkdir/s path #:mode [mode #o755])
  (call/0 mkdir path mode))

(define (rmdir/s path)
  (call/0 rmdir path))

(define (rename/s old-path new-path)
  (call/0 rename old-path new-path))

(define (utime/s path actime modtime)
  (call/0 utime path (make-Utimbuf actime modtime))
  (void))
