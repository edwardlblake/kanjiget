#lang racket/base

#|

    KanjiGet
    Copyright 2011-2012 Edward L. Blake

    This file is part of KanjiGet.

    KanjiGet is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    KanjiGet is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with KanjiGet.  If not, see <http://www.gnu.org/licenses/>.

|#

(require ffi/unsafe)

(provide WINAPI_SetWindowPos
         WINAPI_HWND_TOPMOST
         WINAPI_HWND_NOTOPMOST)

(define WINAPI_SetWindowPos
  (case (system-type)
    [[windows]
     (get-ffi-obj "SetWindowPos" "user32" (_fun _pointer _int _int _int _int _int _int -> _int)
                  (λ () (void)))]
    [else void]))
(define WINAPI_HWND_TOPMOST  -1)
(define WINAPI_HWND_NOTOPMOST -2)
