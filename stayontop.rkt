#lang racket/base

(require ffi/unsafe)

(provide WINAPI_SetWindowPos
         WINAPI_HWND_TOPMOST
         WINAPI_HWND_NOTOPMOST)

(define WINAPI_SetWindowPos
  (case (system-type)
    [[windows]
     (get-ffi-obj "SetWindowPos" "user32" (_fun _pointer _int _int _int _int _int _int -> _int)
                  (lambda () (void)))]
    [else void]))
(define WINAPI_HWND_TOPMOST  -1)
(define WINAPI_HWND_NOTOPMOST -2)
