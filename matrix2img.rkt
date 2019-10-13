#lang racket

(require 2htdp/image
         math/array
         net/base64
         file/convertible)

(provide array->data-uri)

(define (pict->data-uri pict)
  (format "data:image/png;base64,~a"
          (base64-encode (convert pict 'png-bytes))))

(define (bin->color bin)
  (match bin
    [0 (color 255 255 255)]
    [1 (color 0 0 0)]
    [2 (color 146 146 146)]
    [3 (color 255 38  0  )]
    [4 (color 255 38  0  )]
    [5 (color 0   255 0  )]
    [6 (color 4   50  255)]))

(define (array->data-uri arr len-x len-y len-p)
  (pict->data-uri
   (apply above
          (for/list ([y (in-range len-x)])
            (apply beside
                   (for/list ([x (in-range len-y)])
                     (rectangle len-p len-p 'solid
                                (bin->color (array-ref arr (vector x y))))))))))
