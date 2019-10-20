#lang racket

(require 2htdp/image
         math/array
         net/base64
         file/convertible)

(provide array->data-uri)

(define-syntax-rule (pict->data-uri pict)
  (format "data:image/png;base64,~a"
          (base64-encode (convert pict 'png-bytes))))

(define-syntax-rule (bin->color bin)
  (match-let ([(vector r g b) bin])
    (color r g b)))

(define-syntax-rule (array->data-uri arr len-x len-y len-p)
  (pict->data-uri
   (apply above
          (for/list ([y (in-range len-y)])
            (apply beside
                   (for/list ([x (in-range len-x)])
                     (rectangle len-p len-p 'solid
                                (bin->color (array-ref arr (vector x y))))))))))
