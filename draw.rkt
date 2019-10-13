#lang racket

(require math/array
         web-server/servlet
         "matrix2img.rkt"
         (for-syntax racket/syntax))

(provide error
         draw
         undo
         make-grid
         grid->data-uri)

(define error   " .")
(define len-x   #f)
(define len-y   #f)
(define len-p   #f)
(define len-x1  #f)
(define len-y1  #f)
(define grid    #f)
(define figures null)

(define-syntax (check-points stx)
  #`(if #,(syntax-case stx ()
            [(_ (nums ...) (xs ys) ...)
             #'(or (not nums) ... (not xs) ... (not ys) ...
                   (for/or ([i (in-combinations (list (cons xs ys) ...) 2)])
                     (equal? (car i) (cadr i)))
                   (< xs 0) ... (> xs len-x1) ...
                   (< ys 0) ... (> ys len-y1) ... )]
            [(_ _          (xs ys) ...)
             #'(or (< xs 0) ... (> xs len-x1) ...
                   (< ys 0) ... (> ys len-y1) ... )])
        (begin
          (set! error " Invalid coordinates!")
          (set-figures! (rest figures))
          #f)
        (begin
          (set! error " .")
          #t)))

(define-syntax (draw stx)
  (with-syntax* ([(_ figure) stx]
                 [draw-fig   (format-id #'grid "draw-~a" #'figure)])
    #'(λ (req)
        (let* ([inputs (map string->number
                            (extract-bindings 'number (request-bindings req)))]
               [inputs (if (last inputs)
                           inputs
                           (list-set inputs (sub1 (length inputs)) 1))])
          (set-figures! (cons (cons 'figure inputs) figures))
          (draw-fig inputs)))))

(define-syntax-rule (update x y color)
  (array-set! grid (vector (add1 x) (add1 y)) color))

(define-syntax-rule (undo)
  (λ _
    (unless (null? figures)
      (match-let ([(cons (cons figure inputs) others) figures])
        ((match figure
           ['point  draw-point]
           ['line   draw-line]
           ['rect   draw-rect]
           ['circle draw-circle])
         (list-set inputs (sub1 (length inputs)) 0))
        (set-figures! others)))))

(define-syntax-rule (in-line x1 y1 x2 y2)
  (let* ([df-x   (- x2 x1)]
         [df-y   (- y2 y1)]
         [max-df (max (abs df-x) (abs df-y))]
         [dx     (/ df-x max-df)]
         [dy     (/ df-y max-df)])
    (sequence-map (lambda (i)
                    (list (+ x1 (round (* i dx)))
                          (+ y1 (round (* i dy)))))
                  (in-range 0 (add1 max-df)))))

(define-syntax-rule (grid->data-uri)
  (array->data-uri grid len-x len-y len-p))

(define (make-grid x y p)
  (set! len-x  (+ 2 x))
  (set! len-y  (+ 2 y))
  (set! len-p  p)
  (set! len-x1 (sub1 x))
  (set! len-y1 (sub1 y))
  (set! grid   (array->mutable-array (make-array (vector len-x len-y) 1)))
  (array-slice-set! grid
                    (list (:: 1 (sub1 len-x))
                          (:: 1 (sub1 len-y)))
                    (array 0)))

(define (set-figures! new-figures)
  (set! figures new-figures))

(define (draw-point coord)
  (match-let ([(list x y color) coord])
    (when (check-points () (x y))
      (update x y color))))

(define (draw-line coord)
  (match-let ([(list x1 y1 x2 y2 color) coord])
    (when (check-points () (x1 y1) (x2 y2))
      (for ([pt (in-line x1 y1 x2 y2)])
        (match-let ([(list x y) pt])
          (update x y color))))))

(define (draw-rect coord)
  (match-let ([(list x1 y1 a b fill? color) coord])
    (when (check-points (a b) (x1 y1))
      (let ([x2 (+ a x1)]
            [y2 (+ b y1)])
        (when (check-points #f (x1 y1) ((sub1 x2) (sub1 y2)))
          (if fill?
              (for* ([x (in-range x1 x2)]
                     [y (in-range y1 y2)])
                (update x y color))
              (for ([pt (sequence-append
                         (in-line x1 y1 x2 y1)
                         (in-line x1 y1 x1 y2)
                         (in-line x2 y1 x2 y2)
                         (in-line x1 y2 x2 y2))])
                (match-let ([(list x y) pt])
                  (update x y color)))))))))

(define (circle-while x0 y0 d x y color)
  (update (+ x0 x) (+ y0 y) color)
  (update (- x0 x) (+ y0 y) color)
  (update (+ x0 x) (- y0 y) color)
  (update (- x0 x) (- y0 y) color)
  (update (+ x0 y) (+ y0 x) color)
  (update (- x0 y) (+ y0 x) color)
  (update (+ x0 y) (- y0 x) color)
  (update (- x0 y) (- y0 x) color)
  (when (> y x)
    (if (< d 0)
        (circle-while x0 y0 (+ d (* 2 x) 3)       (add1 x) y        color)
        (circle-while x0 y0 (+ d (* 2 (- x y)) 5) (add1 x) (sub1 y) color))))

(define (filled-circle-while x0 y0 d x y color)
  (for ([i (in-range (- x0 x) (+ x0 x 1))])
    (update i (- y0 y) color)
    (update i (+ y0 y) color))
  (for ([i (in-range (- x0 y) (+ x0 y 1))])
    (update i (- y0 x) color)
    (update i (+ y0 x) color))
  (when (> y x)
    (if (< d 0)
        (filled-circle-while x0 y0 (+ d (* 2 x) 3)       (add1 x) y        color)
        (filled-circle-while x0 y0 (+ d (* 2 (- x y)) 5) (add1 x) (sub1 y) color))))

(define (draw-circle coord)
  (match-let* ([(list x0 y0 r fill? color) coord])
    (when (check-points (r) (x0 y0))
      (let ([x+r (+ x0 r)]
            [y+r (+ y0 r)]
            [x-r (- x0 r)]
            [y-r (- y0 r)])
        (when (check-points #f (x+r y+r) (x-r y+r) (x+r y-r) (x-r y-r))
          (if fill?
              (filled-circle-while x0 y0 (- 1 r) 0 r color)
              (circle-while        x0 y0 (- 1 r) 0 r color)))))))
