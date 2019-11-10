#lang racket

(require math/array
         web-server/servlet
         "matrix2img.rkt")

(provide error
         figure
         figures
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
(define figs    null)

(define-syntax (check-points stx)
  #`(if #,(syntax-case stx ()
            [(_ (nums ...) (xs ys) ...)
             #'(or (not nums) ... (< nums 0) ...
                   (not xs) ... (not ys) ...
                   (for/or ([i (in-combinations (list (cons xs ys) ...) 2)])
                     (equal? (car i) (cadr i)))
                   (< xs 0) ... (> xs len-x1) ...
                   (< ys 0) ... (> ys len-y1) ... )]
            [(_ _ (xs ys) ...)
             #'(or (< xs 0) ... (> xs len-x1) ...
                   (< ys 0) ... (> ys len-y1) ... )])
        (begin
          (set! error " Invalid coordinates!")
          (set-figs! (cdr figs))
          #f)
        (begin
          (set! error " .")
          #t)))

(define-syntax-rule (extract-figs text)
  (let ([text* (string-normalize-spaces text)])
    (map string-split
         (string-split
          (string-replace
           (substring text 2 (- (string-length text*) 2))
           ") (" "*")
          "*"))))

(define-syntax-rule (figures)
  (λ (req)
    (let* ([text (extract-binding/single 'number (request-bindings req))]
           [figs (extract-figs text)])
      (for ([inputs figs])
        (draw inputs)))))

(define-syntax-rule (figure fig)
  (λ (req)
    (let ([inputs (cons fig (extract-bindings 'number (request-bindings req)))])
      (draw inputs))))

(define-syntax-rule (draw inputs)
  (let* ([fig    (car inputs)]
         [last-i (last inputs)]
         [inputs (append (map string->number (drop-right (cdr inputs) 1))
                         (normalize-color last-i))])
    ((eval-draw fig) inputs)
    (set-figs! (cons (cons fig inputs) figs))))

(define-syntax-rule (undo)
  (λ _
    (unless (null? figs)
      (match-let ([(cons (cons fig inputs) others) figs])
        ((eval-draw fig) (list-set inputs (sub1 (length inputs))
                                   (vector 255 255 255)))
        (set-figs! others)))))

(define-syntax-rule (eval-draw fig)
  (match fig
    ["point"  draw-point]
    ["line"   draw-line]
    ["circle" draw-circle]
    ["rect"   draw-rect]
    ["trian"  draw-trian]
    ["curve"  draw-curve]))

(define-syntax-rule (normalize-color text)
  (list
   (if (or (string=? text "") (string=? text "#f"))
       (vector 0 0 0)
       (list->vector (map string->number
                          (string-split (substring text 1
                                                   (sub1 (string-length text)))
                                        ","))))))

(define-syntax-rule (update x y color)
  (array-set! grid (vector (add1 x) (add1 y)) color))


(define-syntax-rule (grid->data-uri)
  (array->data-uri grid len-x len-y len-p))

(define-syntax-rule (amount-pts x1 y1 x2 y2 x3 y3)
  (+ (integer-sqrt (+ (expt (- x2 x1) 2)
                      (expt (- y2 y1) 2)))
     (integer-sqrt (+ (expt (- x2 x3) 2)
                      (expt (- y2 y3) 2)))))

;-------------------------------------------

(define (make-grid x y p)
  (set! len-x  (+ 2 x))
  (set! len-y  (+ 2 y))
  (set! len-p  p)
  (set! len-x1 (sub1 x))
  (set! len-y1 (sub1 y))
  (set! grid   (array->mutable-array (make-array (vector len-x len-y) (vector 0 0 0))))
  (array-slice-set! grid
                    (list (:: 1 (sub1 len-x))
                          (:: 1 (sub1 len-y)))
                    (array (vector 255 255 255))))

(define (set-figs! new-figs)
  (set! figs new-figs))

;-------------------------------------------

(define (draw-point coord)
  (match-let ([(list x y color) coord])
    (when (check-points () (x y))
      (update x y color))))

(define (draw-line coord)
  (match-let ([(list x1 y1 x2 y2 color) coord])
    (when (check-points () (x1 y1) (x2 y2))
      (make-line x1 y1 x2 y2 color))))

(define (draw-circle coord)
  (match-let* ([(list x0 y0 r fill? color) coord])
    (when (check-points (r) (x0 y0))
      (let ([x+r (+ x0 r)]
            [y+r (+ y0 r)]
            [x-r (- x0 r)]
            [y-r (- y0 r)])
        (when (check-points #f (x+r y+r) (x-r y+r) (x+r y-r) (x-r y-r))
          (if fill?
              (fill-circle-while x0 y0 (- 1 r) 0 r color)
              (circle-while      x0 y0 (- 1 r) 0 r color)))))))

(define (draw-rect coord)
  (match-let ([(list x1 y1 a b fill? color) coord])
    (when (check-points (a b) (x1 y1))
      (let* ([x2   (+ a x1)]
             [y2   (+ b y1)]
             [x2-1 (sub1 x2)]
             [y2-1 (sub1 y2)])
        (when (check-points #f (x2-1 y2-1))
          (if fill?
              (for* ([x (in-range x1 x2)]
                     [y (in-range y1 y2)])
                (update x y color))
              (begin
                (make-line x1 y1 x2 y1 color)
                (make-line x1 y1 x1 y2 color)
                (make-line x2 y1 x2 y2 color)
                (make-line x1 y2 x2 y2 color))))))))

(define (draw-trian coord)
  (match-let ([(list x1 y1 x2 y2 x3 y3 fill? color) coord])
    (when (check-points () (x1 y1) (x2 y2) (x3 y3))
      (when fill?
        (make-triangle x1 y1 x2 y2 x3 y3 color))
      (make-line x1 y1 x2 y2 color)
      (make-line x1 y1 x3 y3 color)
      (make-line x2 y2 x3 y3 color))))

(define (draw-curve coord)
  (match-let*([(list x1 y1 x2 y2 x3 y3 len color) coord]
              [len                                (cond [len] [1])])
    (when (check-points (len) (x1 y1) (x2 y2) (x3 y3))
      (let* ([df-x   (- x2 (/ (+ x1 x3) 2))]
             [df-y   (- y2 (/ (+ y1 y3) 2))]
             [sum-df (+ (abs df-x) (abs df-y))]
             [dx     (/ df-x sum-df)]
             [dy     (/ df-y sum-df)]
             [amount (/ 1 (amount-pts  x1 y1 x2 y2 x3 y3))]
             [x2.2   (+ (* dx len) x2)]
             [y2.2   (+ (* dy len) y2)])
        (when (check-points #f (x2.2 y2.2))
          (let ([fx (λ (t) (+ (*   x1 (expt (- 1 t) 2))
                              (* 2 x2       (- 1 t)    t)
                              (*   x3                  (expt t 2))))]
                [fy (λ (t) (+ (*   y1 (expt (- 1 t) 2))
                              (* 2 y2       (- 1 t)    t)
                              (*   y3                  (expt t 2))))])
            (for ([t (in-range 0 (add1 amount) amount)])
              (let ([x (round (fx t))]
                    [y (round (fy t))])
                (for ([i (in-range len)])
                  (update (+ x (round (* i dx)))
                          (+ y (round (* i dy)))
                          color))))))))))

;-------------------------------------------

(define (make-line x1 y1 x2 y2 color)
  (let* ([df-x   (- x2 x1)]
         [df-y   (- y2 y1)]
         [max-df (max (abs df-x) (abs df-y))]
         [dx     (/ df-x max-df)]
         [dy     (/ df-y max-df)])
    (for ([i (in-range (add1 max-df))])
      (update (+ x1 (round (* i dx)))
              (+ y1 (round (* i dy)))
              color))))

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

(define (fill-circle-while x0 y0 d x y color)
  (for ([i (in-range (- x0 x) (+ x0 x 1))])
    (update i (- y0 y) color)
    (update i (+ y0 y) color))
  (for ([i (in-range (- x0 y) (+ x0 y 1))])
    (update i (- y0 x) color)
    (update i (+ y0 x) color))
  (when (> y x)
    (if (< d 0)
        (fill-circle-while x0 y0 (+ d (* 2 x) 3)       (add1 x) y        color)
        (fill-circle-while x0 y0 (+ d (* 2 (- x y)) 5) (add1 x) (sub1 y) color))))

(define (make-triangle x1 y1 x2 y2 x3 y3 color)
  (match-let ([(list (cons lx ly) (cons mx my) (cons tx ty))
               (sort (list (cons x1 y1) (cons x2 y2) (cons x3 y3))
                     < #:key cdr)])
    (let* ([df-mty (- ty my)]
           [df-lty (- ty ly)]
           [df-lmy (- my ly)]
           [dxmt   (if (zero? df-mty) 0 (/ (- tx mx) df-mty))]
           [dxlt   (if (zero? df-lty) 0 (/ (- tx lx) df-lty))]
           [dxlm   (if (zero? df-lmy) 0 (/ (- mx lx) df-lmy))]
           [proj-x (cond
                     [(zero? df-mty) tx]
                     [(zero? df-lmy) lx]
                     [else (+ lx (* my dxlt))])]
           [var    (if (> proj-x mx) 1 -1)]
           [-var   (- var)])
      (for ([i (in-range 1 df-lmy)])
        (make-line (round (+ lx (* i dxlm))) (+ ly i)
                   (round (+ lx (* i dxlt))) (+ ly i) color))
      (make-line mx my proj-x my color)
      (for ([i (in-range 1 df-mty)])
        (make-line (round (- tx (* i dxmt))) (- ty i)
                   (round (- tx (* i dxlt))) (- ty i) color)))))
