#lang racket

(require web-server/servlet
         web-server/servlet-env
         "draw.rkt")

(provide make-grid
         app)

(define (form-figure . stx)
  (match-let ([(list embed/url fig text fields ...) stx])
    `(form ([action ,(embed/url (if (string=? fig "figures")
                                    (figures)
                                    (figure fig)))])
           ,text
           ,@(for/list ([i (in-list fields)])
               `(input ([name "number"] [placeholder ,i])))
           (input ([type "submit"])))))

(define (serve)
  (send/suspend/dispatch
   (λ (embed/url)
     (response/xexpr
      `(html (head (title "Rasterizer"))
             (body
              (h2   "Rasterizer")

              ,(form-figure embed/url "figures" "Figures ----> "
                            "Racket syntax")
              ,(form-figure embed/url "point" "Point ------> "
                            "X" "Y" "Color (standard: (0,0,0))")
              ,(form-figure embed/url "line" "Line -------> "
                            "X1" "Y1" "X2" "Y2" "Color (standard: (0,0,0))")
              ,(form-figure embed/url "circle" "Circle -----> "
                            "X" "Y" "R" "Fill? (standard: #f)" "Color (standard: (0,0,0))")
              ,(form-figure embed/url "rect" "Rectangle -> "
                            "X" "Y" "A" "B" "Fill? (standard: #f)" "Color (standard: (0,0,0))")
              ,(form-figure embed/url "trian" "Triangle ---> "
                            "X1" "Y1" "X2" "Y2" "X3" "Y3" "Fill? (standard: #f)" "Color (standard: (0,0,0))")
              ,(form-figure embed/url "curve" "Curve ------> "
                            "X1" "Y1" "X2" "Y2" "X3" "Y3" "Length (standard: 1)" "Color (standard: (0,0,0))")

              (table (form ([action ,(embed/url (undo))])
                           (input ([type "image"] [src "/undo.png"])))
                     ,error)
              (img ([src ,(grid->data-uri)])))))))
  (serve))

(define (app)
  (serve/servlet (λ _ (serve))
                 #:extra-files-paths (list ".")
                 #:servlet-path "/Rasterizer"))
