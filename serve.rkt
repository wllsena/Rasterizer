#lang racket

(require web-server/servlet
         web-server/servlet-env
         "draw.rkt"
         (for-syntax racket/syntax))

(provide make-grid
         app)

(define (form-figure . stx)
  (match-let ([(list embed/url fig text fields ...) stx])
    `(form ([action ,(embed/url (match fig
                                  ['point  (draw point)]
                                  ['line   (draw line)]
                                  ['rect   (draw rect)]
                                  ['triag  (draw rect)]
                                  ['circle (draw circle)]))])
           ,text
           ,@(for/list ([i (in-list fields)])
               `(input ([name "number"] [placeholder ,i])))
           (input ([type "submit"])))))

(define (serve)
  (send/suspend/dispatch
   (λ (embed/url)
     (response/xexpr
      `(html (head (title "Renderer"))
             (body
              (h2   "Hello!")

              ,(form-figure embed/url 'point "Point ------> "
                            "X" "Y" "Color (standard: 1)")
              ,(form-figure embed/url 'line "Line -------> "
                            "X1" "Y1" "X2" "Y2" "Color (standard: 1)")
              ,(form-figure embed/url 'rect "Rectangle -> "
                            "X" "Y" "A" "B" "Fill? (standard: #f)" "Color (standard: 1)")
              ,(form-figure embed/url 'triag "Triangle ---> "
                            "X1" "Y1" "X2" "Y2" "X3" "Y3" "Fill? (standard: #f)" "Color (standard: 1)")
              ,(form-figure embed/url 'circle "Circle -----> "
                            "X" "Y" "R" "Fill? (standard: #f)" "Color (standard: 1)")

              (table (form ([action ,(embed/url (undo))])
                           (input ([type "image"] [src "/undo.png"])))
                     ,error)
              (img ([src ,(grid->data-uri)])))))))
  (serve))

(define (app)
  (serve/servlet (λ _ (serve))
                 #:extra-files-paths (list ".")
                 #:servlet-path "/serve.rkt"))
