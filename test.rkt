#lang racket

(require "serve.rkt")

(make-grid 100 100 5)

(app)

#|
((point 20 40 (255,0,0)) (line 10 90 90 10 (0,255,0)) (circle 50 50 10 0 (0,0,255)) (rect 1 1 4 3 #f #f))
|#
