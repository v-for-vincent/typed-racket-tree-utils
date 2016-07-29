#lang typed/racket
(struct (a) node ([label : a] [children : (Listof (node a))]))
(provide (struct-out node))