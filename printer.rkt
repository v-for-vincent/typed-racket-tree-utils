#lang typed/racket
(require "tree.rkt")

(: tree-print (-> node (-> node String) Void))
(define (tree-print the-tree node-print)
  (tree-print-aux "" the-tree node-print))
(define (tree-print-aux margin the-tree node-print)
  (void))
(provide tree-print)
  