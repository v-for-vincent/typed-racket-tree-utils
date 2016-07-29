#lang typed/racket
(require "tree.rkt")
(require/typed racket/base [string-append (-> String String String)])

(: debranch (-> String String))
(define (debranch m)
  (cond [(equal? m "") ""]
        [(equal? (substring m (- (string-length m) 2) (string-length m)) "+-") "| "]
        [else "  "])) ; `- is only option and need 'else' to typecheck

(: next-margin (-> String Boolean String))
(define (next-margin margin last-child?)
  (cond [(and last-child? (equal? margin "")) "`-"]
        [(equal? margin "") "+-"]
        [(and last-child? (equal? (substring margin (- (string-length margin) 2) (string-length margin)) "+-")) (string-append (substring margin 0 (- (string-length margin) 2)) "| `-")]
        [(equal? (substring margin (- (string-length margin) 2) (string-length margin)) "+-") (string-append (substring margin 0 (- (string-length margin) 2)) "| +-")]
        [(and last-child? (equal? (substring margin (- (string-length margin) 2) (string-length margin)) "`-")) (string-append (substring margin 0 (- (string-length margin) 2)) "  `-")]
        [else (string-append (substring margin 0 (- (string-length margin) 2)) "  +-")]))

(: list-init (All (A) (-> (Listof A) (Listof A))))
(define (list-init lst)
  (take lst (- (length lst) 1)))

(: tree-display (All (A) (-> (node A) (-> (node A) Void) Void)))
(define (tree-display the-tree node-display)
  (tree-display-aux "" the-tree node-display))

(: tree-display-aux (All (A) (-> String (node A) (-> (node A) Void) Void)))
(define (tree-display-aux margin the-tree node-display)
  (if (null? (node-children the-tree))
      (begin (display margin) (node-display the-tree) (newline))
      (begin (display margin)
             (node-display the-tree)
             (newline)
             (map (λ ([c : (node A)]) (begin (tree-display-aux (next-margin margin #f) c node-display))) (list-init (node-children the-tree)))
             (tree-display-aux (next-margin margin #t) (last (node-children the-tree)) node-display))))

(provide tree-display)

; TODO move to test file
(: test-tree (node String))
(define test-tree
  (let* ([a (node "a" '())]
         [b (node "b" '())]
         [c (node "c" '())]
         [d (node "d" '())]
         [e (node "e" '())]
         [f (node "f" '())]
         [g (node "g" '())]
         [ab (node "ab" (list a b))]
         [cd (node "cd" (list c d))]
         [cde (node "cde" (list cd e))]
         [g* (node "g*" (list g))]
         [fg* (node "fg*" (list f g*))])
    (node "abcdefg" (list ab cde fg*))))

(: test-node-display (-> (node String) Void))
(define (test-node-display my-node)
  (display (node-label my-node)))

(: test (-> Void))
(define (test) (tree-display test-tree test-node-display))