#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/string))
(require (for-syntax syntax/stx))

(provide let_expr let_templ_expr match_expr
         let_bind let_templ_bind match_bind
         let_templ
         right_binding)

;;;;;;;;;;;;;;;; Desugar ;;;;;;;;;;;;;;;;
;; expand let item lists to nested let
(define-syntax (let_expr stx)
  (syntax-case stx (let_items)
    [(_ (let_items thing) body)
     #'(let_expr thing body)]
    [(_ (let_items first rest ...) body)
     #'(let_expr first (let_expr (let_items rest ...) body))]))

(define-syntax (let_bind stx)
  (syntax-case stx (let_items)
    [(_ (let_items thing) body)
     #'(let_bind thing body)]
    [(_ (let_items first rest ...) body)
     #'(let_bind first (let_bind (let_items rest ...) body))]))

(define-syntax (let_templ stx)
  (syntax-case stx (let_templ_items)
    [(_ (let_templ_items thing) body)
     #'(let_templ thing body)]
    [(_ (let_templ_items first rest ...) body)
     #'(let_templ first (let_templ (let_templ_items rest ...) body))]))

(define-syntax (let_templ_bind stx)
  (syntax-case stx (let_templ_items)
    [(_ (let_templ_items thing) body)
     #'(let_templ_bind thing body)]
    [(_ (let_templ_items first rest ...) body)
     #'(let_templ_bind first (let_templ (let_templ_items rest ...) body))]))

(define-syntax (let_templ_expr stx)
  (syntax-case stx (let_templ_items)
    [(_ (let_templ_items thing) body)
     #'(let_templ_expr thing body)]
    [(_ (let_templ_items first rest ...) body)
     #'(let_templ_expr first (let_templ (let_templ_items rest ...) body))]))

;; Desugar destructure to nested let
;; probably need to delay to next phase (treat as decoder)

;; Desugar match to if/else
(define-syntax (match_expr stx)
  (syntax-case stx ()
    [(_ expr cases ...)
     (with-syntax ([symbol (generate-temporary)])
       (with-syntax ([ifs (desugar-match-expr-case #'symbol (stx->list #'(cases ...)))])
         #'(let_expr (left_binding symbol expr) ifs)))]))

(define-for-syntax (desugar-match-expr-case top_id cases)
  (if (stx-null? cases)
      #'(bindset)
      (syntax-case (stx-car cases) (match_expr_case match_clause)
        [(match_expr_case (match_clause destruct condition) body)
         (with-syntax ([inner (desugar-match-expr-case top_id (stx-cdr cases))])
           (with-syntax ([top top_id])
             ;; in principle could move the destruct as a let around the if and mess with scope.
             ;; destructure twice is no big deal to duplicate, just creating references
             #'(if_expr (match_expr_cond top destruct condition)
                        (let_expr (left_binding destruct top) body)
                        inner)))]
        [(match_expr_case (match_clause destruct) body)
         (with-syntax ([inner (desugar-match-expr-case top_id (stx-cdr cases))])
           (with-syntax ([top top_id])
             #'(if_expr (match_expr_cond top destruct)
                        (let_expr (left_binding destruct top) body)
                        inner)))])))

(define-syntax (match_bind stx)
  (syntax-case stx ()
    [(_ expr cases ...)
     (with-syntax ([symbol (generate-temporary)])
       (with-syntax ([ifs (desugar-match-bind-case #'symbol (stx->list #'(cases ...)))])
         #'(let_bind (left_binding symbol expr) ifs)))]))

(define-for-syntax (desugar-match-bind-case top_id cases)
  (if (stx-null? cases)
      #'(bindset)
      (syntax-case (stx-car cases) (match_bind_case match_clause)
        [(match_bind_case (match_clause destruct condition) body)
         (with-syntax ([inner (desugar-match-bind-case top_id (stx-cdr cases))])
           (with-syntax ([top top_id])
             #'(if_bind (match_bind_cond top destruct condition)
                        (let_bind (left_binding destruct top) body)
                        inner)))]
        [(match_bind_case (match_clause destruct) body)
         (with-syntax ([inner (desugar-match-bind-case top_id (stx-cdr cases))])
           (with-syntax ([top top_id])
             #'(if_bind (match_bind_cond top destruct)
                        (let_bind (left_binding destruct top) body)
                        inner)))])))


;; Expand right binding lhs expressions to a let
(define-syntax (right_binding stx)
  (syntax-case stx (reference let_bind left_binding right_binding)
    [(_ (reference _ ...) _) stx]
    [(_ lhs rhs)
     (with-syntax ([symbol (generate-temporary)])
       #'(let_bind (left_binding symbol lhs) (right_binding symbol rhs)))]))
