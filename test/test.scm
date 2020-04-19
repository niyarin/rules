(include "../src/rules.scm")

(import (scheme base)
        (niyarin rules)
        (scheme write);;
        (srfi 78))

(let ((matched (rules/match '()
                            '(x ... y)
                            '(3 4 5))))
  (check (not (not matched)) => #t)
  (check (assq 'x matched) => '(x 3 4))
  (check (assq 'y matched) => '(y . 5)))

(let ((matched (rules/match '()
                            '(a (b ...) ... c)
                            '(1 (2 3 4) (5 6) (7) 8))))
  (check (not (not matched)) => #t)
  (check (assq 'a matched) => '(a . 1))
  (check (assq 'b matched) => '(b (2 3 4) (5 6) (7)))
  (check (assq 'c matched) => '(c . 8)))

(let ((matched (rules/match '()
                            '(a b)
                            '(1 2 3 4))))
  (check (not (not matched)) => #f))

(let ((match-expand-res
        (rules/match-expand '() '(let ((variable init) ...) body ...)
                                '((lambda (variable ...) body ...) init ...)
                                '(let ((a 1) (b 2)) 'hello (cons a b)))))
  (check match-expand-res => '((lambda (a b) 'hello (cons a b)) 1 2)))

(check-report)
