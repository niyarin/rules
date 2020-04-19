# rules
Scheme simple pattern matching function like syntax-rules.




### examples

```Scheme
(rules/match-expand '() 
                    '(let ((variable init) ...) body ...)
                    '((lambda (variable ...) body ...) init ...)
                    '(let ((a 1) (b 2)) 'hello (cons a b)))
           
;=>  ((lambda (a b) 'hello (cons a b)) 1 2)
```
