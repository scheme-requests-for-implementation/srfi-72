
(load "simple-macros.scm")
(load "simple-syntax-case.scm")


(repl '
 (
  
  ;; Only ground-level bindings are exported.
  
  (module m (x y)
    (define x 1)
    (begin-for-syntax (define x 2))
    (begin-for-syntax (define y 3)))
  
  (import m)
  x                        ;==> 1
  ; (begin-for-syntax y)   ;==> reference to unidentified identifier: y
  
  
  
  
  ;; Removing bindings from an environment:
  
  (module remove-x (x))
  
  (define x 1)     
  x                  ;==> 1
  (import remove-x)  
  ; x                ;==> reference to unidentified identifier: x
  
  
  
  ;; Importing different bindings for the same name
  ;; at different levels of the reflective tower.  
  
  (module m (x)
    (define x 1))
  
  (module n (x)
    (define x 2))
  
  (module o (x)
    (define x 3))
  
  (module p ()
    (import m)
    (begin-for-syntax (import n))
    (begin-for-syntax (begin-for-syntax (import o)))
    
    (let-syntax ((foo (lambda (form)
                        (let-syntax ((bar (lambda (form)
                                            (quasisyntax
                                             (quasisyntax (list x ,x ,,x))))))
                          (bar)))))
      (display (foo))))
  
  (import p)  ;==> (1 2 3)
  
  
  
  ;; Importing into all levels:
  
  (module m (z)
    (define z 1))
  
  (import-for-all m)
  
  (let-syntax ((foo (lambda (form)
                      (let-syntax ((bar (lambda (form)
                                          (quasisyntax
                                           (quasisyntax (list z ,z ,,z))))))
                        (bar)))))
    (foo))   
  ;==> (1 1 1)
  
  
  
  ;; In the following example, u is available during compilation
  ;; at the meta-level for expanding the right hand side of v, while
  ;; v is available during compilation at the ground level for
  ;; expanding the argument of display. The display statement is
  ;; compiled but not evaluated until the module is later imported: 
  
  (module m ()
    
    (begin-for-syntax
      (define-syntax u (lambda (form) (syntax 1))))
    
    (define-syntax v (lambda (form) (quasisyntax (list ,(u) 2))))
    
    (display (v)))
  
  (import m)  ;==> (1 2)
  
  
  
  ;; In the following example, the code wrapped in begin-for-syntax is
  ;; evaluated when m is imported for syntax during
  ;; expansion of n, which imports it directly, and during expansion
  ;; of o, which imports it indirectly, and again when the toplevel
  ;; import form is expanded. The expanded module bodies are not executed
  ;; until the final toplevel import form is evaluated. 
  
  (module m ()
    (begin-for-syntax
      (display 'm-syntax))     ;==> m-syntax
    (display 'm-execute))     
  
  (module n ()
    (import m))               ;==> m-syntax
  
  (module o ()
    (import n)                ;==> m-syntax
    (import m))               
  
  (import o)                  ;==> m-syntax m-execute
  
  
  
  ;; In the following example, 'm is only displayed once
  ;; even if m is imported into o via two different
  ;; import chains.
  
  (module m ()
    (display 'm))   
  
  (module n ()  
    (import m)     
    (display 'n))   
  
  (module o ()    
    (import n)     
    (import m))            
  
  (import o)        ;==> m n
  
  
  ;; In the following example, during compilation of n,
  ;; the import wrapped in 
  ;; begin-for-syntax is first expanded, causing m to be instantiated
  ;; for syntax.  Since the form is wrapped in begin-for-syntax,
  ;; it is immediately evaluated, causing m to be instantiated
  ;; for execution.  Finally, compilation of the second
  ;; (import m) form does not instantiate m again
  ;; for expansion, since it has already been so instantiated.
  
  (module m ()
    (begin-for-syntax (display 'm-syntax))  ;==> m-syntax
    (display 'm-execute))
  
  (module n ()
    (begin-for-syntax (import m))           ;==> m-syntax m-execute
    (import m))
  
  
  
  
  ;; Another reflective tower example:
  
  (module m (x)
    (define x 1))
  
  (module n ()
    (begin-for-syntax
      (import m))
    (define x 2)
    (display (let-syntax ((m (lambda (_) x)))
               (m)))
    (display x)
    ) ; n
  
  (import n)  ;==> 1 2
  
  
  
  ;; Reflective tower example.  Note how bindings used at the
  ;; meta-level have to be defined inside begin-for-syntax.  
  
  (module m (v)
    
    (begin-for-syntax 
      (define-syntax u (lambda (form) (syntax 1))))
    
    (define-syntax v (lambda (form) (quasisyntax (g ,(+ (f) (u))))))
    (begin-for-syntax 
      (define (f) 2))
    
    (define (g n) (/ n))
    
    ) ; m
  
  (import m)
  
  (v)        ;==> 1/3
  
  
  
  ;; Matthew Flatt's example:
  
  (module records (define-record record-ref)
    
    (begin-for-syntax 
      (import syntax-case)
      
      (display "Creating fresh table")         ;==> Creating fresh table
      (newline)
      (define registry '())
      
      (define (register name fields)
        (if (assq name registry)
            (syntax-error "Duplicate record type: " name)) 
        (display "Registering: ") (display name)
        (newline)
        (set! registry (cons (cons name fields) registry)))
      
      ); begin-for-syntax
    
    (define-syntax define-record 
      (syntax-rules ()
        ((define-record name pred? field ...)
         (begin
           (begin-for-syntax 
             (register 'name '(field ...)))
           (define (pred? x) (and (pair? x) (eq? (car x) 'name)))
           (define (name field ...)
             (list 'name field ...))))))
    
    (define-syntax record-ref
      (lambda (form)
        (syntax-case form ()
          ((_ exp name field)
           (let ((entry (assq (syntax-object->datum (syntax name)) registry)))
             (if entry
                 (let ((maybe-member (member (syntax-object->datum (syntax field))
                                             entry)))
                   (if maybe-member
                       (with-syntax
                           ((index (- (length entry) (length maybe-member))))
                         (syntax
                          (list-ref exp index)))
                       (syntax-error "Unknown field" (syntax field))))
                 (syntax-error "Unknown record type" (syntax name))))))))
    ) ; records
  
  
  
  
  (module savannah (giraffe giraffe? lion lion?)
    (import records)                                     ;==> Creating fresh table  
    (define-record giraffe giraffe? height speed weight) ;==> Registering: giraffe
    (define-record lion    lion?    speed weight))       ;==> Registering: lion  
  
  
  (module metrics (weight)
    (import records)                                     ;==> Creating fresh table
    (import savannah)                                    ;==> Registering: giraffe
    ;==> Registering: lion 
    (define (weight animal)
      (cond ((lion? animal)    (record-ref animal lion weight))
            ((giraffe? animal) (record-ref animal giraffe weight)))))
  
  (module main ()
    (import savannah)                                    ;==> Creating fresh table
    ;==> Registering: giraffe
    ;==> Registering: giraffe
    (import metrics)
    (display (weight (giraffe 25 51 1000))))    
  
  (import main)                                          ;==> Creating fresh table
  ;==> Registering: giraffe
  ;==> Registering: giraffe
  ;==> 1000
  
  
  
  ;; If set-syntax! from SRFI 72 is provided, it may be used as follows
  ;; to implement interfaces and mutually recursive modules:
  
  (module interface-even (even)
    (define-syntax even (lambda (form) #f)))
  
  (module interface-odd (odd)
    (define-syntax odd (lambda (form) #f)))
  
  (module mod-even (even)
    (begin-for-syntax 
      (import syntax-case))
    (import interface-even)
    (import interface-odd)
    (set-syntax! even
                 (syntax-rules ()
                   ((even)         #t)
                   ((even x y ...) (odd y ...)))))
  
  (module mod-odd (odd)
    (begin-for-syntax 
      (import syntax-case))
    (import interface-even)
    (import interface-odd)
    (set-syntax! odd
                 (syntax-rules ()
                   ((odd)         #f)
                   ((odd x y ...) (even y ...)))))
  
  (import mod-even)
  (import mod-odd)
  
  (even a a a)   ;==> #f
  (odd  a a a)   ;==> #t
  
  
  
  
  ;; Macros may expand to modules. This allows one to implement
  ;; parametrized modules, as the following example shows:
  
  (module eager-kons (kons kar kdr)
    (define kons cons)
    (define kar car)
    (define kdr cdr))
  
  (module lazy-kons (kons kar kdr)
    (begin-for-syntax 
      (import syntax-case))
    
    (define-syntax kons
      (syntax-rules ()
        ((kons x y) (delay (cons x y)))))
    (define (kar x) (car (force x)))
    (define (kdr x) (cdr (force x))))
  
  (begin-for-syntax
    (import syntax-case))
  
  (define-syntax make-list-module
    (syntax-rules ()
      ((make-list-module name operations-module)
       (module name (kons kar kdr kadr)
         (import operations-module)
         (define (kadr x) (kar (kdr x)))))))
  
  (make-list-module eager-lists eager-kons)
  (make-list-module lazy-lists lazy-kons)
  
  (import eager-lists)
  
  (kons 1 (kons 2 3))                 ;==> (1 2 . 3)
  (kadr (kons 1 (kons 2 3)))          ;==> 2
  
  (import lazy-lists)
  
  (kons 1 (kons 2 3))                 ;==> PROMISE
  (kadr (kons 1 (kons 2 3)))          ;==> 2
  
  
  ))
  
  
  
