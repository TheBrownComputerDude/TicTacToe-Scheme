#lang scheme
(define board '(- - - - - - - - -))
(define turn 0)
(define winner #f)

(define print-board (lambda (b) (if (null? b)
                                    (display "")
                                    (begin
                                      (display (car b))
                                      (unless (not (= (remainder (- (length b) 1) 3) 0))
                                        (newline))
                                      (print-board (cdr b))))))

(define do-turn (lambda (index) (if (= turn 0)
                                    (begin
                                      (set! board (append (get-left board index) '(X) (get-right board index)))
                                      (set! turn 1))
                                    (begin
                                      (set! board (append (get-left board index) '(O) (get-right board index)))
                                      (set! turn 0)))))

(define get-left (lambda(lst index)
                   (if (> index 0)
                       (cons (car lst) (get-left (cdr lst) (- index 1)))
                       '())))

(define get-right (lambda (lst index)
                    (if (null? lst)
                        '()
                        (if(< index 0)
                           (cons (car lst) (get-right (cdr lst) (- index 1)))
                           (get-right (cdr lst) (- index 1))))))

(define valid(lambda (lst index)
               (if (null? lst)
                    #f
                   (if (= index 0)
                       (if (eq? (car lst) (car '(-)))
                           #t
                           #f)
                       (valid (cdr lst) (- index 1)))
                  )))

(define full?(lambda (lst)
               (if (null? lst)
                   #t
                   (if (eq? (car lst) (car '(-)))
                       #f
                       (full? (cdr lst))))))

(define winner? (lambda (check)
                  (if (or (check-horizontal board check) (check-vertical board check 0) (check-diagonal check))
                      #t
                      #f)))

(define check-horizontal (lambda (lst check)
                           (if(null? lst)
                              #f
                              (if (and (eq? (car lst) check) (eq? (cadr lst) check)  (eq? (caddr lst) check))
                                  #t
                                  (check-horizontal (cdddr lst) check)))))

(define check-vertical (lambda (lst check i)
                         (if (> i 2)
                             #f
                             (if (and (eq? (car lst) check) (eq? (cadddr lst) check) (eq? (list-ref lst 6) check))
                                 #t
                                 (check-vertical (cdr lst) check (+ i 1))))))
(define check-diagonal (lambda (check)
                         (if (or (and (eq? (list-ref board 0) check) (eq? (list-ref board 4) check) (eq? (list-ref board 8) check))
                                 (and (eq? (list-ref board 2) check) (eq? (list-ref board 4) check) (eq? (list-ref board 6) check)))
                             #t
                             #f)))

(define i -1)
(define game-loop (lambda ()
                    (unless (eq? winner #t)
                        (begin
                          (print-board board)
                          (newline)
                          (display "index of where you want to play:")
                          (set! i (read))
                          (if(eq? (valid board i) #t)
                             (begin
                               (do-turn i)
                               (set! winner (winner? (if (= turn 0) (car '(O)) (car '(X)))))
                               (if (eq? winner #t)
                                   (if (= turn 0)
                                       (begin
                                         (print-board board)
                                         (display "Player 2 won"))
                                       (begin
                                         (print-board board)
                                         (display "Player 1 won")))
                                   (begin
                                    (set! winner (full? board))
                                    (if(eq? winner #t)
                                       (display "Tied game")
                                       (game-loop)))))
                               (begin
                                 (display "not a valid index")
                                 (newline)
                                 (game-loop)))))))
(game-loop)
                                    
                                     
                        
                        
                                      
                                   
                           
                   
                           
                       










                    
                                              
                                  