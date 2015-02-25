#lang racket
(require racket/tcp)
(define listener (tcp-listen 9020 4 #t))
(define king "Sheldon Cooper")
(define mypass "asdfasdf")
(define corypass "password")

;server loop
(let loop ()
  (define-values (in out) (tcp-accept listener))
  ;new thread for each client connected
  (thread 
   (lambda ()
     ;functions and thread-specific variables
     (define (read)
       (read-line in 'return-linefeed))
     (define (send msg)  
       (log msg)
       (write-string (string-append msg "\r\n") out)
       (flush-output out))
     (define (log msg)
       (printf "\nS: ~a" msg))
     (define name "unknown")
     (define buffer empty)
     (define (list-to-string list del)
       (cond
         [(empty? list) ""]
         [else (string-append (format "~a~a" (first list) del) (list-to-string (rest list) del))]))
     (define (get-range list s e i str)
       (cond
         [(empty? list) str]
         [(if (and (<= s i) (>= e i))
             (get-range (rest list) s e (+ i 1) (string-append str (first list)))             
             (get-range (rest list) s e (+ i 1) str))]))
     
     (display "\nNew Client connected")     
     (send "Welcome to Tanner's chat room")
     
     ;send/receive loop
     (let/ec done 
       ;runs for each line read
       (let loop ([buf buffer][n name])
         (define line (read))
         (printf "\nR: ~a" line)
         (when (eof-object? line)
           (set! line ""))                     
         (define args (string-split line))
         ;regex on the received string
         (match line 
           [(pregexp "^help$")         
            (send "help\ntest <words>\nname <name>\nget\npush <stuff>\ngetrange <startline> <endline>\ngetking\nsetking <king>\nadios\n")]
           [(pregexp "^test: .+$")            
            (send (string-trim (substring line 5)))]
           [(pregexp "^name: .+$")
            (send "OK")
            (loop buf (substring line 5))]
           [(pregexp "^get$") 
            (send (list-to-string (reverse buf) ""))]
           [(pregexp "^push: .+$")            
            (send "OK")
            (loop (cons (format "~a: ~a\n" n (string-trim (substring line 5))) buf) n)]
           [(pregexp "^getrange [0-9]+ [0-9]+$") 
            (send (get-range (reverse buf) (string->number (second args)) (string->number (third args)) 0 ""))]
           [(pregexp "^getking$") 
            (send king)]
           [(pregexp "^setking .+$") 
            (send (format "Hahaha, ~a still reigns!" king))]
           [(pregexp "^sudo setking .+ ([0-9]{1,3}.){3}[0-9]{1,3}")
            (send "setting...")
            (define-values (cory-in cory-out) (tcp-connect (string-trim (last args)) 9020))
            (write-string (format "kingme ~a ~a\r\n" (list-to-string (reverse (rest (reverse (rest args)))) " ") corypass) cory-out)
            (write-string "adios\r\n" cory-out)
            (flush-output cory-out)
            (close-input-port cory-in)
            (close-output-port cory-out)]
           [(pregexp (format "^kingme .+ ~a$" mypass))           
            (set! king (list-to-string (reverse (rest (reverse (rest args)))) " "))
            (done)]
           [(pregexp "^adios$") 
            (done)]
           [else 
            (send (string-append "Error: unrecognized command: " line))])
         (loop buf n)))
     ;on client close, end thread
     (display "\nClient disconnected")
     (close-input-port in)
     (close-output-port out)))(loop))
