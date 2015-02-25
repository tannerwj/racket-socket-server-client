#lang racket
(require racket/tcp)

;client
(define (client host port)
  ;connect to server
  (define-values (in out) (tcp-connect host port))
  
  ;thread to read from server
  (define (read-loop)
    (define (read)
      (read-line in 'return-linefeed))
    (define line (read))
    (unless (eof-object? line)
      (display (format "~a\n" line))
      (read-loop)))
  
  (define rt (thread read-loop)) 
  
  ;loop for keyboard input, send to server
  (let loop ()
    (define (send msg) 
      (write-string (string-append (string-trim msg) "\r\n") out)
      (flush-output out))
    (define line (read-line))
    (send line)
    (cond
      ;if adios, close client
      [(equal? line "adios")       
       (kill-thread rt)
       (close-input-port in)
       (close-output-port out)]
      [else (loop)])))
      
(client "localhost" 9020)
