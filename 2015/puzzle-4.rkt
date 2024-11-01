#lang racket

(require racket/file)
(require file/md5)

(define input (file->string "4-input"))

(define (find-salt requested-string current)
  (let* ([full-message (string-append input (number->string current))]
         [hash (bytes->string/locale (md5 full-message))])
    (if (string-prefix? hash requested-string)
        current
        (find-salt requested-string (+ current 1)))))

(printf "Round 1: ~a~%" (find-salt "00000"  0))
(printf "Round 2: ~a~%" (find-salt "000000" 0))

;; Local Variables:
;; compile-command: "racket puzzle-4.rkt"
;; End:
