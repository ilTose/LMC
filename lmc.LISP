;;;; Nome file: lmc.lisp
;;;; Progetto: Little Man Computer
;;;; Progetto realizzato da: Pozzoli Davide (829584) e Toselli Massimo Junior (830209)

;;; Architettura del LMC

;;; funzioni di supporto
(defun get-id (n)
  (floor n 100))

(defun get-xx(n)
  (rem n 100))

(defun repl(list xx e)
  (if (= xx 0)
      (append (list e) (cdr list))
    (append (list (car list)) (repl (cdr list) (- xx 1) e))))

(defun get-acc(State)
        (nth 2 State))

(defun get-pc(State)
        (nth 4 State))

(defun get-mem(State)
        (nth 6 State))

(defun get-in(State)
        (nth 8 State))

(defun get-out(State)
        (nth 10 State))

(defun get-flag(State)
        (nth 12 State))

(defun incr-pc (pc)
  (if (< pc 100)
      (+ pc 1)
    1))


(defun execute (id xx State)
  (cond
   ;; Add:
   ;; Esegui l�istruzione di addizione tra l�accumulatore e il valore
   ;; contenuto nella cella indicata da xx
   ((= id 1) (if (>= (+ (nth xx (get-mem State)) (get-acc State)) 1000)
                 (list 'state
                  :ACC (rem (+ (nth xx (get-mem State)) (get-acc State)) 1000)
                  :PC (incr-pc (get-pc state))
                  :MEM (get-mem State)
                  :IN (get-in State)
                  :OUT (get-out State)
                  :FLAG "flag")
                 (list 'state
                  :ACC (rem (+ (nth xx (get-mem State)) (get-acc State)) 1000)
                  :PC (incr-pc (get-pc state))
                  :MEM (get-mem State)
                  :IN (get-in State)
                  :OUT (get-out State)
                  :FLAG "noflag")))

   ;; Sub:
   ;; Esegui l�istruzione di sottrazione tra l�accumulatore e il
   ;; valore contenuto nella cella indicata da xx
   ((= id 2) (if (>= (- (get-acc State) (nth xx (get-mem State))) 0)
                 (list 'state
                  :ACC (rem (- (get-acc State) (nth xx (get-mem State))) 1000)
                  :PC (incr-pc (get-pc state))
                  :MEM (get-mem State)
                  :IN (get-in State)
                  :OUT (get-out State)
                  :FLAG "noflag")
                 (list 'state
                  :ACC (rem (- (get-acc State) (nth xx (get-mem State))) 1000)
                  :PC (incr-pc (get-pc state))
                  :MEM (get-mem State)
                  :IN (get-in State)
                  :OUT (get-out State)
                  :FLAG "flag")))

   ;; Store
   ;; Esegue una istruzione di store del valore dell�accumulatore
   ;; nella cella indicata da xx
   ((= id 3) (list 'state
              :ACC (get-acc State)
              :PC (incr-pc (get-pc state))
              :MEM (repl (get-mem State) xx (get-acc State))
              :IN (get-in State)
              :OUT (get-out State)
              :FLAG (get-flag State)))

   ;; Load
   ;; Esegue una istruzione di load dal valore contenuto nella
   ;; cella indicata da xx nell�accumulatore
   ((= id 5) (list 'state
              :ACC (nth xx (get-mem State))
              :PC (incr-pc (get-pc state))
              :MEM (get-mem State)
              :IN (get-in State)
              :OUT (get-out State)
              :FLAG (get-flag State)))

   ;; Branch
   ;; Esegue una istruzione di branch non condizionale al
   ;; valore indicato da xx
   ((= id 6) (list 'state
              :ACC (get-acc State)
              :PC xx
              :MEM (get-mem State)
              :IN (get-in State)
              :OUT (get-out State)
              :FLAG (get-flag State)))

   ;; Branch if 0
   ;; Esegue una istruzione di branch condizionale (se l�accumulatore � zero
   ;; e non vi � il flag acceso) al valore indicato da xx.
   ((= id 7) (if (and
                    (= (get-acc State) 0)
                    (string-equal (get-flag State) "noflag"))
                 (list 'state
                  :ACC (get-acc State)
                  :PC xx
                  :MEM (get-mem State)
                  :IN (get-in State)
                  :OUT (get-out State)
                  :FLAG (get-flag State))
                 (list 'state
                  :ACC (get-acc State)
                  :PC (incr-pc (get-pc state))
                  :MEM (get-mem State)
                  :IN (get-in State)
                  :OUT (get-out State)
                  :FLAG (get-flag State))))

   ;; Branch if positive
   ;; Esegue una istruzione di branch condizionale (se non vi � il flag acceso)
   ;; al valore indicato da xx.
   ((= id 8) (if (string-equal (get-flag State) "noflag")
                 (list 'state
                  :ACC (get-acc State)
                  :PC xx
                  :MEM (get-mem State)
                  :IN (get-in State)
                  :OUT (get-out State)
                  :FLAG(get-flag State))
                 (list 'state
                  :ACC (get-acc State)
                  :PC (incr-pc (get-pc state))
                  :MEM (get-mem State)
                  :IN (get-in State)
                  :OUT (get-out State)
                  :FLAG (get-flag State))))

   ;; Input
   ;; Esegue una istruzione di input
   ((and (= id 9) (= xx 01))
    (list 'state
     :ACC (car (get-in State))
     :PC (incr-pc (get-pc state))
     :MEM (get-mem State)
     :IN (cdr (get-in State))
     :OUT (get-out State)
     :FLAG (get-flag State)))

   ;; Output
   ;; Esegue una istruzione di output
   ((and (= id 9) (= xx 02))
    (list 'state
     :ACC (get-acc State)
     :PC (incr-pc (get-pc state))
     :MEM (get-mem State)
     :IN (get-in State)
     :OUT (append (get-out State) (list (get-acc State)))
     :FLAG (get-flag State)))

   ;; Halt
   ;; Esegue una istruzione di halt
   ((= id 0) (list 'halted-state
              :ACC (get-acc State)
              :PC (get-pc State)
              :MEM (get-mem State)
              :IN (get-in State)
              :OUT (get-out State)
              :FLAG (get-flag State)))))

;;; One_Instruction (State)
;;; ove State e NewState sono stati del LMC rappresentati come descritto
;;; sopra ed il predicato � vero quando l�esecuzione di una singola
;;; istruzione a partire da State porta allo stato NewState.
(defun one-instruction (state)
  (execute (get-id (nth (get-pc state) (get-mem state)))
            (get-xx (nth (get-pc state) (get-mem state)))
            state))

;;; Execution_loop(State)
;;; ove State rappresenta  lo  stato  iniziale  del  LMC  e Out la  coda
;;; di output nel momento in cui viene raggiunto uno stato di stop
;;; (e quindi eseguita una istruzione di halt). Il predicato deve fallire
;;; nel caso l�esecuzione termini senza eseguire una istruzione di halt
;;; (ad  esempio  se  si incontra una istruzione non valida).
(defun execution-loop (state)
  (if (eql (nth 0 state) 'halted-state)
      (get-out state)
    (execution-loop (one-instruction state))))

;;; ASSEMBLER

;;; funzioni di supporto

(defun gen-list (in)
  (cond
   ((eq (listen in) nil) ())
   ( t (append (list (read-line in)) (gen-list in)))))

;;; elimina spazi ai bordi e fa l'upcase
(defun clear-string (stringa)
 (string-trim " " (string-upcase stringa))
)

;;; separa le parole e le inserisce in una lista
(defun get-words (stringa)
  (if (string-equal (clear-string stringa) "")
      ()
    (append
     (list (write-to-string (read-from-string (clear-string stringa))))
     (get-words
      (string-left-trim
       (write-to-string (read-from-string (clear-string stringa) nil))
       (clear-string stringa))))))

;;; cicla la lista con le righe
(defun get-instruction (lista)
 (if (null lista)
     ()
 (append
  (list (get-words (car lista)))
  (get-instruction (cdr lista)))))

;;; delete comment
(defun find-remove (stringa)
  (subseq stringa 0 (position #\/ stringa))
)

(defun delete-comment (lista)
  (if (null lista)
      ()
    (append
     (list (find-remove (car lista)))
     (delete-comment (cdr lista)))))

;;; creazione lista label
(defun check-label (lista n)
  (cond
   ((= (length lista) 2) (cond
                          ((string-equal (second lista) "INP")
                            (append (list (first lista)) (list n)))
                          ((string-equal (second lista) "OUT")
                            (append (list (first lista)) (list n)))
                          ((string-equal (second lista) "HLT")
                            (append (list (first lista)) (list n)))
                          ((string-equal (second lista) "DAT")
                            (append (list (first lista)) (list n)))
                          (t ())))
   ((= (length lista) 3) (append (list (first lista)) (list n)))))

(defun assert-label (lista n)
  (cond ((null lista) ())
        ((null (car lista)) (assert-label (cdr lista) n))
        (t (append
            (list (check-label (car lista) n))
            (assert-label (cdr lista) (+ n 1))))))

;;; funzione che controlla la label
(defun exist-label (lista label)
  (cond
   ((null label) ())
   ((null lista) ())
   ((string-equal (car (car lista)) label)(second (car lista)))
   (t (exist-label (cdr lista) label))))

;;; sostituzione etichette con numeri corrispondenti
(defun get-ins (etichetta istruzioni istruzionitot)
  (cond
   ((null etichetta) ())
   ((= (length etichetta) 1) etichetta)
   ((= (length etichetta) 3) (get-ins
                              (list
                               (second etichetta)
                               (third etichetta))
                              istruzioni istruzionitot))
   ((= (length etichetta) 2)
      (cond
        ((numberp (exist-label istruzionitot (car etichetta)))
          (cdr etichetta))
        ((numberp (exist-label istruzionitot  (second etichetta)))
          (list
           (car etichetta)
           (write-to-string (exist-label istruzionitot  (second etichetta)))))
        ( t etichetta)))))

;;; trasforma le istruzioni assembly in codice macchina
(defun identify (lista)
(cond
 ((= (length lista) 1) (cond
                        ((string-equal (car lista) "HLT") 000)
                        ((string-equal (car lista) "INP") 901)
                        ((string-equal (car lista) "OUT") 902)
                        ((string-equal (car lista) "DAT") 0)))

 ((= (length lista) 2) (cond
                        ((string-equal (car lista) "ADD")
                          (+ 100 (parse-integer (second lista))))
                        ((string-equal (car lista) "SUB")
                          (+ 200 (parse-integer (second lista))))
                        ((string-equal (car lista) "STA")
                          (+ 300 (parse-integer (second lista))))
                        ((string-equal (car lista) "LDA")
                          (+ 500 (parse-integer (second lista))))
                        ((string-equal (car lista) "BRA")
                          (+ 600 (parse-integer (second lista))))
                        ((string-equal (car lista) "BRZ")
                          (+ 700 (parse-integer (second lista))))
                        ((string-equal (car lista) "BRP")
                          (+ 800 (parse-integer (second lista))))
                        ((string-equal (car lista) "DAT")
                          (parse-integer (second lista)))))))

;;; crea la lista memoria
(defun create-mem (istruzioni istruzionitot)
  (cond
   ((null istruzioni) ())
   ((null (car istruzioni)) (create-mem (cdr istruzioni) istruzionitot))
   (t (append
       (list (identify (get-ins (car istruzioni) istruzioni istruzionitot )))
       (create-mem (cdr istruzioni) istruzionitot)))))


;;; riempi memoria fino di 0 fino a 100 elementi
(defun fill-memory (mem)
  (append mem (fill-mem (- 100 (length mem)))))
(defun fill-mem (n)
  (if (= n 0)
      ()
    (append (fill-mem (- n 1)) (list 0))))

;;; apre il file
(defun open-file (file)
(let ((in (open file :if-does-not-exist nil)))
  (get-instruction (delete-comment (gen-list in)))))

;;; lmc_load (Filename)
;;; si preoccupa di leggere un file che contiene un codice assembler e
;;; che produce il contenuto �iniziale� della memoria sistema (una lista
;;; di 100 numeri tra 0 e 999).
(defun lmc-load (file)
   (fill-memory
    (create-mem(open-file file )
    (assert-label (open-file file) 0 ))))

;;; lmc_run (Filename, Input)
;;; si preoccupa di leggere un file che contiene un codice assembler, lo
;;; carica (con lmc_load), imposta la coda di input al valore fornito e
;;; produce un output che � il risultato dell�invocazione di execution_loop
(defun lmc-run (file input)
  (execution-loop
    (list 'state
     :ACC 0
     :PC 0
     :MEM (lmc-load file)
     :INPUT input
     :OUTPUT ()
     :FLAG "noflag")))



;;;; end of file