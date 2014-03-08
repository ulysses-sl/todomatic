;;;
;;; todo.scm : Tod-o-Matic
;;; Copyright 2014 Sak Lee <mail@saklee.net> Some rights reserved.
;;; Written in Chicken Scheme 4.8.0
;;;
;;;
;;;
;;; Display:
;;; -Split half, top for list and bottom for detail 
;;; -Menu on the top of the screen
;;;  ([Q]uit/[N]ew/[D]elete/[E]dit/[C]heck/[L]ater/[T]oggle)
;;; -Done? | Time | Date | Category 1 | Category 2 | Title
;;; -Auto-refresh every 5 minutes
;;; -Cursor on current item
;;;
;;; Save/Load:
;;; -Load ~/.todo/todo.data on start or create one if it does not exist
;;; -Save data on every edit
;;;
;;; Add/Delete/Edit/Defer:
;;; -Quick entry using the bottom half screen
;;;   -----EXAMPLE starts-----
;;;   - 20YR MTH DT DAY HR:MN CATEGORY1 CATEGORY2
;;;   - Title
;;;   - Detail...
;;;   -----EXAMPLE ends-----
;;; -Auto-complete for every text entry
;;;
;;;
;;;
;;; ---Implementation Detail---
;;; 
;;; Savefile format:
;;; ((todo entry1 entry2 ...) (done entry1 entry2 ...))
;;;
;;; Data entry format:
;;; (time-signature category1 category2 title detail)
;;; -1: utc-time object from srfi-19
;;; -2, 3, 4, 5: string object
;;;
;;; Auto-complete:
;;; -Weighted tries 
;;;  (char weight end-of-word next-letter-node ...)
;;; -Delete the auto-complete trie with delete key on suggestion
;;; -Save auto-complete trie when the entry is saved
;;;

(require-extension srfi-19)
(require-extension srfi-71)
(require-extension ncurses)
(require-extension directory-utils)
(use extras utils files srfi-1 srfi-13 srfi-14)
(import-for-syntax matchable)

;;;
;;; Utility Procedures
;;;

;;; General Macros
(define-syntax ++
  (syntax-rules ()
    ((++ x)
     (+ x 1))))

(define-syntax --
  (syntax-rules ()
    ((-- x)
     (- x 1))))

(define-syntax let-if
  (syntax-rules ()
    ((_ pred bind-t bind-f body ...)
     (if pred
       (let bind-t body ...)
       (let bind-f body ...)))))

(define-syntax let-if*
  (syntax-rules ()
    ((_ pred bind-t bind-f body ...)
     (if pred
       (let* bind-t body ...)
       (let* bind-f body ...)))))

(define-syntax let-cond
  (syntax-rules ()
    ((_ ((pred bind ...) . conds) body ...)
     (if pred
       (let (bind ...) (body ...))
       (let-cond conds body ...)))))


;;; Expiration checker. Use alone or with time-24, time-3d, and time-1w
(define-syntax expires-in?
  (syntax-rules ()
    ((_ entry)
      (time>? (current-time) (car entry)))
    ((_ entry time)
      (time>? (current-time) (subtract-duration (car entry) time)))))

;;; Remove current entry from data for deletion
(define (remove-entry data cursor)
  (cond
    ((null? data) '())
    ((= cursor 0) (cdr data))
    (else (cons (car data) (remove-entry (cdr data) (-- cursor))))))

;;; insert new or taken-out entry into data
(define (insert-entry data entry)
  (cond
    ((null? entry) data)
    ((null? data) (list entry))
    ((time<=? (car entry) (caar data)) (cons entry data))
    (else (cons (car data) (insert-entry (cdr data) entry)))))

;;; extract entry from the data and return entry with new data
(define (extract-entry data cursor)
  (cond ((null? data) (values '() '()))
        ((= cursor 0) (values (car data) (cdr data)))
        (else
          (receive (entry new-data) (extract-entry (cdr data) (-- cursor))
            (values entry (cons (car data) new-data))))))

(define (lookup-entry data cursor)
  (cond ((null? data) '())
        ((= cursor 0) (car data))
        (else (loopup-entry (cdr data) (-- cursor)))))

;;; Compare between entries for (sort lst less?) procedure
(define (earlier? entry1 entry2)
  (time<=? (car entry1) (car entry2)))

;;; Load data from file
(define (load-data)
  (if (file-exists? "~/.todo/todo.data")
    (let ((data (call-with-input-string (read-all "~/.todo/todo.data") read)))
      (values (car data) (cadr data) 0 0 'todopage))
    (values '() '() -1 -1 'todopage)))

;;; Save data from file
(define (save-data data)
  (begin (create-pathname-directory "~/.todo/")
         (delete-file* "~/.todo/todo.data")
         (call-with-output-file "~/.todo/todo.data"
           (lambda (file) (pretty-print data file)))))

;;;
;;; Constants
;;;

(define esc-char (integer->char 27))

(define time-24 (make-time time-duration 0 86400))

(define time-3d (make-time time-duration 0 259200))

(define time-1w (make-time time-duration 0 604800))

(define (color-pairs) ; To invoke color, use (COLOR_PAIR #)
                      ; or (bitwise-ior (COLOR_PAIR #) A_BOLD)
  (init_pair 1 COLOR_WHITE COLOR_BLACK)  ; normal text / entry
  (init_pair 2 COLOR_BLACK COLOR_WHITE)  ; hl entry

  (init_pair 3 COLOR_RED COLOR_BLACK)    ; 24 hr / expired / hl menu
  (init_pair 4 COLOR_WHITE COLOR_RED)    ; hl 24 hr
  (init_pair 5 COLOR_WHITE COLOR_MAGENTA); hl expired

  (init_pair 6 COLOR_YELLOW COLOR_BLACK) ; 3 day
  (init_pair 7 COLOR_WHITE COLOR_YELLOW) ; hl 3 day

  (init_pair 8 COLOR_GREEN COLOR_BLACK)  ; 1 wk
  (init_pair 9 COLOR_WHITE COLOR_GREEN)  ; hl 1 wk

  (init_pair 10 COLOR_WHITE COLOR_BLUE)  ; page indicator (todo/done)
  (init_pair 11 COLOR_BLACK COLOR_BLACK) ; menu/form
  (init_pair 12 COLOR_RED COLOR_WHITE)   ; title center O
  (init_pair 13 COLOR_BLUE COLOR_WHITE)) ; title "To-d-O-matic"

(define-syntax text-color
  (ir-macro-transformer
   (lambda (expr inject compare)
     (match expr
       ((_ color)
        `(lambda () (attrset (COLOR_PAIR ,color))))
       ((_ color bold)
        `(lambda () (attrset (bitwise-ior (COLOR_PAIR ,color) A_BOLD))))))))

;;;
;;; Ncurses procedure
;;;

; color scheme for texts
(define text-normal      (text-color 1))
(define text-title       (text-color 13))
(define text-title-O     (text-color 12 'bold))
(define text-menu        (text-color 11 'bold))
(define text-menu-hl     (text-color 3 'bold))
(define text-indicator   (text-color 10 'bold))
; entry colors scheme
(define text-entry       (text-color 1 'bold))
(define text-entry-hl    (text-color 2 'bold))
(define text-entry-xp    (text-color 3))
(define text-entry-xp-hl (text-color 5))
(define text-entry-24    (text-color 3 'bold))
(define text-entry-24-hl (text-color 4 'bold))
(define text-entry-3d    (text-color 6 'bold))
(define text-entry-3d-hl (text-color 7 'bold))
(define text-entry-1w    (text-color 8 'bold))
(define text-entry-1w-hl (text-color 9 'bold))

; Logo
(define todomatic
  (lambda ()
    (move 0 1)
    (text-title)
    (addstr "To-d-")
    (text-title-O)
    (addstr "O")
    (text-title)
    (addstr "-matic")
    (text-normal)))

; Menu item color
(define menu-item
  (lambda (str)
    (let* ((list-str (string->list str))
           (first-ch (car list-str))
           (rest-str (list->string (cdr list-str))))
      (begin
        (addstr " | ")
        (text-menu-hl)
        (addch first-ch)
        (text-menu)
        (addstr rest-str)
        (text-normal)))))

; Todo / done page indicator
(define toggle-indicator
  (lambda (current-page)
    (if (eqv? current-page 'todopage)
      (begin
        (addstr " ")
        (text-indicator)
        (addstr "todo")
        (text-menu)
        (addstr " | done")
        (text-normal))
      (begin
        (addstr " ")
        (text-menu)
        (addstr "todo | ")
        (text-indicator)
        (addstr "done")
        (text-normal)))))

; Split line after each part
(define (line-split line)
  (begin
    (move line 0)
    (text-menu)
    (let loop ((n (COLS)))
      (if (= n 0)
        (text-normal)
        (begin
          (addstr "-")
          (loop (-- n)))))))

(define (startwin)
  (begin
    (initscr)
    (start_color)
    (raw)
    (curs_set 0)
    (noecho)
    (keypad (stdscr) #t)
    (timeout 300000)
    (color-pairs)))

(define (draw-main todo done cursor top page)
  (if (eqv? page 'todopage)
    (define data todo)
    (define data done))
    (begin
      (text-normal)
      (clear)
      ; draw menu
      (todomatic)
      (menu-item "Quit")
      (menu-item "New")
      (menu-item "Edit")
      (menu-item "Delete")
      (menu-item "Later")
      (menu-item "Check")
      (menu-item "Toggle")
      (toggle-indicator page)
      (line-split 1)
      ; draw upper half
      (draw-upper-half data cursor top)
      (line-split (+ 3 (visible-lines)))
      ; draw lower-half
      ;(draw-lower-half (lookup-entry data cursor))
      (refresh)))

(define (visible-lines)
  (quotient (LINES) 2))

(define (truncate-pad str len)
  (letrec
    ((take-n
       (lambda (str-lst n acc)
         (cond ((= n 0)
                 (reverse-list->string acc))
               ((null? str-lst)
                 (take-n '() (-- n) (cons #\space acc)))
               (else
                 (take-n (cdr str-lst) (-- n) (cons (car str-lst) acc)))))))
    (take-n (string->list str) len '())))

(define (take-date-time time)
  (let ((time-string (date->string (time->date time))))
    (truncate-pad time-string 19)))

(define (display-entry entry on-cursor)
  (let ((date-text (take-date-time (car entry)))
        (category-1-text (truncate-pad (cadr entry) 8))
        (category-2-text (truncate-pad (caddr entry) 8))
        (title (truncate-pad (cadddr entry) (- (COLS) 48))))
    (begin
      (if on-cursor
        (cond ((expires-in? entry)
                (text-entry-xp-hl))
              ((expires-in? entry time-24)
                (text-entry-24-hl))
              ((expires-in? entry time-3d)
                (text-entry-3d-hl))
              ((expires-in? entry time-1w)
                (text-entry-1w-hl))
              (else
                (text-entry-hl)))
        (cond ((expires-in? entry)
                (text-entry-xp))
              ((expires-in? entry time-24)
                (text-entry-24))
              ((expires-in? entry time-3d)
                (text-entry-3d))
              ((expires-in? entry time-1w)
                (text-entry-1w))
              (else
                (text-entry))))
      (addstr " ")
      (addstr date-text)
      (addstr "    ")
      (addstr category-1-text)
      (addstr "    ")
      (addstr category-2-text)
      (addstr "    ")
      (addstr title))))

(define (draw-upper-half data cursor top)
  (letrec
    ((iterate-entries
             (lambda (data cursor top lines screen-pos)
               (cond
                 ((or (null? data) (> screen-pos (visible-lines)))
                   (refresh))
                 ((= cursor lines)
                   (begin
                     (move (+ screen-pos 2) 0)
                     (display-entry (car data) #t)
                     (iterate-entries (cdr data) cursor top (++ lines) (++ screen-pos))))
                 ((<= top lines)
                   (begin
                     (move (+ screen-pos 2) 0)
                     (display-entry (car data) #f)
                     (iterate-entries (cdr data) cursor top (++ lines) (++ screen-pos))))
                 (else
                   (iterate-entries (cdr data) cursor top (++ lines) screen-pos))))))
    (iterate-entries data cursor top 0 0)))

(define (new-entry-dialog data)
  (insert-entry data `(,(current-time) "categor1" "categor2" "title" "whatever lorem ipsum")))

(define (edit-entry-dialog data cursor)
  data)

(define (defer-entry-dialog data cursor)
  data)

(define (move-cursor data cursor top input)
  (cond
    ((and (= (char->integer input) KEY_DOWN) (= cursor (-- (length data))))
      (values 0 0))
    ((and (= (char->integer input) KEY_UP) (= cursor 0))
      (if (< (length data) (visible-lines))
        (values (-- (length data)) 0)
        (values (-- (length data)) (- (length data) (visible-lines)))))
    ((= (char->integer input) KEY_DOWN)
      (if (< cursor (-- (+ top (visible-lines))))
        (values (++ cursor) top)
        (values (++ cursor) (++ top))))
    (else
      (if (= cursor top)
        (values (-- cursor) (-- top))
        (values (-- cursor) top)))))

;;;
;;; Main program
;;;

;;; for capturing continuation
(define update #f)

;;; main function
(define (main)
  (startwin)
  (save-data
    (call/cc
      (lambda (save)
        (let ((todo done cursor top page
                (call/cc
                  (lambda (current-data)
                    (set! update current-data) (load-data)))))
          ; align cursor and top
          (if (eqv? page 'todopage)
            (if (> cursor (-- (length todo)))
              (set! cursor (-- (length todo))))
            (if (> cursor (-- (length done)))
              (set! cursor (-- (length done)))))
          (if (< cursor top) (set! top cursor))
          ; actual drawing
            (draw-main todo done cursor top page)
          ; then wait input to process
          (let ((input (getch)))
            (cond
              ; (Q)uit
              ((or (eqv? input #\Q) (eqv? input #\q))
                (save (list todo done))) 
              ; (N)ew
              ((or (eqv? input #\N) (eqv? input #\n))
                (update (new-entry-dialog todo) done 0 0 'todopage))
              ; (D)elete
              ((or (eqv? input #\D) (eqv? input #\d))
                (let-if (eqv? page 'todopage)
                  ((new-todo (remove-entry todo cursor)) (new-done done))
                  ((new-todo todo) (new-done (remove-entry done cursor)))
                  (update new-todo new-done cursor top page)))
              ; (E)dit
              ((or (eqv? input #\E) (eqv? input #\e))
                (let-if (eqv? page 'todopage)
                  ((new-todo (edit-entry-dialog todo cursor)) (new-done done))
                  ((new-todo todo) (new-done (edit-entry-dialog done cursor)))
                  (update new-todo new-done cursor top page)))
              ; (C)heck
              ((or (eqv? input #\C) (eqv? input #\c))
                (let-if* (eqv? page 'todopage)
                  ((entry new-todo (extract-entry todo cursor))
                   (new-done (insert-entry done entry)))
                  ((entry new-done (extract-entry done cursor))
                   (new-todo (insert-entry todo entry)))
                  (update new-todo new-done cursor top page)))
              ; (L)ater
              ((or (eqv? input #\L) (eqv? input #\l))
                (if (eqv? page 'todopage)
                  (let ((new-todo (defer-entry-dialog todo cursor)))
                    (update new-todo done cursor top page))
                  (update todo done cursor top page)))
              ; (T)oggle
              ((or (eqv? input #\T) (eqv? input #\t))
                (if (eqv? page 'todopage)
                  (update todo done 0 0 'donepage)
                  (update todo done 0 0 'todopage)))
              ; Up and Down arrow
              ((or (= (char->integer input) KEY_DOWN)
                   (= (char->integer input) KEY_UP))
                (let-if (eqv? page 'todopage)
                  ((new-cursor new-top (move-cursor todo cursor top input)))
                  ((new-cursor new-top (move-cursor done cursor top input)))
                  (update todo done new-cursor new-top page)))
              ; Refresh
              (else
                (update todo done cursor top page))))))))
  (endwin))

;;;
;;; Main stub
;;;

(main)
