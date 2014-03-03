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
;;; ((todo entry1 entry2 ...) (done entry1 entry2) ())
;;;
;;; Data entry format:
;;; (time-signature category1 category2 title detail)
;;; -1: date object from srfi-19
;;; -2, 3, 4, 5: string object
;;;
;;; Auto-complete:
;;; -Weighted tries 
;;;  (char weight end-of-word next-letter-node ...)
;;; -Delete the auto-complete trie with delete key on suggestion
;;; -Save auto-complete trie when the entry is saved
;;;

(require-extension srfi-19)
(require-extension ncurses)
(require-extension directory-utils)
(use extras utils files srfi-1 srfi-13 srfi-14)

;;;
;;; Utility Procedures
;;;

(define (++ x) (+ x 1))
(define (-- x) (- x 1))

;;; Entry expired?
(define (expired? entry)
  (time<? (current-date) (car entry)))

;;; Entry within 24 hrs?
(define (in-24hour? entry)
  (time<? (current-date) (add-duration (car entry) duration-24hr)))

;;; Entry within 3 days?
(define (in-3days? entry)
  (time<? (current-date) (add-duration (car entry) duration-3day)))

;;; Entry within 1 week?
(define (in-1week? entry)
  (time<? (current-date) (add-duration (car entry) duration-1wk)))

;;; Remove current entry from data for deletion
(define (remove-entry data cursor)
  (cond
    ((null? data) '())
    ((= cursor 0) (cdr data))
    (else (cons (car data) (remove-data (cdr data) (-- cursor))))))

;;; insert new or taken-out entry into data
(define (insert-entry data entry)
  (cond
    ((null? entry) data)
    ((null? data) (list entry))
    ((time<=? (car entry) (caar data)) (cons entry data))
    (else (cons (car data) (insert-data (cdr data) entry)))))

;;; extract entry from the data
(define (extract-entry data cursor)
  (cond ((null? data) (values '() '()))
        ((= cursor 0) (values (car data) (cdr data)))
        (else
          (receive (entry new-data)
                   (extract-entry (cdr data) (-- cursor))
                   (values entry (cons (car data) new-data))))))

;;; Compare between entries for (sort lst less?) procedure
(define (earlier? entry1 entry2)
  (time<=? (car entry1) (car entry2)))

;;; Load data from file
(define (load-data)
  (if (file-exists? "~/.todo/todo.data")
    (let ((data (call-with-input-string (read-all "~/.todo/todo.data") read)))
      (values (car data) (cadr data)))
    (values '() '())))

;;; Save data from file
(define (save-data data)
  (begin (create-pathname-directory "~/.todo/")
         (delete-file* "~/.todo/todo.data")
         (call-with-output-file "~/.todo/todo.data"
           (lambda (file) (display data file)))))

;;;
;;; Constants
;;;

(define esc-char (integer->char 27))

(define duration-24hr (make-time time-duration 0 86400))

(define duration-3day (make-time time-duration 0 259200))

(define duration-1wk (make-time time-duration 0 604800))

(define (color-pairs) ; To invoke color, use (COLOR_PAIR #) or (bitwise-ior (COLOR_PAIR #) A_BOLD)
  (init_pair 1 COLOR_WHITE COLOR_BLACK)  ; normal text / entry
  (init_pair 2 COLOR_WHITE COLOR_BLUE)   ; hl entry

  (init_pair 3 COLOR_RED COLOR_BLACK)    ; 24 hr / expired / hl menu
  (init_pair 4 COLOR_RED COLOR_BLUE)     ; hl 24 hr / expired

  (init_pair 5 COLOR_YELLOW COLOR_BLACK) ; 3 day
  (init_pair 6 COLOR_YELLOW COLOR_BLUE)  ; hl 3 day

  (init_pair 7 COLOR_GREEN COLOR_BLACK)  ; 1 wk
  (init_pair 8 COLOR_GREEN COLOR_BLUE)   ; hl 1 wk

  (init_pair 9 COLOR_BLACK COLOR_BLUE)   ; page indicator (todo/done)
  (init_pair 10 COLOR_BLACK COLOR_BLACK) ; menu/form
  (init_pair 11 COLOR_RED COLOR_WHITE)   ; title center O
  (init_pair 12 COLOR_BLUE COLOR_WHITE)) ; title "To-d-O-matic"

;;;
;;; Ncurses procedure
;;;

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
  (let* ((text-normal
           (lambda () (attrset (COLOR_PAIR 1))))
         (text-title
           (lambda () (attrset (COLOR_PAIR 12))))
         (text-title-O
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 11) A_BOLD))))
         (text-menu
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 10) A_BOLD))))
         (text-menu-hl
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 3) A_BOLD))))
         (text-indicator
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 2) A_BOLD))))
         ; below are entry colors
         (text-entry
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 1) A_BOLD))))
         (text-entry-hl
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 2) A_BOLD))))
         (text-entry-xp
           (lambda () (attrset (COLOR_PAIR 3))))
         (text-entry-xp-hl
           (lambda () (attrset (COLOR_PAIR 4))))
         (text-entry-24
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 3) A_BOLD))))
         (text-entry-24-hl
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 4) A_BOLD))))
         (text-entry-3d
           (lambda () (attrset (bitwise-ior (COLOR_PAIR 5) A_BOLD))))
         (text-entry-3d-hl
           ((lambda () attrset (bitwise-ior (COLOR_PAIR 6) A_BOLD))))
         (text-entry-1w
           ((lambda () attrset (bitwise-ior (COLOR_PAIR 7) A_BOLD))))
         (text-entry-1w-hl
           ((lambda () attrset (bitwise-ior (COLOR_PAIR 8) A_BOLD))))
         ; shortcuts
         (todomatic
           (lambda ()
             (move 0 1)
             (text-title)
             (addstr "To-d-")
             (text-title-O)
             (addstr "O")
             (text-title)
             (addstr "-matic")
             (text-normal)))
         (menu-item
           (lambda (str)
             (let* ((list-str (string->list str))
                    (first-ch (car list-str))
                    (rest-str (list->string (cdr list-str))))
               (addstr " | ")
               (text-menu-hl)
               (addch first-ch)
               (text-menu)
               (addstr rest-str)
               (text-normal))))
         (toggle-indicator
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
         (line-split
           (lambda ()
             (move 1 0)
             (text-menu)
             (let loop ((n (COLS)))
                 (if (= n 0)
                   (text-normal)
                   (begin
                     (addstr "-")
                     (loop (-- n))))))))
    ; draw menu
    (begin
      (text-normal)
      (clear)
      (todomatic)
      (menu-item "Quit")
      (menu-item "New")
      (menu-item "Edit")
      (menu-item "Delete")
      (menu-item "Later")
      (menu-item "Check")
      (menu-item "Toggle")
      (toggle-indicator page)
      (line-split)
      (refresh))))

(define (visible-lines)
  (quotient (LINES) 2))

(define (new-entry-dialog data)
  data)

(define (edit-entry-dialog data cursor)
  data)

(define (defer-entry-dialog data cursor)
  data)

(define (move-cursor data cursor top input)
  (cond ((= (length data) 0)
          (values -1 -1))
        ((= input KEY_UP)
          (cond ((= cursor 0)
                  (values 0 0))
                ((= cursor top)
                  (values (-- cursor) (-- top)))
                (else
                  (values (-- cursor) top))))
        (else ; (= input KEY_DOWN)
          (cond ((<= (length data) (visible-lines))
                  (if (< cursor (-- (length data)))
                    (values (++ cursor) 0)
                    (values (-- (length data)) 0)))
                ((= cursor (-- (+ top (visible-lines))))
                  (if (= cursor (-- (length data)))
                    (values (-- (length data)) (- (length data) (visible-lines)))
                    (values (++ cursor) (++ top))))
                (else
                  (values (++ cursor) top))))))

;;;
;;; Main program
;;;

(define (main)
  (startwin)
  (save-data
    (call/cc
      (lambda (save)
        (receive (list-todo list-done) (load-data)
          (let loop ((todo list-todo)  ; list of items to do
                     (done list-done)  ; list of finished items
                     (cursor 0) (top 0); current item and portion of list shown. (-1, -1) when empty
                     (page 'todopage)) ; either 'todopage or 'donepage
            (if (eqv? page 'todopage)  ; if cursor is out of place, put it on the last of the list
              (if (> cursor (- (length todo) 1)) (set! cursor (- (length todo) 1)))
              (if (> cursor (- (length done) 1)) (set! cursor (- (length done) 1))))
            (if (< cursor top) (set! top cursor))
            (draw-main todo done cursor top page)
            (let ((input (getch)))
              (cond
                ((or (eqv? input #\Q) (eqv? input #\q)) ; Quit
                  (save (list todo done))) 
                ((or (eqv? input #\N) (eqv? input #\n)) ; New
                  (loop (new-entry-dialog todo) done 0 0 'todopage))
                ((or (eqv? input #\D) (eqv? input #\d)) ; Delete
                  (if (eqv? page 'todopage)
                    (loop (remove-entry todo cursor) done cursor top page)
                    (loop todo (remove-entry done cursor) cursor top page)))
                ((or (eqv? input #\E) (eqv? input #\e)) ; Edit
                  (if (eqv? page 'todopage)
                    (loop (edit-entry-dialog todo cursor) done cursor top page)
                    (loop todo (edit-entry-dialog done cursor) cursor top page)))
                ((or (eqv? input #\C) (eqv? input #\c)) ; Check
                  (if (eqv? page 'todopage)
                    (receive (entry new-todo) (extract-entry todo cursor)
                             (loop new-todo (insert-entry done entry) cursor top page))
                    (receive (entry new-done) (extract-entry done cursor)
                             (loop (insert-entry todo cursor) new-done cursor top page))))
                ((or (eqv? input #\L) (eqv? input #\l)) ; Later
                  (if (eqv? page 'todopage)
                    (loop (defer-entry-dialog todo cursor) done cursor top page)
                    (loop todo done cursor top page)))
                ((or (eqv? input #\T) (eqv? input #\t)) ; Toggle
                  (if (eqv? page 'todopage)
                    (loop todo done 0 0 'donepage)
                    (loop todo done 0 0 'todopage)))
                ((or (= (char->integer input) KEY_DOWN) (= (char->integer input) KEY_UP))
                      (receive (ncursor ntop)
                               (if (eqv? page 'todopage)
                                 (move-cursor todo cursor top input)
                                 (move-cursor done cursor top input))
                               (loop todo done ncursor ntop page)))
                (else
                      (loop todo done cursor top page)))))))))
  (endwin))

;;;
;;; Main stub
;;;

(main)
