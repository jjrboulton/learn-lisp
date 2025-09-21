;;; A simple, interactive Common Lisp curriculum guide.

;;; Declare our own package to avoid polluting the user package.
(defpackage :lisp-curriculum
  (:use :common-lisp)
  (:export :start))

(in-package :lisp-curriculum)

;;; -----------------------------------------------------------------------------
;;; Data Structure - The Curriculum
;;; We represent everything as a plist for simplicity.

(defparameter *curriculum*
  '(:phases
    ((:name "Phase 1: Lisp Bootcamp"
      :lessons ((:id l1 :title "The REPL is Your Best Friend")
                (:id l2 :title "S-Expressions and Prefix Notation")
                (:id l3 :title "Basic Data Types")
                (:id l4 :title "The Core Data Structure: The Linked List")
                (:id l5 :title "Other Key Data Structures")
                (:id l6 :title "Defining Variables")
                (:id l7 :title "Defining Functions"))
      :challenges ((:id c1-1 :title "REPL Exploration")
                   (:id c1-2 :title "List Manipulation I (my-length)")
                   (:id c1-3 :title "List Manipulation II (my-reverse)")
                   (:id c1-4 :title "Data Structures (Hash Table)")
                   (:id c1-5 :title "Fibonacci")))

     (:name "Phase 2: Functional Programming and Control Flow"
      :lessons ((:id l8  :title "First-Class Functions")
                (:id l9  :title "Key Higher-Order Functions")
                (:id l10 :title "The `loop` Macro")
                (:id l11 :title "Other Iteration: `dolist`, `dotimes`, `do`")
                (:id l12 :title "Conditionals")
                (:id l13 :title "Equality Predicates")
                (:id l14 :title "Local Variables: `let`, `let*`"))
      :challenges ((:id c2-1 :title "Higher-Order Practice (squares)")
                   (:id c2-2 :title "Filtering (evens)")
                   (:id c2-3 :title "`loop` Mastery (sum, factorial, alist)")
                   (:id c2-4 :title "QuickSort"))))))

;;; -----------------------------------------------------------------------------
;;; Core Helper Functions (The "Engine")

(defun get-phase (phase-num)
  "Retrieve a phase by its number (1-indexed). Returns NIL if not found."
  (nth (1- phase-num) (getf *curriculum* :phases)))

(defun get-lesson (phase lesson-id)
  "Find a lesson by its ID within a given phase plist."
  (find lesson-id (getf phase :lessons) :key (lambda (x) (getf x :id))))

(defun get-challenge (phase challenge-id)
  "Find a challenge by its ID within a given phase plist."
  (find challenge-id (getf phase :challenges) :key (lambda (x) (getf x :id))))

(defun print-menu-header (title)
  "Print a consistent header for menus."
  (terpri)
  (format t "~%~a~%" title)
  (format t "~v,,,'-<~>~%" (length title))
  (terpri))

(defun list-phases ()
  "Print a list of all available phases."
  (print-menu-header "LISPCURRICULUM - PHASES")
  (let ((phases (getf *curriculum* :phases)))
    (loop for phase in phases
          for i from 1
          do (format t "~d. ~a~%" i (getf phase :name)))))

(defun list-phase-contents (phase-num)
  "Print the lessons and challenges for a specific phase."
  (let ((phase (get-phase phase-num)))
    (if phase
        (progn
          (print-menu-header (format nil "PHASE ~d: ~a" phase-num (getf phase :name)))
          (format t "Lessons:~%")
          (loop for lesson in (getf phase :lessons)
                do (format t "  ~a - ~a~%" (getf lesson :id) (getf lesson :title)))
          (terpri)
          (format t "Challenges:~%")
          (loop for challenge in (getf phase :challenges)
                do (format t "  ~a - ~a~%" (getf challenge :id) (getf challenge :title))))
        (format t "Error: Phase ~a not found.~%" phase-num))))

;;; -----------------------------------------------------------------------------
;;; Lesson and Challenge Display Functions

(defun show-lesson (lesson-id)
  "Display the content for a specific lesson. This is a stub that would be filled with real content."
  (format t "~%~%Showing lesson: ~a~%" lesson-id)
  (format t "========================================~%")
  ;; In a real application, this would fetch text from a database or file.
  (format t "This is where the detailed lesson content for '~a' would be.~%~%" lesson-id)
  (format t "It would explain the concepts in detail with examples.~%")
  (format t "Type (start) to return to the main menu.~%"))

(defun show-challenge (challenge-id)
  "Display the instructions for a specific challenge."
  (format t "~%~%Challenge: ~a~%" challenge-id)
  (format t "========================================~%")
  ;; In a real application, this would fetch text from a database or file.
  (format t "This is where the challenge instructions for '~a' would be.~%~%" challenge-id)
  (format t "**Instructions:**~%")
  (format t "Write a function that...~%")
  (format t "Your function will be tested.~%~%")
  (format t "You can now enter your code at the prompt.~%")
  (format t "Enter a form (e.g., a function definition) and press enter.~%")
  (format t "Type 'submit' when you are done to run the tests.~%")
  (format t "Type 'giveup' to abandon this challenge.~%"))

;;; -----------------------------------------------------------------------------
;;; Challenge Evaluation Functions
;;; Each challenge has its own tester function.

(defun test-repl-exploration (user-code)
  (declare (ignore user-code))
  (format t "This challenge is about experimenting in the REPL.~%")
  (format t "If you understand the concepts, you pass!~%")
  'pass)

(defun test-my-length (user-code)
  "Test if USER-CODE is a correct definition of MY-LENGTH."
  (let ((test-lists '(() (a) (a b c) (a b (c d)))))
    (handler-case
        (progn
          (eval user-code) ; Define the function
          (if (loop for list in test-lists
                    always (equal (funcall 'my-length list) (length list)))
              (progn (format t "All tests passed! Well done.~%") 'pass)
              (progn (format t "Some tests failed. Check your logic.~%") 'fail)))
      (error (e) (format t "Error evaluating your code: ~a~%" e) 'fail))))

(defun test-my-reverse (user-code)
  "Test if USER-CODE is a correct definition of MY-REVERSE."
  (let ((test-lists '(() (a) (a b c) (1 2 3 4))))
    (handler-case
        (progn
          (eval user-code) ; Define the function
          (if (loop for list in test-lists
                    always (equal (funcall 'my-reverse list) (reverse list)))
              (progn (format t "All tests passed! Well done.~%") 'pass)
              (progn (format t "Some tests failed. Check your logic.~%") 'fail)))
      (error (e) (format t "Error evaluating your code: ~a~%" e) 'fail))))

(defun test-hash-table (user-code)
  (declare (ignore user-code))
  (format t "Tests for hash table challenge would run here.~%")
  'pass) ; Stub

(defun test-fibonacci (user-code)
  "Test if USER-CODE is a correct definition of FIBONACCI."
  (let ((test-cases '((0 0) (1 1) (2 1) (3 2) (4 3) (5 5) (6 8))))
    (handler-case
        (progn
          (eval user-code) ; Define the function
          (if (loop for (n expected) in test-cases
                    always (equal (funcall 'fibonacci n) expected))
              (progn (format t "All tests passed! Well done.~%") 'pass)
              (progn (format t "Some tests failed. Check your logic.~%") 'fail)))
      (error (e) (format t "Error evaluating your code: ~a~%" e) 'fail))))

(defun run-challenge-tests (challenge-id user-code)
  "Determine which test function to run based on the challenge ID."
  (format t "~%Running tests for ~a...~%" challenge-id)
  (funcall (ecase challenge-id
             (c1-1 #'test-repl-exploration)
             (c1-2 #'test-my-length)
             (c1-3 #'test-my-reverse)
             (c1-4 #'test-hash-table)
             (c1-5 #'test-fibonacci))
           user-code))

;;; -----------------------------------------------------------------------------
;;; Main REPL Loop & Input Handler

(defun challenge-loop (challenge-id)
  "A inner loop for a user to input code for a specific challenge."
  (let (user-code)
    (loop do
      (format t "~&challenge(~a)> " challenge-id)
      (force-output)
      (let ((input (read)))
        (cond
          ((equal input 'giveup)
           (format t "Returning to main menu. You can try again later!~%")
           (return-from challenge-loop))
          ((equal input 'submit)
           (if user-code
               (let ((result (run-challenge-tests challenge-id user-code)))
                 (if (eq result 'pass)
                     (progn
                       (format t "Challenge passed! Congratulations!~%")
                       (return-from challenge-loop))
                     (format t "Keep working on it. You can modify your code and submit again.~%")))
               (format t "No code submitted yet. Enter your code first.~%")))
          (t
           (setf user-code input) ; Store the user's code
           (format t "Code recorded. Type 'submit' to test or 'giveup' to quit.~%")))))))

(defun handle-challenge-input ()
  "Helper function to get a challenge ID and then manage the challenge session."
  (format t "Challenge ID (e.g., C1-1): ")
  (force-output)
  (let ((challenge-id (read)))
    (show-challenge challenge-id)
    (challenge-loop challenge-id)))

(defun main-loop ()
  "The main read-eval-print loop for the curriculum manager."
  (loop do
    (format t "~&curriculum> ")
    (force-output)
    (let ((input (read)))
      (cond
        ((or (equal input 'quit) (equal input 'exit)) 
         (return-from main-loop (format t "Goodbye!~%")))
        ((equal input 'list-phases) 
         (list-phases))
        ((and (listp input) (equal (first input) 'list-phase) (numberp (second input)))
         (list-phase-contents (second input)))
        ((and (listp input) (equal (first input) 'show-lesson))
         (show-lesson (second input)))
        ((and (listp input) (equal (first input) 'show-challenge))
         (show-challenge (second input))
         (challenge-loop (second input)))
        (t (format t "Unknown command. Try list-phases, (list-phase 1), (show-lesson L1), (show-challenge C1-1), or quit.~%"))))))

(defun start ()
  "The main entry point for the interactive curriculum."
  (print-menu-header "WELCOME TO THE COMMON LISP CURRICULUM")
  (format t "Your journey to Lisp mastery begins.~%~%")
  (format t "Available commands:~%")
  (format t "  list-phases           - List all phases~%")
  (format t "  (list-phase N)        - List contents of phase number N~%")
  (format t "  (show-lesson ID)      - Show a lesson (e.g., (show-lesson L1))~%")
  (format t "  (show-challenge ID)   - Start a challenge (e.g., (show-challenge C1-1))~%")
  (format t "  quit                  - Exit the program~%~%")
  (format t "You are always in control. Type a command or experiment in the REPL.~%")
  (terpri)
  (main-loop))

;; Start the curriculum when the file is loaded
(start)
