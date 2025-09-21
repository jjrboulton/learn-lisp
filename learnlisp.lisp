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
;;; Lesson Content Database
;;; Each lesson has structured content with sections

(defparameter *lesson-content* 
  '((l1 :title "The REPL is Your Best Friend"
     :sections 
     ((:type :theory
       :title "What is the REPL?"
       :content "The REPL (Read-Eval-Print Loop) is the heart of Lisp development.
Unlike compiled languages where you write code, compile, and run,
Lisp lets you interact with your program as you build it.

REPL stands for:
• READ: Takes your input and parses it into Lisp data structures
• EVAL: Evaluates the parsed expression
• PRINT: Shows you the result
• LOOP: Repeats the process

This immediate feedback makes Lisp incredibly powerful for exploration!")

      (:type :example
       :title "Basic REPL Interactions"
       :content "Try these examples in the REPL:

> 42
42

> (+ 2 3)
5

> (* (+ 2 3) 4)
20

> 'hello
HELLO

Each expression gets evaluated immediately!")

      (:type :exercise
       :title "Your Turn: Basic Arithmetic"
       :prompt "Calculate the following expressions in the REPL:"
       :tasks ("(+ 10 (* 3 4))" 
               "(/ 100 (+ 2 3))"
               "(- (* 6 7) 2)")
       :expected-results (22 20 40))

      (:type :theory
       :title "Everything is Data"
       :content "In Lisp, code and data have the same structure - lists!
This is called 'homoiconicity' and it's Lisp's superpower.

(+ 2 3) is a list with three elements:
- The symbol +
- The number 2  
- The number 3

When evaluated, Lisp calls the function + with arguments 2 and 3.")

      (:type :exercise
       :title "Exploring Symbols and Evaluation"
       :prompt "Try these and observe what happens:"
       :tasks ("'+" "'(+ 2 3)" "(quote (+ 2 3))" "(eval '(+ 2 3))")
       :note "Notice the difference between quoted and unquoted expressions!")))

    (l2 :title "S-Expressions and Prefix Notation"
     :sections
     ((:type :theory
       :title "S-Expressions: The Universal Syntax"
       :content "S-expressions (symbolic expressions) are Lisp's uniform syntax.
Everything in Lisp is either an atom or a list:

ATOMS: 42, hello, +, nil, t
LISTS: (), (a), (a b c), (+ 2 3), ((nested) (lists))

This uniform structure makes Lisp code easy to parse and manipulate.")

      (:type :example  
       :title "Prefix Notation in Action"
       :content "Most languages use infix: 2 + 3 * 4
Lisp uses prefix: (+ 2 (* 3 4))

Advantages of prefix:
• No operator precedence confusion
• Easy to extend to multiple arguments: (+ 1 2 3 4 5)
• Uniform structure for all operations")

      (:type :exercise
       :title "Converting Infix to Prefix"
       :prompt "Convert these mathematical expressions to Lisp prefix form:"
       :tasks ("2 + 3 * 4" "(5 + 6) / (2 - 1)" "sqrt(16) + 2^3")
       :solutions ("(+ 2 (* 3 4))" "(/ (+ 5 6) (- 2 1))" "(+ (sqrt 16) (expt 2 3))"))))

    ))

;;; -----------------------------------------------------------------------------
;;; Challenge Content Database

(defparameter *challenge-content*
  '((c1-1 :title "REPL Exploration" 
     :difficulty :beginner
     :description "This challenge tests your comfort with the REPL environment.
You'll explore basic Lisp expressions and discover how evaluation works."
     :instructions "Complete these tasks in the REPL:
1. Evaluate some arithmetic expressions
2. Experiment with quoting
3. Try some built-in functions"
     :type :exploration
     :tasks (("Evaluate: (+ (* 2 3) (/ 12 4))" 9)
             ("What does 'hello return?" hello)
             ("Try: (length '(a b c d))" 4)
             ("Experiment: (reverse '(1 2 3 4))" (4 3 2 1)))
     :completion-check :manual)

    (c1-2 :title "List Manipulation I (my-length)"
     :difficulty :beginner  
     :description "Implement your own version of the LENGTH function.
This challenge introduces recursion and list processing."
     :instructions "Write a function MY-LENGTH that counts elements in a list.

Examples:
(my-length '()) => 0
(my-length '(a)) => 1  
(my-length '(a b c)) => 3

Hint: Think recursively! A list is either empty or has a first element 
and a rest. Use (first list) and (rest list)."
     :type :coding
     :test-function test-my-length
     :starter-code "(defun my-length (lst)
  ;; Your code here
  )")

    (c1-3 :title "List Manipulation II (my-reverse)" 
     :difficulty :intermediate
     :description "Implement your own REVERSE function.
This builds on list processing skills and introduces accumulator patterns."
     :instructions "Write MY-REVERSE to reverse a list without using the built-in REVERSE.

Examples:
(my-reverse '()) => ()
(my-reverse '(a)) => (a)
(my-reverse '(a b c)) => (c b a)

Try two approaches:
1. Simple recursion: (append (my-reverse (rest lst)) (list (first lst)))
2. Tail recursion with an accumulator"
     :type :coding
     :test-function test-my-reverse
     :starter-code "(defun my-reverse (lst)
  ;; Your code here
  )

;; Optional: tail-recursive version
(defun my-reverse-tr (lst &optional acc)
  ;; Your code here
  )")

    (c1-5 :title "Fibonacci"
     :difficulty :intermediate
     :description "The classic Fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, 13...
Each number is the sum of the two preceding ones."
     :instructions "Write a function FIBONACCI that returns the nth Fibonacci number.
fibonacci(0) = 0, fibonacci(1) = 1, fibonacci(n) = fibonacci(n-1) + fibonacci(n-2)

Try both recursive and iterative approaches. Which is faster for large n?"
     :type :coding  
     :test-function test-fibonacci
     :starter-code "(defun fibonacci (n)
  ;; Your code here
  )

;; Bonus: iterative version
(defun fibonacci-iter (n)
  ;; Your code here
  )")))

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
;;; Lesson Display Functions

(defun get-lesson-content (lesson-id)
  "Retrieve the content for a specific lesson."
  (find lesson-id *lesson-content* :key (function first)))

(defun show-lesson (lesson-id)
  "Display an interactive lesson with structured content."
  (let ((lesson (get-lesson-content lesson-id)))
    (if lesson
        (progn
          (print-menu-header (format nil "LESSON ~a: ~a" 
                                     (string lesson-id) 
                                     (getf (rest lesson) :title)))
          (show-lesson-sections (getf (rest lesson) :sections))
          (format t "~%Lesson complete! Type 'quit' to return to main menu.~%")
          (lesson-interaction-loop lesson-id))
        (format t "Lesson ~a not found. Available lessons: L1, L2, etc.~%" lesson-id))))

(defun show-lesson-sections (sections)
  "Display all sections of a lesson."
  (loop for section in sections
        for section-num from 1 do
    (format t "~%~%=== Section ~d: ~a ===~%" 
            section-num (getf section :title))
    (case (getf section :type)
      (:theory (show-theory-section section))
      (:example (show-example-section section))
      (:exercise (show-exercise-section section)))))

(defun show-theory-section (section)
  "Display a theory/reading section."
  (format t "~%~a~%" (getf section :content))
  (format t "~%[Press ENTER to continue]")
  (read-line)
  (read-line))

(defun show-example-section (section)
  "Display an example section."
  (format t "~%~a~%" (getf section :content))
  (format t "~%[Press ENTER to continue]")
  (read-line)
  (read-line))

(defun show-exercise-section (section)
  "Display an interactive exercise section."
  (format t "~%~a~%" (getf section :prompt))
  (when (getf section :note)
    (format t "~%NOTE: ~a~%" (getf section :note)))
  
  (let ((tasks (getf section :tasks))
        (expected (getf section :expected-results))
        (solutions (getf section :solutions)))
    
    (loop for task in tasks
          for i from 0 do
      (format t "~%Try: ~a~%" task)
      (format t "curriculum-exercise> ")
      (force-output)
      (let ((user-input (read)))
        (format t "Result: ~a~%" 
                (handler-case (eval user-input)
                  (error (e) (format nil "Error: ~a" e))))
        
        (when (and expected (< i (length expected)))
          (format t "Expected: ~a~%" (nth i expected)))
        
        (when (and solutions (< i (length solutions)))
          (format t "Solution: ~a~%" (nth i solutions))))))
  
  (format t "~%Exercise complete!~%"))

(defun lesson-interaction-loop (lesson-id)
  "Allow continued interaction after lesson completion."
  (format t "~%You can continue experimenting here, or type 'quit' to return.~%")
  (loop do
    (format t "~&lesson(~a)> " lesson-id)
    (force-output)
    (let ((input (read)))
      (cond
        ((equal input 'quit) (return-from lesson-interaction-loop))
        (t (format t "~a~%" 
                   (handler-case (eval input)
                     (error (e) (format nil "Error: ~a" e)))))))))

;;; -----------------------------------------------------------------------------
;;; Challenge Display and Management

(defun get-challenge-content (challenge-id)
  "Retrieve the content for a specific challenge."
  (find challenge-id *challenge-content* :key (function first)))

(defun show-challenge (challenge-id)
  "Display detailed challenge information and instructions."
  (let ((challenge (get-challenge-content challenge-id)))
    (if challenge
        (let ((props (rest challenge)))
          (print-menu-header (format nil "CHALLENGE ~a: ~a" 
                                     (string challenge-id)
                                     (getf props :title)))
          (format t "Difficulty: ~a~%" (getf props :difficulty))
          (format t "~%~a~%~%" (getf props :description))
          (format t "~a~%~%" (getf props :instructions))
          
          (when (getf props :starter-code)
            (format t "=== STARTER CODE ===~%~a~%~%" (getf props :starter-code)))
          
          (case (getf props :type)
            (:exploration 
             (format t "This is an exploration challenge. Try the tasks and report your findings.~%")
             (show-exploration-tasks (getf props :tasks)))
            (:coding
             (format t "Enter your function definition, then type 'submit' to test it.~%")
             (format t "Type 'hint' for help, or 'giveup' to abandon this challenge.~%"))))
        (format t "Challenge ~a not found.~%" challenge-id))))

(defun show-exploration-tasks (tasks)
  "Display exploration tasks for practice."
  (format t "~%=== EXPLORATION TASKS ===~%")
  (loop for (task expected) in tasks
        for i from 1 do
    (format t "~%~d. ~a~%" i task)
    (when expected
      (format t "   Expected result: ~a~%" expected)))
  (format t "~%Complete these tasks, then type 'submit' when done.~%"))

(defun challenge-loop (challenge-id)
  "Enhanced challenge interaction loop with hints and better UX."
  (let ((challenge (get-challenge-content challenge-id))
        (user-code nil)
        (attempt-count 0))
    
    (if (not challenge)
        (format t "Challenge not found!~%")
        
        (let ((challenge-type (getf (rest challenge) :type)))
          (loop do
            (format t "~&challenge(~a)> " challenge-id)
            (force-output)
            (let ((input (read)))
              (cond
                ((equal input 'giveup)
                 (format t "No worries! You can try again later. Learning takes time.~%")
                 (return-from challenge-loop))
                
                ((equal input 'hint)
                 (show-hint challenge-id attempt-count))
                
                ((equal input 'submit)
                 (incf attempt-count)
                 (case challenge-type
                   (:exploration 
                    (format t "Great! Exploration challenges help build intuition.~%")
                    (format t "Challenge completed! 🎉~%")
                    (return-from challenge-loop))
                   (:coding
                    (if user-code
                        (let ((result (run-challenge-tests challenge-id user-code)))
                          (if (eq result 'pass)
                              (progn
                                (format t "Excellent work! Challenge completed! 🎉~%")
                                (when (> attempt-count 1)
                                  (format t "Persistence pays off! You solved it in ~d attempts.~%" attempt-count))
                                (return-from challenge-loop))
                              (progn
                                (format t "Not quite right. ")
                                (if (< attempt-count 3)
                                    (format t "Try 'hint' for help, or keep experimenting!~%")
                                    (format t "You're working hard! Don't give up!~%")))))
                        (format t "Please enter your function definition first.~%")))))
                
                ((equal input 'show-tests)
                 (show-test-cases challenge-id))
                
                (t
                 (when (eq challenge-type :coding)
                   (setf user-code input)
                   (format t "Code saved! Type 'submit' to test, 'hint' for help, or 'show-tests' to see test cases.~%"))))))))))

(defun show-hint (challenge-id attempt-count)
  "Provide contextual hints based on challenge and attempt number."
  (format t "~%💡 HINT: ")
  (case challenge-id
    (c1-1 (format t "Try each expression one at a time. Notice how quoting affects evaluation.~%"))
    (c1-2 (cond
            ((= attempt-count 0) (format t "Think recursively: how long is an empty list? How long is a non-empty list?~%"))
            ((= attempt-count 1) (format t "Base case: (null lst) returns 0. Recursive case: 1 + (my-length (rest lst))~%"))
            (t (format t "Structure: (if (null lst) 0 (+ 1 (my-length (rest lst))))~%"))))
    (c1-3 (cond  
            ((= attempt-count 0) (format t "Try appending the reversed rest to a list containing the first element.~%"))
            ((= attempt-count 1) (format t "Use (append (my-reverse (rest lst)) (list (first lst)))~%"))
            (t (format t "For efficiency, try tail recursion with an accumulator parameter.~%"))))
    (c1-5 (cond
            ((= attempt-count 0) (format t "Base cases: fib(0)=0, fib(1)=1. Recursive: fib(n) = fib(n-1) + fib(n-2)~%"))
            ((= attempt-count 1) (format t "Use COND for multiple conditions: (cond ((= n 0) 0) ((= n 1) 1) ...)~%"))
            (t (format t "Try memoization or iteration to avoid recomputing the same values.~%"))))
    (t (format t "Keep experimenting! Break the problem into smaller parts.~%")))
  (format t "~%"))

(defun show-test-cases (challenge-id)
  "Show what test cases will be run (educational, not cheating)."
  (format t "~%🧪 TEST CASES:~%")
  (case challenge-id
    (c1-2 (format t "• (my-length '()) should return 0~%")
          (format t "• (my-length '(a)) should return 1~%")  
          (format t "• (my-length '(a b c)) should return 3~%")
          (format t "• (my-length '(a b (c d))) should return 3~%"))
    (c1-3 (format t "• (my-reverse '()) should return ()~%")
          (format t "• (my-reverse '(a)) should return (a)~%")
          (format t "• (my-reverse '(a b c)) should return (c b a)~%")
          (format t "• (my-reverse '(1 2 3 4)) should return (4 3 2 1)~%"))
    (c1-5 (format t "• (fibonacci 0) should return 0~%")
          (format t "• (fibonacci 1) should return 1~%")
          (format t "• (fibonacci 2) should return 1~%")
          (format t "• (fibonacci 5) should return 5~%")
          (format t "• (fibonacci 6) should return 8~%"))
    (t (format t "Test cases not specified for this challenge.~%")))
  (format t "~%"))

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
          (eval user-code)
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
          (eval user-code)
          (if (loop for list in test-lists
                    always (equal (funcall 'my-reverse list) (reverse list)))
              (progn (format t "All tests passed! Well done.~%") 'pass)
              (progn (format t "Some tests failed. Check your logic.~%") 'fail)))
      (error (e) (format t "Error evaluating your code: ~a~%" e) 'fail))))

(defun test-hash-table (user-code)
  (declare (ignore user-code))
  (format t "Tests for hash table challenge would run here.~%")
  'pass)

(defun test-fibonacci (user-code)
  "Test if USER-CODE is a correct definition of FIBONACCI."
  (let ((test-cases '((0 0) (1 1) (2 1) (3 2) (4 3) (5 5) (6 8))))
    (handler-case
        (progn
          (eval user-code)
          (if (loop for (n expected) in test-cases
                    always (equal (funcall 'fibonacci n) expected))
              (progn (format t "All tests passed! Well done.~%") 'pass)
              (progn (format t "Some tests failed. Check your logic.~%") 'fail)))
      (error (e) (format t "Error evaluating your code: ~a~%" e) 'fail))))

(defun run-challenge-tests (challenge-id user-code)
  "Determine which test function to run based on the challenge ID."
  (format t "~%Running tests for ~a...~%" challenge-id)
  (funcall (ecase challenge-id
             (c1-1 (function test-repl-exploration))
             (c1-2 (function test-my-length))
             (c1-3 (function test-my-reverse))
             (c1-4 (function test-hash-table))
             (c1-5 (function test-fibonacci)))
           user-code))

;;; -----------------------------------------------------------------------------
;;; Main REPL Loop & Input Handler

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

(start)
