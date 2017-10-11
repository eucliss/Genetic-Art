(ns genetic-art.core
  (:gen-class))

;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '()
   :integer '(1 2 3 4 5 6 7)
   :image '()
   :input {:in1 4}})

; An example Push program
(def example-push-program
  '(4 5 integer_+ integer_*))

(def example-push-program-input
  '(in1))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:program '(3 5 integer_* 4 integer_-)
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37
   :determinant-error 12})


;;;;;;;;;;
;; Instructions must all be either functions that take one Push
;; state and return another or constant literals.
(def instructions
  (list
   'in1  ;; Need more inputs. ...
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   0
   1
   2
   3
   4
   5
   ))


;;;;;;;;;;
;; Utilities

(def image_example
  '((6,3,4,5),
       (5, 3, 1, 9),
       (1, 12, 6, 8),
   (5, 12, 9, 6))
  )

(def image_example_empty
  '((0,0,0,0),
   (0, 0, 0, 0),
   (0, 0, 0, 0),
   (0, 0, 0, 0))
  )

(def empty-push-state
  {:exec '()
   :integer '()
   :image '()
   :input {}})

(def full-state
  {:exec '(integer_+ integer_-)
   :integer '(2 1 3 4)
   :image '((0,0,0,0),
            (0, 0, 0, 0),
            (0, 0, 0, 0),
            (0, 0, 0, 0))
   :input {:in1 '((6,3,4,5),
            (5, 3, 1, 9),
            (1, 12, 6, 8),
            (5, 12, 9, 6))}})

(def div-0-state
  {:exec '(integer_+ integer_-)
   :integer '(2 0 3 4)
   :image '((0,0,0,0),
            (0, 0, 0, 0),
            (0, 0, 0, 0),
            (0, 0, 0, 0))
   :input {:in1 '((6,3,4,5),
            (5, 3, 1, 9),
            (1, 12, 6, 8),
            (5, 12, 9, 6))}})

;; GOOD
(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  ;; We added a clause that deals with pushing to input stack, it reads the keys in the stack,
  ;;  and it will add an input at in(# inputs + 1)
  ;; ex: (push-to-stack {:input {:in1 3 :in2 6}} :input 10) --> {:input {:in1 3 :in2 6 :in3 10}}
  ;; We utilized this in the in1 function
  [state stack item]
  (if (= stack :input)
    (assoc state stack (assoc (state stack) (keyword (str "in" (+ 1 (count (keys (state stack))))))
                        item))
    (assoc state stack (conj (state stack) item))))

;; GOOD
(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (= 0 (count (state stack))))

;; GOOD
(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (if (empty-stack? state stack)
    state
    (assoc state stack (rest (state stack)))))

;; GOOD
(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty-stack? state stack)
    :no-stack-item
    (first (state stack))))

;; GOOD
(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks stacks
         args '()]
    (if (empty? stacks)
      {:state state :args (reverse args)}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))


;; GOOD
(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (reverse (:args args-pop-result)))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;;
;; Instructions

;; MAYBE
(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (push-to-stack (pop-stack state :exec)      ;; Pop in1 from the exec stack
                 :exec                        ;; and push :in1 from :input stack
                 ((state :input) :in1)))

;; BAD
;; I'm thinking for these, we could theoretically +,-,*,/ an int to a row in the image
;; But I think we can use these functions
(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (if (< (count (get state :integer)) 2) ;; conditional to make sure there are enough ints on the stack
    (pop-stack state :exec)
    (make-push-instruction state +' [:integer :integer] :integer)))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top integer."
  [state]
  (if (< (count (get state :integer)) 2)
    (pop-stack state :exec)
    (make-push-instruction state -' [:integer :integer] :integer)))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (if (< (count (get state :integer)) 2)
    (pop-stack state :exec)
    (make-push-instruction state *' [:integer :integer] :integer)))

;; BAD
;; Needs to be changed based on how we do integer_%
(defn divide_by_zero?
  "Helper function for integer_%.  Makes sure we don't divide by 0."
  [state]
  (= (first (state :integer))) 0)

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (if (< (count (get state :integer)) 2)
    (pop-stack state :exec)
    (if (divide_by_zero? state) ;; Return the numerator to the int stack if dividing by 0, else division
      (assoc state :integer
             (conj
              (get (pop-stack (pop-stack state :integer) :integer)
                   :integer)
              (peek-stack state :integer)))
      (make-push-instruction state quot [:integer :integer] :integer))))

(defn vsplit_combine
  "Splits two input images in half and combines them, half of image A, half of image B"
  [A B]
  :STUB
  )

(defn vertical_rotate
  "Rotates the input image vertically by a number
  vertical_rotate(x, 2) ----> ((a), (b), (c)) -> ((b), (c), (a))"
  [A number]
  )

(defn horizontal_rotate
  "Rotates the input image horizontally by a number
  horizontal_rotate(x, 1) ----> ((a1, a2), (b1, b2), (c1, c2)) ->
  ((a2, a1), (b2, b1), (c2, c1))"
  [A number]
  )

;;;;;;;;;;
;; Interpreter

;; MAYBE
(defn load-exec
  "Places a program in the exec stack in a map"
  [program state]
  (assoc state :exec (concat program (state :exec))))


;; MAYBE
(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Returns the new Push state."
  [push-state]
  (if (not (empty-stack? push-state :exec))  ;; If it is empty, return the push-state
    (let [element (peek-stack push-state :exec)] ;; Else lets see whats the first element
      (cond
        (instance? String element) (push-to-stack (pop-stack push-state :exec) :string element) ;; String
        (instance? Number element) (push-to-stack (pop-stack push-state :exec) :integer element) ;; Number
        (= 'in1 element) (in1 push-state) ;; required b/c else statement applies first item in :exec stack and then pops it, so without this inputs just get removed form exec stack
        (seq? element) (interpret-one-step (load-exec element (pop-stack push-state :exec))) ;; Nested isntructions
        :else (pop-stack ;; This is for symbols ( integer_+, integer_-, ...)
               ((eval (first
                         (get (get-args-from-stacks push-state '(:exec))
                              :args)))
               push-state) :exec)))
    push-state))

;; MAYBE
(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing."
  [program start-state]
  (let [state (load-exec program start-state)]
    (loop [state state] ;; Loop until the :exec stack is empty
      (if (empty-stack? state :exec)
          state
          (recur (interpret-one-step state)))))) ;; Recur interpret each step


;;;;;;;;;;
;; GP


;;;;;;;;;;
;; Parent Selection Techniques
;; GOOD
(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-initial-program-size]
  (let [program-size (+ (rand-int max-initial-program-size) 1)]
    (repeatedly program-size #(rand-nth instructions))))

;; MAYBE
(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population
   tournament-size]
  (let [tournament-members (repeatedly tournament-size #(rand-nth population))]
    ;; This finds the individual with the smallest total-error
    (apply min-key #(% :total-error) tournament-members)))

;; GOOD
(defn prob-pick
  "Returns true [prob] amount of the time.  Need second case so we can use with filter."
  ([prob] (< (rand) prob))
  ([prob x] (prob-pick prob)))


;;;;;;;;;;
;; Variation Techniques

;; ---- Mutations ------------------------


;; BAD
;; Needs to be changed based on program structure
(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the program) with some probability. Returns child program."
  [prog
   instructions]
  ;; Added instructions as a parameter
  (let [child (reduce concat
                      (map (fn [x]
                             (if (prob-pick 0.05)
                               (list x (nth instructions (rand-int (count instructions))))
                               (list x))) prog))]
    (if (prob-pick 0.05)
      (conj child (nth instructions (rand-int (count instructions))))
      child)))


;; BAD
(defn uniform-deletion
  "Randomly deletes instructions from program at some rate. Returns child program."
  [program]
  (filter #(not (prob-pick 0.05 %)) program))

(defn fuck-shit-stack
  "Completely re-writes a matrix based off nothing but random numbers"
  [A]
  :STUB)

(defn row_mutate
  "Mutates elements of a row index in A based on a probability, if 50% prob,
  each element in the row of A at that index has a 50% chance of being mutated"
  [A index probability]
  :STUB)

(defn column_mutate
  "Same as row but with using columns"
  [A index probability]
  :STUB)

(defn coordinate_mutate
  "Mutates a single element at a certain coordinate"
  [A row col]
  :STUB)

;; ------------------------------------
;; ---------- Crossovers ------------------

;; BAD
;; Gunna have to change this to work with the new individual and image structure
(defn crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns child program."
  [prog-a
   prog-b]
  (loop [prog-a prog-a
         prog-b prog-b
         new '()]
    (if (empty? prog-a) ;; If one is empty then 50% chance to take the others instruction at that index
      (concat new (filter #(prob-pick 0.5 %) prog-b))
      (if (empty? prog-b)
        (concat new (filter #(prob-pick 0.5 %) prog-a))
        (recur (rest prog-a)
               (rest prog-b)
               (if (= (rand-int 2) 0) ;; Pick one of the programs instructions and add to child
                 (apply list (conj (apply vector new) (first prog-a)))
                 (apply list (conj (apply vector new) (first prog-b)))))))))

;; BAD
(defn prog-to-individual
  "Takes a program and creates an individual with no error values or with error values if given."
  ([prog]  ;; Just converts program to individual with no errors
  {:program prog
   :errors '[]
   :total-error 0})
  ([prog error-list total-error]  ;; Converts a program to an individual with its errors
   {:program prog
    :errors (first error-list)
    :total-error (first error-list)}))


;;;;;;;;;;;;
;; New Population Creation Function

;; BAD
;; Gunna make more instructions so we need to make this even more vobust
;; probably going to want to make a new function that decides if we are going to
;;  mutate or crossover or something like that
(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population
   tournament-size]
  (let [seed (rand)    ;; Want to keep the same random number to base decision on
        parent1 (into () (:program (tournament-selection population tournament-size)))    ;; Only want to select parents once, so save them
        parent2 (into () (:program (tournament-selection population tournament-size)))]
    (cond
      (< seed 0.5) (crossover parent1 parent2)
      (and (>= seed 0.5) (< 0.75)) (uniform-addition parent1 parent2)
      (>= seed 0.75) (uniform-deletion parent1))))


;;;;;;;;;;;;
;; Reporting

;; MAYBE
(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).

-------------------------------------------------------
               Report for Generation 3
-------------------------------------------------------
Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
Best program size: 33
Best total error: 727
Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)
  "
  [population generation]
  (println)
  (println "-------------------------------------------------------")
  (printf  "                    Report for Generation %s           " generation)
  (println)
  (println "-------------------------------------------------------")

  (let [best-prog (apply min-key #(% :total-error) population)]
    (printf "Best program: ")
    (println (best-prog :program)) ;; Wanted to print the actual program, not just the location
    (println)
    (printf "Best program size: %s" (count (best-prog :program)))
    (println)
    (printf "Best total error: %s" (best-prog :total-error))
    (println)
    (printf "Best errors: %s" (best-prog :errors))))

;; MAYBE
(defn report-more
  "Increased reporting we wanted to see state of our population"
  [pop gen]
  (report pop gen)
  (println)
  (printf "Total population error: %s" (reduce + (map #(% :total-error) pop)))
  (println)
  (printf "Average program size: %s" (quot (reduce + (map #(count (% :program)) pop)) (count pop))))

;; --------------------------------------------------

;; GOOD
(defn init-population
  "Initialize a population of random programs of a certain maximum size"
  [size max-program-size]
  ;; Creates individuals with no errors associated with them yet
  (map #(prog-to-individual %) (take size (repeatedly #(make-random-push-program instructions max-program-size)))))

;; MAYBE
(defn get-child-population
  "Creates the next generation using select-and-vary function on the previous generation"
  [population population-size tournament-size]
  (loop [new-pop '()]
    (if (= (count new-pop) population-size)
      new-pop
      (recur (conj new-pop
                   (select-and-vary population tournament-size))))))

;; MAYBE
(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-program-size (max size of randomly generated programs)"
  [{:keys [population-size max-generations error-function instructions max-initial-program-size]}]
  (loop [count 0
         population (map #(error-function %) (init-population population-size max-initial-program-size))]
    (report population count)
    (if (>= count max-generations) ;; If we reach max-generations, null, otherwise keep going
      nil
      (if (= 0 (get (apply min-key #(% :total-error) population) :total-error)) ;; Anyone with error=0?
        :SUCCESS
        (recur (+ count 1) ;; Recur by making new population, and getting errors
               (map #(error-function (prog-to-individual %)) (get-child-population (map #(error-function %) population) population-size 20))))))) ;; Using a fixed tournament size of 20 for quick conversion


;;;;;;;;;;
;; Error Functions

;; GOOD
(defn abs
  "Returns the absolute value of a number x"
  [x]
  (if (< x 0)
    (*' -1 x)
    x))

;; MAYBE
(defn evaluate-one-case
  "Evaluates a single case for regression error function"
  [individual state value]
  (interpret-push-program (:program individual) (push-to-stack state :input value)))

;; MAYBE
;; PRolly useful comparing on line of the image, needs to be changed probably
(defn abs-difference-in-solution-lists
  "Computes the differences in the solutions for the input programs, returns errors list"
  ;; Ex: solution:(1 2 3 4), program solution:(4 4 4 4), output of this function: (3 2 1 0)
  [l1 l2]
  (loop [l1 l1
         l2 l2
         final '()]
    (if (= (count l1) 0)
      (reverse final)
      (recur (rest l1)
             (rest l2)
             (conj final (abs (- (first l1) (first l2))))))))

;; MAYBE
;; Probably going to need to change
(defn get-solution-list
  "Gets the list of solution for a test case"
  [individual]
  (map #(if (= (:integer %) '())
             1000000 ;; Large penalty
             (first (:integer %)))
             (map #(evaluate-one-case individual empty-push-state %) test-cases)))

;; BAD

(defn determinant
  "Returns the determinant of a matrix"
  [A]
  :STUB
  )

(defn determinant-error
  "Returns the determinant error for a given matrix based off the ideal matrix"
  [A B]
  :STUB
  )

(defn regression-error-function
  "Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual]
  (let [target-list (map #(target-function %) test-cases) ;; List of solutions for the target function
        program-list (get-solution-list individual) ;; List solutions for given individual
        errors (abs-difference-in-solution-lists target-list program-list) ;; Calculates errors
        ]
    {:program (:program individual)
     :errors errors
     :total-error (reduce + errors)}))

;;;;;;;;;;
;; The main function. Uses some problem-specific functions.

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (push-gp {:instructions instructions
            :error-function regression-error-function
            :max-generations 100
            :population-size 200
            :max-initial-program-size 50}))
