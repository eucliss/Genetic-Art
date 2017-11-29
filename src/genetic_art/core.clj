
(ns genetic_art.core
  (:gen-class))
(require '[clojure.core.matrix :as m])
(require '[clojure.core.matrix.operators :as m-ops])


(m/set-current-implementation :vectorz)

(use 'mikera.image.core)
(use 'mikera.image.colours)
(require '[mikera.image.filters :as filt])


;;;;;;;;;;
;; Examples

; An examplePush state
(def example-push-state
  {:exec '()
   :integer '(1 2 3 4 5 6 7)
   :image '()
   :input {:in1 4}})

; An example Push program
(def example-push-program
  '(in2 in1* hsplit_combine invert_colors))

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

;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
;;;;;;;;;;;;;;;;;;;;;;;
(def image_example
  '(6,3,4,5 5, 3, 1, 9, 1, 12, 6, 8, 5, 12, 9, 6)
  )

(def image_example_empty
  '(0,0,0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

(def empty-push-state
  {:exec '()
   :integer '()
   :image '()
   :input {}
   :bool '()})

(def in-state
  {:exec '()
   :integer '()
   :image '()
   :input {:in1 '(6,3,4,5 5, 3, 1, 9, 1, 12, 6, 8, 5, 12, 9, 6)}})

(def full-state
  {:exec '(1 integer_-* integer_-*)
   :integer '(4 3 3 4)
   :image '((1,3,4,5, 6, 7, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10))
   :input {:in1 '(6,3,4,5 5, 3, 1, 9, 1, 12, 6, 8, 5, 12, 9, 6)}})

(def buff-state
  {:exec '(noise_filter 1 integer_-* integer_-*)
   :integer '(4 3 3 4)
   :image (list (load-image-resource "arrow_up.jpg"))
   :input {:in1 (load-image-resource "cars.jpg")}
   :bool '(true false)})

(def double-image-state-100px
  {:exec '(noise_filter 1 integer_-* integer_-*)
   :integer '(4 3 3 4)
   :image (list (load-image-resource "btnPlus.png") (load-image-resource "arrow_up.jpg"))
   :input {:in1 (load-image-resource "cars.jpg")}
   :bool '(true false)})

;(false false 1 exec_dup exec_dup section-and exec_if exec_if edge_filter)

(def bad-stuff
  {:exec '(exec_dup exec_dup section-and exec_if exec_if edge_filter)
   :integer '()
   :image '()
   :input {}
   :bool '(false false)})

(defn dub-func-state
  []
  {:exec '(exec_dup noise_filter 1 integer_-* integer_-*)
   :integer '(4 3 3 4)
   :image (list (load-image-resource "btnPlus.png") (load-image-resource "arrow_up.jpg"))
   :input {:in1 (load-image-resource "cars.jpg")}
   :bool '(true false)})

(def buff-state-cars
  {:exec '(emboss_filter 1 integer_-* integer_-*)
   :integer '(4 3 3 4)
   :image (list (load-image-resource "cars.jpg"))
   :input {:in1 (load-image-resource "arrow_up.jpg")}
   :bool '(true false)})

(def test-state
  {:exec '(1 integer_-* integer_-*)
   :integer '(4 3 3 4)
   :image '((0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3), (4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7))
   :input {:in1 '(6,3,4,5 5, 3, 1, 9, 1, 12, 6, 8, 5, 12, 9, 6)}})

(def div-0-state
  {:exec '(integer_+ integer_-)
   :integer '(2 0 3 4)
   :image '((0,0,0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
   :input {:in1 '(6,3,4,5 5, 3, 1, 9, 1, 12, 6, 8, 5, 12, 9, 6)}})


;(exec_dup scramble_grid true noise_filter edge_filter exec_dup 1 false false noise_filter section-xor)
;(scramble_grid emboss_filter section-xor invert_colors true exec_dup section-and section-or)
;(scramble_grid scramble_grid emboss_filter invert_colors true exec_dup section-and section-or)


;; THESE PROGRAMS ARE CURRENTLY CAUSING THE HAGNING
;{:program (in1* exec_dup invert_colors edge_filter in1* in1* true false 1), :errors (0.0 0.0 0.0 0.0 0.0 0.0 0.0 4.632222114615712E17 0.0 0.0 0.0 0.0 0.0 1.437434250550272E15 0.0 0.0 0.0 2.59104440947307424E17 -0.0 0.0 0.0 -0.0 0.0 0.0 0.0), :total-error 7.2376408665942886E17}
 ;{:program (in1* exec_dup invert_colors edge_filter in1* true false 1), :errors (0.0 0.0 0.0 0.0 0.0 0.0 0.0 4.632222114615712E17 0.0 0.0 0.0 0.0 0.0 1.437434250550272E15 0.0 0.0 0.0 2.59104440947307424E17 -0.0 0.0 0.0 -0.0 0.0 0.0 0.0), :total-error 7.2376408665942886E17}
;{:program (in1* exec_dup invert_colors edge_filter in1* true false 1), :errors (0.0 0.0 0.0 0.0 0.0 0.0 0.0 4.632222114615712E17 0.0 0.0 0.0 0.0 0.0 1.437434250550272E15 0.0 0.0 0.0 2.59104440947307424E17 -0.0 0.0 0.0 -0.0 0.0 0.0 0.0), :total-error 7.2376408665942886E17}

;{:program 
; , :errors (0.0 0.0 0.0 0.0 0.0 0.0 0.0 4.632222114615712E17 0.0 0.0 0.0 0.0 0.0 1.437434250550272E15 0.0 0.0 0.0 2.59104440947307424E17 -0.0 0.0 0.0 -0.0 0.0 0.0 0.0), :total-error 7.2376408665942886E17} 


(def broken-empty-stack
  {:exec '(1 false true in1* in1* edge_filter invert_colors exec_dup in1*)
   :integer '(4 3 3 4)
   :image (list (load-image-resource "cars.jpg"))
   :input {:in1 (load-image-resource "arrow_up.jpg")}
   :bool '(true false)})

;;;;;;;;;;;;;;;;;;
;; Examples end ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; Instructions must all be either functions that take one Push
;; state and return another or constant literals.

(def init-instructions
  (list

   ;'in1*
   ;'in2
   ;'in1*
   ;'in2
   ;'in1*
   ;'in2
   ;'fuck-shit-stack
   'exec_dup
   'exec_if
   'invert_colors
   ;'laplace_filter
   ;'emboss_filter
   ;'edge_filter
   ;'laplace_filter
   'noise_filter
   'scramble_grid
   'section-and
   'section-or
   'section-xor
   'hsplit_combine
   'section-rotate
   'section-rotate
   'section-rotate
   true
   false
   1
   ))

(def instructions
  (list
   'in1  ;; Need more inputs. ...
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   0
   1
   ))

(def instruction
  (list
   'in1  ;; Need more inputs. ...
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   'vsplit_combine
   'hsplit_combine
   'vertical_rotate
   'horizontal_rotate
   'row_mutate
   'column_mutate
   'invert_colors
   'scramble_grid
   'section-and
   'section-or
   'section-xor
   0
   1
   2
   3
   4
   5
   ))

;; NEED MORE INSTRUCTIONS

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

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
  ;(println state)
  ;(println stack)
  (zero? (count (state stack))))

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

;;(defn list-to-matrix
;;  [lst width height]
;;  ::STUB
  ;; (into [] lst)
  ;; (split-at width lst)
  ;;(into [] (dotimes [i width]
;;  (loop [lst (into [] (vector (first (split-at width lst))))
;;         rest (split-at width lst)]
;;    (if (empty? rest)
;;      lst
 ;;     (recur (split-at
 ;; )))))

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

(defn print-image-pixels
  "Takes a BufferedImage and prints it"
  [image]
  (loop [x 0]
    (loop [y 0]
      (when (< y (dec (height image)))
            (print (get-pixel image x y) "")
            (recur (inc y))))
    (when (< x (dec (width image)))
          (recur (inc x)))))

(defn image_to_matrix
  [img]
  (m/matrix (into [] (map #(into [] %)(partition (width img) (get-pixels img))))))

;;;;;;;;;;;;;;;;;;;
;; Utilities End ;;
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; Instructions ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integer stack instructions

;; THESE WILL BE USED FOR EXEC INSTRUCTIONS ONLY

;; MAYBE
(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  ;; Doesnt pop from the exec stack
  [state]
  (push-to-stack state
                 :exec
                 ((state :input) :in1)))

(defn in1*
  [state]
  (push-to-stack state :image ((state :input) :in1)))
(defn in2
  [state]
  (push-to-stack state :image ((state :input) :in2)))
  ;;(assoc state :image ((state :input) :in1)))

;; BAD
;; I'm thinking for these, we could theoretically +,-,*,/ an int to a row in the image
;; But I think we can use these functions
(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top integer."
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

;; If the deducer is greater than the item, return 0, else return x-y
(defn zero_-
  [x y]
  (if (> x y)
   (-' x y)
   0))
;;(zero_- 4 5 ) ;;=> 0
;;(zero_- 6 5)  ;;=> 1

;; Takes a state and applies subtraction to it getting the item from the integers
;; If the integer is bigger than the list item, it returns 0
(defn integer_-*
  [state]
  (let [deducer (peek-stack state :integer)]
    (if (zero? (count (:integer state)))
      state
      (pop-stack (assoc state :image (concat (list (map #(zero_- % deducer) (first (get state :image)))) (rest (:image state)))) :integer))))

(defn minus_pixels
  [imag pixels deducer]
  (dotimes [i (count pixels)] (aset pixels i (zero_- (nth pixels i) deducer)))
  (set-pixels imag pixels)
  imag)

(defn integer_-*_buffered
  [state]
  (if (zero? (count (:integer state)))
     state
     (let [deducer (peek-stack state :integer)
           imag (first (get state :image))
           pixels (get-pixels imag)]
       (pop-stack (assoc state :image (conj (rest (:image state)) (minus_pixels imag pixels deducer))) :integer))))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

;; BAD
;; Needs to be changed based on how we do integer_%
(defn divide_by_zero?
  "Helper function for integer_%.  Makes sure we don't divide by 0."
  [state]
  (zero? (first (state :integer))))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (if (< (count (:integer state)) 2)
    state
    (if (divide_by_zero? state) ;; Return the numerator to the int stack if dividing by 0, else division
      (assoc state :integer
             (conj
              (get (pop-stack (pop-stack state :integer) :integer)
                   :integer)
              (peek-stack state :integer)))
      (make-push-instruction state quot [:integer :integer] :integer))))

;;;;;;;;;;;;;;;;;;;;;
;; Exec instructions

;; WILL NEED TO DEAL WITH INFINITE LOOPS IN EXEC INSTRUCTIONS
(defn exec_do*count
  ""
  [state]
  :STUB)

(defn exec_do*range
  ""
  [state]
  :STUB)

(def dup-state
  {:exec '(exec_dup 1 integer_+)
   :integer '(4 3 3 4)
   :image '((1,3,4,5, 6, 7, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10))
   :input {:in1 '(6,3,4,5 5, 3, 1, 9, 1, 12, 6, 8, 5, 12, 9, 6)}})

(defn exec_dup
  "Duplicates the first element on the exec stack and places it in front"
  [state]
  :STUB
  (if (zero? (count (get state :exec)))
    state
    (if (= (peek-stack bad-stuff :exec) 'exec_dup)
      state
      (assoc state :exec (conj (get state :exec) (peek-stack state :exec)))
      )))

(def if-state
  {:exec '(exec_if)
   :integer '(4 3 3 4)
   :image '((1,3,4,5, 6, 7, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10))
   :input {:in1 '(6,3,4,5 5, 3, 1, 9, 1, 12, 6, 8, 5, 12, 9, 6)}
   :bool '()})

(defn exec_if
  "If bool stack has true, first item on exec stack, else second item"
  [state]
  (if (or (zero? (count (get state :bool))) (> 1 (count (get state :exec)))) ;; need enough elements to work with
    state
    (if (peek-stack state :bool)
      ;; remove second element of exec, popping from bool stack also
      (assoc (pop-stack state :bool) :exec (conj (rest (rest (get state :exec))) (peek-stack state :exec)))
      ;; remove first element of exec, popping from bool stack also
      (assoc (pop-stack state :bool) :exec (rest (get state :exec)))
      )))

;;;;;;;;;;;;;;;;;;;;;
;; Image manipulation

(defn vsplit_combine_list
  "Splits two input images in half and combines them, half of image A, half of image B.
  Split column is decided randomly here."
  [ls1 ls2 rand-index width height]
  (loop [new-lst '()
         index 0
         images (list ls1 ls2)]
        (if (= index (count ls1))
          new-lst
          (if (= index width)
            (recur (concat new-lst (list (nth (first (reverse images)) index)))
                   (+ index 1)
                   (reverse images))
            (if (and (zero? (mod index rand-index)) (not (zero? index)))
              (recur (concat new-lst (list (nth (first (reverse images)) index)))
                     (+ index 1)
                     (reverse images))
              (recur (concat new-lst (list (nth (first images) index)))
                     (+ index 1)
                     images))))))

;; indx = mod rand index concat reverse
;; index = width reverse

(def vsplit-state
  {:exec '(1 integer_-* integer_-*)
   :integer '(4 3 3 4)
   :image '((0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3), (4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7))
   :input {:in1 '(6,3,4,5 5, 3, 1, 9, 1, 12, 6, 8, 5, 12, 9, 6)}})

;;(first (get (vsplit_combine_list vsplit-state) :image)) ;; => ( 0 0 4 4 1 1 5 5 2 2 6 6 3 3 7 7)

(defn vsplit_helper
  [state img2 new-list]
  ;(println (count new-list))
  ;(println (width img2))
  ;;(set-pixels img2 (int-array new-list))
  ;;(show img2 :zoom 10.0)
  (push-to-stack (pop-stack (pop-stack state :image) :image) :image img2))

(defn vsplit_combine
  "Splits two images horizontally and combines them.
  Split row is decided randomly here."
  [state]
  (if (< (count (get state :image)) 2)
    state
    (let [img1 (peek-stack state :image)
          img2 (peek-stack (pop-stack state :image) :image)
          rand-index (+ 1 (rand-int (- (width img1) 2)))
          sub-pixels1 (get-pixels (sub-image img1 0 0 rand-index (height img1)))
          sub-pixels2 (get-pixels (sub-image img2 rand-index 0 (- (width img1) rand-index) (height img1)))
          ]
      (vsplit_helper state img2 (int-array (vsplit_combine_list sub-pixels1 sub-pixels2 rand-index (width img1) (height img1)))))))
      ;;(set-pixels img2 (int-array (concat sub-pixels1 sub-pixels2)))
      ;;(show img2 :zoom 10.0)
      ;;(push-to-stack (pop-stack (pop-stack state :image) :image) :image img2) )))

(defn hsplit_combine
  "Splits two images horizontally and combines them.
  Split row is decided randomly here."
  [state]
  (if (< (count (get state :image)) 2)
    state
    (let [img1 (peek-stack state :image)
          img2 (peek-stack (pop-stack state :image) :image)
          rand-index (+ 1 (rand-int (- (height img1) 2)))
          sub-pixels1 (get-pixels (sub-image img1 0 0 (width img1) rand-index))
          sub-pixels2 (get-pixels (sub-image img2 0 rand-index (width img1) (- (height img1) rand-index)))
          ]
      (set-pixels img2 (int-array (concat sub-pixels1 sub-pixels2)))
      ;(show img2 :zoom 10.0)
      (push-to-stack (pop-stack (pop-stack state :image) :image) :image img2) )))


(defn replace-mat-section
  [orig-mat
   sub-mat
   start-x
   start-y
   dim]
  ;(println (range start-x (+ start-x dim)))
  (map (fn [y]
         (map (fn [x]
                (m/mset! orig-mat
                         x y
                         (m/mget sub-mat
                                 (- x start-x) (- y start-y))))
              (range start-x (+ start-x dim)))
         (range start-y (+ start-y dim)))))
              
(defn replace-img-section2
  [img
   sub-img
   start-x
   start-y]
  (map (fn [x]
         (map (fn [y]
                (set-pixel img
                           x y
                           (get-pixel sub-img
                                      (- x start-x) (- y start-y))))
              (range start-y (+ start-y (height sub-img)))))
         (range start-x (+ start-x (width sub-img))))
  img)


(defn replace-img-helper
  [img
   sub-img
   start-x
   start-y
   y]
  (loop [x start-x]
    (if (< x (+ start-x (width sub-img)))
      (do
        (set-pixel img
                   x y
                   (get-pixel sub-img
                              (- x start-x) (- y start-y)))
        (recur (inc x))))))
  
(defn replace-img-section
  [img
   sub-img
   start-x
   start-y]
  (loop [y start-y]
    (if (< y (+ start-y (height sub-img)))
      (do 
        (replace-img-helper img sub-img start-x start-y y)
        (recur (inc y)))))
  img)
    
(defn section-rotate
  [state]
  (let [img (peek-stack state :image)
        rand-x (rand-int (width img))
        rand-y (rand-int (height img))
        rand-dim (inc (rand-int (min (dec (- (width img) rand-x)) (dec (- (height img) rand-y)))))
        ;img-matrix (image_to_matrix img)
        rotated-section (rotate (sub-image img rand-x rand-y rand-dim rand-dim) (rand-nth '(90 180 270)))]
        ;rotated-section-matrix (image_to_matrix
  
    (push-to-stack (pop-stack state :image) :image
                   (replace-img-section img rotated-section rand-x rand-y))
    ))


(defn apply-bit-operators
  [ls op]
  (apply #((eval op) % %2) ls))

(defn image-bitwise-helper
  [state op]
  (if (>= 2 (count (get state :image)))
    state
    (let [img1 (peek-stack state :image)
          img2 (peek-stack (pop-stack state :image) :image)
          pixels1 (int-array (get-pixels img1))
          pixels2 (int-array (get-pixels img2))]
      (set-pixels img2 (int-array (map #(apply-bit-operators % op) (map list pixels1 pixels2))))
      (push-to-stack (pop-stack (pop-stack state :image) :image) :image img2) )))

(defn section-and
  "Takes a state, takes two random rectangles out of
  two images of random dimensions (same for each image) and performs a bitwise AND between
  all of the pixels in rectangle 1 and rectangle 2.  Returns modified image 2"
  [state]
  :STUB
  (image-bitwise-helper state 'bit-and))

(defn section-or
  "Takes a state, takes two random rectangles out of
  two images of random dimensions (same for each image) and performs a bitwise OR between
  all of the pixels in rectangle 1 and rectangle 2.  Returns modified image 2"
  [state]
  :STUB
  (image-bitwise-helper state 'bit-or))

(defn section-xor
  "Takes a state, takes two random rectangles out of
  two images of random dimensions (same for each image) and performs a bitwise XOR between
  all of the pixels in rectangle 1 and rectangle 2.  Returns modified image 2"
  [state]
  :STUB
  (image-bitwise-helper state 'bit-xor))

;(show (peek-stack (section-and double-image-state-100px) :image) :zoom 10.0)

(defn vertical_rotate
  "Rotates the input image vertically by a random number, which is generated here.
  vertical_rotate(x, 2) ----> ((a), (b), (c)) -> ((b), (c), (a))"
  [state]
  :STUB)

(defn horizontal_rotate
  "Rotates the input image horizontally by a random number, which is generated here.
  horizontal_rotate(x, 1) ----> ((a1, a2), (b1, b2), (c1, c2)) ->
  ((a2, a1), (b2, b1), (c2, c1))"
  [state]
  :STUB)

(defn rand-color-input
  [x]
  (rand-colour))

(defn fuck-shit-stack
  "Completely re-writes a matrix based off nothing but random numbers"
  [state]
  (if (empty? (get state :image))
    state
    (let [pixels (get-pixels (first (get state :image))) ;; get pixel list
          imag (first (get state :image))] ;; get image from state
      (set-pixels imag (int-array (map rand-color-input pixels))) ;; set pixels in pixel list, and then write the list to the image
      (assoc (pop-stack state :image) :image (conj (get (pop-stack state :image) :image) imag))))) ;; replace image in stack

(defn row_mutate
  "Mutates elements of a random row in A based on a probability.
  Each element in the row has a [probability] chance of being mutated"
  [state]
  :STUB)

(defn column_mutate
  "Same as row but with using columns"
  [state]
  :STUB)

(defn invert_colors
  "Inverts colors of the pic"
  [state]
  :STUB
  (if (empty-stack? state :image)
    state
    (assoc (pop-stack state :image) :image (conj (get (pop-stack state :image) :image) (filter-image (peek-stack state :image) (filt/invert))))))

(defn laplace_filter
  "Applies a laplace filter to the image"
  [state]
  :STUB
    (if (empty-stack? state :image)
    state
    (assoc (pop-stack state :image) :image (conj (get (pop-stack state :image) :image) (filter-image (peek-stack state :image) (filt/laplace))))))

(defn emboss_filter
  "Applies a emboss filter to the image"
  [state]
  :STUB
  (if (empty-stack? state :image)
    state
    (assoc (pop-stack state :image) :image (conj (get (pop-stack state :image) :image) (filter-image (peek-stack state :image) (filt/emboss))))))

(defn edge_filter
  "Applies an edge filter to the image"
  [state]
  :STUB
  (if (empty-stack? state :image)
    state
    (assoc (pop-stack state :image) :image (conj (get (pop-stack state :image) :image) (filter-image (peek-stack state :image) (filt/edge))))))


(defn noise_filter
  "Applies a noise filter to the image"
  [state]
  :STUB
  (if (empty-stack? state :image)
    state
    (assoc (pop-stack state :image) :image (conj (get (pop-stack state :image) :image) (filter-image (peek-stack state :image) (filt/noise))))))

(defn fuck-shit-stack
  "Completely re-writes a matrix based off nothing but random numbers"
  [state]
  (let [pixels (get-pixels (first (get state :image))) ;; get pixel list
        imag (first (get state :image))] ;; get image from state
    (set-pixels imag (int-array (map rand-color-input pixels))) ;; set pixels in pixel list, and then write the list to the image
    (assoc (pop-stack state :image) :image (conj (get (pop-stack state :image) :image) imag)))) ;; replace image in stack

(defn three-egg-scramble
  "Takes a split list and turns it into buffered image from the state"
  [state lst]
;;  (println (count lst)))
  ;;(println (set-pixels (peek-stack state :image) (int-array (apply concat (shuffle lst)))))
  (let [pixels (int-array (apply concat (shuffle lst)))
        img (peek-stack state :image)]
    (set-pixels (peek-stack state :image) (int-array (apply concat (shuffle lst))))
    state))
  ;;(assoc state :image (conj (pop-stack state :image) img))))


;; Does bad stuff but still isnt perfect, kinda funky
(defn scramble_grid
  "Splits the image into smaller rectangles of random size and randomly places all of them in
  a new spot"
  [state]
  (if (empty? (get state :image))
    state
    (let [img (first (get state :image))
          wid (quot (width img) 2)
          hght (quot (height img) 2)]
      (loop [split_list '()
             x 0
             y 0]
        (if (>= y (* 2 hght))
          (three-egg-scramble state split_list)
          (if (>= x (* 2 wid))
            (recur split_list
                   0
                   (+' y hght))
            (recur (conj split_list (get-pixels (sub-image img x y wid hght)))
                   (+' x wid)
                   y)))))))


;; (concat (shuffle '(1 2 3)) (shuffle '( 4 5 6)))
;; (set-pixels imag (int-array (map rand-color-input pixels)))



;;;;;;;;;;;;;;;;;;;;;;
;; Instructions End ;;
;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;
;; Interpreter ;;
;;;;;;;;;;;;;;;;;

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
    (let [element (peek-stack push-state :exec)
          popped-state (pop-stack push-state :exec)] ;; Else lets see whats the first element
      (cond
        (instance? Boolean element) (push-to-stack popped-state :bool element)
        (integer? element) (push-to-stack popped-state :integer element) ;; Number
        (= 'in1 element) (in1* popped-state) ;; required b/c else statement applies first item in :exec stack and then pops it, so without this inputs just get removed form exec stack
        (seq? element) (interpret-one-step (load-exec element popped-state)) ;; Nested isntructions
        :else ((eval element) popped-state)))
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

;; Just for testing really, im calling (interpret-full-state full-state) => state with image (0 0 0 0 0 0 1 2 3 3 ...)
(defn interpret-full-state
  [state]
  (interpret-push-program (get state :exec) (assoc state :exec '())))
;;;;;;;;;;;;;;;;;;;;;
;; Interpreter End ;;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; GP   ;;
;;;;;;;;;;

;; GOOD
(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-initial-program-size]
  (let [program-size (+ (rand-int max-initial-program-size) 1)]
    (repeatedly program-size #(rand-nth instructions))))


(defn shuffle-order
  "Returns a list of indicies that are in the range of test-cases size, without replacement
  so that we can shuffle the test cases for each individual in the same way."
  [individual]
  (let [test-size (count (:errors individual))]
    (shuffle (range test-size))))

(defn shuffle-test-cases
  "Returns a shuffled list of test-cases for an individual"
  [individual order]
  (let [shuffled (map #(nth (:errors individual) %) order)] 
    (assoc individual :errors shuffled)))

(defn find-lowest-error
  "Finds the lowest error in the population for a given test case"
  [population case]
  ;(println "case errors: " (map #(nth % case) (map #(:errors %) population)))
  (apply min (map #(nth % case) (map #(:errors %) population))))


(defn errors-stddev
  [population
   case]
  (let [errors-list (map #(nth (:errors %) case) population)
        mean-error (/ (apply + errors-list) 2)]
    (/ (apply + (map #(Math/pow (- % mean-error) 2) errors-list)) 2)))
    
(defn lexicase-selection
  "Takes a population of evaluated individuals. Goes through test
  cases in random order.  Removes any individuals with error value on
  given test case greater than best error in population.  Once we are done
  going through test cases, random remaining individual will be returned for
  reproduction."
  [population tournament-size]
  (let [order (shuffle-order (first population))
        new-pop (map #(shuffle-test-cases % order) population)]
    (loop [candidates new-pop
           case 0]

      (if (empty? candidates)
        (rand-nth population)
      (if (= (count candidates) 1)
        (first candidates)
        (if (>= case (count order))
          (rand-nth candidates)
          (let [lowest-error (find-lowest-error candidates case)
                new-candidates (remove #(= % nil)
                                       (map (fn [candidate]
                                              (if (<= (nth (:errors candidate) case) 
                                                      (+ lowest-error (errors-stddev candidates case)))
                                                
                                                  candidate)) candidates))]
            (recur new-candidates
                   (inc case)))))
      ))))





                ; (loop [cand-index 0
                ;        curr-pop candidates]
                ;   (println "CAND-INDEX: " cand-index)
                ;   (println "currpop size: " (count curr-pop))
                ;   (println "candidates first error: " (nth (:errors (nth candidates cand-index)) case))
                ;   (println "candidates errors: " (:errors (nth candidates cand-index)))
                ;   (if (>= cand-index (count candidates))
                ;     curr-pop
                ;     (if (> (nth (:errors (nth candidates cand-index)) case) lowest-error)
                ;       (recur cand-index
                ;              (concat (subvec (vec curr-pop) 0 cand-index)
                ;                      (subvec (vec curr-pop) (inc cand-index))))
                ;       (recur (inc cand-index)
                ;              curr-pop))))))))))




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

;;;;;;;;;;;;;;;;;;;;;;;;
;; GP Operators       ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------------------
;; ---------- Crossovers ------------------

;; BAD
;; Gunna have to change this to work with the new individual and image structure

(defn uniform-crossover
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



(defn pick-indices
  [prog]
  (let [indices (range (count prog))        
        first (rand-nth indices)
        second (if (> (count indices) 1)
                 (rand-nth (remove #(= % first) indices))
                 first)]
    (sort (list first second))))

(defn two-point-crossover
  [prog-a
   prog-b]
  (let [indices-a (pick-indices prog-a)
        indices-b (pick-indices prog-b)]
    (concat (subvec (vec prog-b) 0 (first indices-b))
            (subvec (vec prog-a) (first indices-a) (last indices-a))
            (subvec (vec prog-b) (last indices-b)))))


(defn alternation-crossover
  [prog-a
   prog-b]
  )

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

;;;;;;;;;;;;;;;;;;
;; End operators
;;;;;;;;;;;;;;;;;;

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

(defn state-to-individual
  ([state]  ;; Just converts program to individual with no errors
  {:program (:program state)
   :image (:image state)
   :errors '[]
   :total-error 0})
  ([state error-list total-error]  ;; Converts a program to an individual with its errors
   {:program (:program state)
    :image (:image state)
    :errors (first error-list)
    :total-error (first error-list)}))


;;;;;;;;;;;;
;; New Population Creation Function

(defn remove-selected
  [population
   parent1]
  (let [ind (.indexOf (map #(:program %) population) parent1)]
    (concat (subvec (vec population) 0 ind) (subvec (vec population) (inc ind)))))

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
   tournament-size
   parent-select-fn]
  (let [seed (rand)    ;; Want to keep the same random number to base decision on
        parent1 (:program (parent-select-fn population tournament-size))    ;; Only want to select parents once, so save them
        new-pop (remove-selected population parent1)
        parent2 (:program (parent-select-fn population tournament-size))]

    (cond
      (< seed 0.5) (if (<= seed 0.25)
                     (uniform-crossover parent1 parent2)
                     (two-point-crossover parent1 parent2))
      (and (>= seed 0.5) (< 0.75)) (uniform-addition parent1 parent2)
      (>= seed 0.75) (uniform-deletion parent1))))



;; GOOD
(defn init-population
  "Initialize a population of random programs of a certain maximum size"
  [size max-program-size instructions]
  ;; Creates individuals with no errors associated with them yet
  (map #(prog-to-individual %) (take size (repeatedly #(make-random-push-program instructions max-program-size)))))

(defn load-images
  "Loads a bunch of BufferedImages into a list from
  a bunch of image file names"
  [& images]
  (map #(load-image-resource %) images))

;; MAYBE
(defn get-child-population
  "Creates the next generation using select-and-vary function on the previous generation"
  [population population-size tournament-size parent-select-fn]
  (loop [new-pop '()]
    (if (= (count new-pop) population-size)
      new-pop
      (recur (conj new-pop
                   (select-and-vary population tournament-size parent-select-fn))))))


;; THESE PROGRAMS ARE CURRENTLY CAUSING THE HAGNING
;{:program (in1* exec_dup invert_colors edge_filter in1* in1* true false 1)
;, :errors (0.0 0.0 0.0 0.0 0.0 0.0 0.0 4.632222114615712E17 0.0 0.0 0.0 0.0 0.0 1.437434250550272E15 0.0 0.0 0.0 2.59104440947307424E17 -0.0 0.0 0.0 -0.0 0.0 0.0 0.0), :total-error 7.2376408665942886E17}
 ;{:program (in1* exec_dup invert_colors edge_filter in1* true false 1)
;, :errors (0.0 0.0 0.0 0.0 0.0 0.0 0.0 4.632222114615712E17 0.0 0.0 0.0 0.0 0.0 1.437434250550272E15 0.0 0.0 0.0 2.59104440947307424E17 -0.0 0.0 0.0 -0.0 0.0 0.0 0.0), :total-error 7.2376408665942886E17}
;{:program (in1* exec_dup invert_colors edge_filter in1* true false 1)
;, :errors (0.0 0.0 0.0 0.0 0.0 0.0 0.0 4.632222114615712E17 0.0 0.0 0.0 0.0 0.0 1.437434250550272E15 0.0 0.0 0.0 2.59104440947307424E17 -0.0 0.0 0.0 -0.0 0.0 0.0 0.0), :total-error 7.2376408665942886E17}

;{:program (in1* exec_dup invert_colors edge_filter in1* in1* true false 1)
; , :errors (0.0 0.0 0.0 0.0 0.0 0.0 0.0 4.632222114615712E17 0.0 0.0 0.0 0.0 0.0 1.437434250550272E15 0.0 0.0 0.0 2.59104440947307424E17 -0.0 0.0 0.0 -0.0 0.0 0.0 0.0), :total-error 7.2376408665942886E17} 

(def bad-ind
  (prog-to-individual '(in1* exec_dup invert_colors edge_filter in1* true false 1)
                      ))

(def broken-empty-stack
  {:exec '(1 false true in1* in1* edge_filter invert_colors exec_dup in1*)
   :integer '(4 3 3 4)
   :image (list (load-image-resource "cars.jpg"))
   :input {:in1 (load-image-resource "arrow_up.jpg")}
   :bool '(true false)})


;;;;;;;;;;
;; Error Functions


(def target-image1
  (load-image-resource "exp.jpg"))

(def target-image2
  (load-image-resource "300dali2.jpg"))


(def target-image32
  (load-image-resource "32_insta.png"))

(def target-image100
  (load-image-resource "100_idk.png"))

;; GOOD
(defn abs
  "Returns the absolute value of a number x"
  [x]
  (if (< x 0)
    (*' -1 x)
    x))

(def test-cases-pixels
  (list (rest (get-pixels (first (load-images "arrow_up.jpg"))))))

(defn test-cases1
  []
  (load-images "arrow_up.jpg" "btnPlus.png"))

(defn test-cases2
  []
  (load-images "cars.jpg" "cars.jpg"))

(defn test-cases3
  []
  (load-images "300dali1.jpg" "300trippy.png"))


(defn test-cases32
  []
  (load-images "32_g+.png" "32_face.png" "32_twitter.png" "32_pin.png"))

(defn test-cases100
  []
  (map #(resize % 100 100)(load-images "300trippy.png" "300dali2.jpg" "300dali1.jpg" "100_fund.jpeg" "100_nbc.png" "100_soccer.png" "100_icons.jpeg" "100_house.png")))

(defn load-initial-state
  [state input-images]
  (loop [iter 0
         state state]
    (if (= iter (count input-images))
      state
      (recur (inc iter)
             (push-to-stack state :image (nth input-images iter))))))

(defn multiple-inputs
  [state lst]
  (loop [iter 0
         state state]
    (if (= iter (count lst))
      state
      (recur (+ 1 iter)
             (push-to-stack state :input (nth lst iter))))))

;; MAYBE
(defn evaluate-one-case
  "Evaluates a single case for regression error function"
  [individual initial-push-state input-images]
  (interpret-push-program (:program individual) initial-push-state))

(def test-ind
  (prog-to-individual '(in1* 1 integer_-*)))

(def bad-test-ind
  (prog-to-individual '(1 in1* in1* integer_-* in1* in1* integer_-* in1* in1* in1* integer_-* integer_-* integer_-*)))

;; MAYBE
;; PRolly useful comparing on line of the image, needs to be changed probably
(defn abs-difference-in-solution-lists
  "Computes the differences in the solutions for the input programs, returns errors list"
  ;; Ex: solution:(1 2 3 4), program solution:(4 4 4 4), output of this function: (3 2 1 0)
  [l1 l2]
  (if (zero? (count l2))
    (repeat (count l1) 100000)
    (loop [l1 l1
           l2 l2
           final '()]
      (if (= (count l1) 0)
        (reverse final)
        (recur (rest l1)
               (rest l2)
               (conj final (abs (- (first l1) (first l2)))))))))

;; MAYBE
;; Probably going to need to change
(defn get-solution
  "Gets the list of solution for a test case"
  [individual initial-push-state input-images]
  (evaluate-one-case individual initial-push-state input-images))
;  (map #(first (get % :image)) (map #(evaluate-one-case individual empty-push-state %) test-cases)))
  ;(map #(first (get % :image)) (map #(evaluate-one-case individual empty-push-state %) test-cases)))
  ;;(map #(if (zero? (count (:image %)))
    ;;         1000000 ;; Large penalty
      ;;       (get :image %)))
  ;;           (map #(evaluate-one-case individual empty-push-state %) test-cases))


(defn determinant-error
  "Returns the determinant error for a given matrix based off the ideal matrix"
  [A B]
  :STUB
  )


(defn need
  []
  (prog-to-individual (make-random-push-program init-instructions 20)))

(defn image-determinant
  [img]
  (m/det (image_to_matrix img)))


;; CITE: Taken from https://github.com/clojure/math.combinatorics. Could
;; have imported as a dependency, but I only needed the cartesian product
;; function, so I just copied and pasted here.
(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                      (if-let [rst (next (v-seqs i))]
                        (assoc v-seqs i rst)
                        (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn sectionalize
  "Splits an image into a list of 2x2 sections.  Does this by
  creating a range of starting coordinates for x and y, then taking
  the cartesian product of those two ranges to get a sequence of
  coords to start the sections at.  Can change the size of the sections
  by changing x-section-size and y-section-size."
  [img]
  (let [x-section-size (quot (width img) 10)
        y-section-size (quot (height img) 10)
        x-inds (range 0 (width img) x-section-size)
        y-inds (range 0 (height img) y-section-size)
        coords (cartesian-product x-inds y-inds)]
    (map #(sub-image img (first %) (last %) x-section-size y-section-size) coords)))


(defn sectionalize2
  [img]
  (let [wid (quot (width img) 5)
        hght (quot (height img) 5)]
    (loop [split_list '()
           x 0
           y 0]
      (if (>= y (* 5 hght))
        (reverse split_list)
        (if (>= x (* 5 wid))
          (recur split_list
                 0
                 (+' y hght))
          (recur (conj split_list (sub-image img x y wid hght))
                 (+' x wid)
                 y))))))

(defn test-case-list
  "Just a wrapper for getting the list of determinants for the target image.
  The resulting list will be used both in the error function and in lexicase
  selection."
  [target-image]
  (map image-determinant (sectionalize target-image)))

(def indddd
  (prog-to-individual example-push-program))

(def shit-prog
  (prog-to-individual '(in1* in1* section-xor section-xor invert_colors in1* section-xor exec_dup laplace_filter in2 in2 in2 hsplit_combine invert_colors laplace_filter invert_colors in1* in1* section-and in1* invert_colors invert_colors 1 invert_colors laplace_filter in1* invert_colors invert_colors invert_colors 1 exec_dup 1 laplace_filter hsplit_combine)
                      ))


;; NEED TO ACCOUNT FOR THERE NOT BEING AN IMAGE ONT HE STACK
(defn Euclidean-error-function
  [individual initial-push-state input-images target-image]
  (let [target-list (test-case-list target-image)
        result (peek-stack (get-solution individual initial-push-state input-images) :image)
        program-list (if (identical? result :no-stack-item)
                       (repeat (count target-list) 10000000000)
                       (test-case-list result)) ;; List solutions for given individual
        errors (abs-difference-in-solution-lists target-list program-list)]
    {:program (:program individual)
     :errors errors
     :total-error (reduce + errors)}))

(defn image-error-function
  [individual]
    {:program (:program individual)
     :errors 2
     :total-error 2})

;;;;;;;;;;;;
;; Reporting
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
  [population generation input-images]
  (println)
  (println "-------------------------------------------------------")
  (printf  "                    Report for Generation %s           " generation)
  (println)
  (println "-------------------------------------------------------")
  
  (let [best-prog (apply max-key #(get % :total-error) population)
        img (peek-stack (get-solution best-prog (load-initial-state empty-push-state (input-images)) input-images) :image)]
    (printf "Best program: ")
    (println (best-prog :program)) ;; Wanted to print the actual program, not just the location
    (println)
    (printf "Best program size: %s" (count (get best-prog :program)))
    (println)
    (printf "Best total error: %s" (get best-prog :total-error))
    (println)
    (printf "Best errors: %s" (get best-prog :errors))
    ;(show img :zoom 2.0)
    (write (resize img 1000 1000) (str "results/" (width img) "/" (new java.util.Date)   "_gen" generation ".png") "png" :quality 1.0 :progressive true)



    
    ))


(defn report3
  [population generation]
  (println)
  (println "-------------------------------------------------------")
  (printf  "                    Report for Generation %s           " generation)
  (println)
  (println "-------------------------------------------------------")
  )

(defn report2
  [population generation]
  (println population)
  (println)
  (println generation)
  (println)
  (println))



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
  [{:keys [population-size max-generations error-function instructions max-initial-program-size
           initial-push-state input-images target-image parent-select-fn]}]
  (loop [count 0
         population (map #(error-function % initial-push-state input-images target-image)
                         (init-population population-size max-initial-program-size instructions))]
    (report population count input-images)
    (if (>= count max-generations) ;; If we reach max-generations, null, otherwise keep going
      :nil
      (if (= 0 (get (apply min-key #(get % :total-error) population) :total-error)) ;; Anyone with error=0?
        :SUCCESS
        (recur (+ count 1) ;; Recur by making new population, and getting errors
               (map #(error-function (prog-to-individual %) initial-push-state input-images target-image)
                    (get-child-population
                     (map #(error-function % initial-push-state input-images target-image) population)
                     population-size 10 parent-select-fn))))))) ;; Using a fixed tournament size of 20 for quick conversion

;(defn profs
;  []
;  (map #(resize % 100 100) (load-images "mark.jpg" "rick.jpg" "campbell.jpg")))

;(def target-prof
;  (resize (load-image-resource "stu.jpg") 100 100))

(def one-prog
  (prog-to-individual '(section-xor section-or exec_dup exec_dup section-xor exec_dup exec_dup exec_dup true scramble_grid section-xor section-xor exec_dup section-or section-and section-and hsplit_combine section-and section-xor section-xor section-and hsplit_combine section-and exec_dup hsplit_combine hsplit_combine hsplit_combine section-xor hsplit_combine exec_dup exec_dup scramble_grid section-and section-or section-or hsplit_combine section-xor section-or section-or section-or section-xor section-or true section-xor section-or hsplit_combine section-or true true hsplit_combine exec_dup section-and section-and section-or section-or true section-or section-or hsplit_combine section-or exec_dup section-or exec_dup section-or hsplit_combine section-xor scramble_grid scramble_grid hsplit_combine exec_dup true hsplit_combine section-or section-or scramble_grid scramble_grid section-or section-or section-xor scramble_grid section-or section-and section-or section-and scramble_grid section-or section-or section-or section-and section-or scramble_grid section-and true hsplit_combine section-and section-or scramble_grid section-or section-and section-or section-and hsplit_combine section-or exec_dup exec_dup section-or section-or true exec_dup exec_dup section-and section-or section-and exec_dup section-or section-or section-and hsplit_combine hsplit_combine true section-xor section-or hsplit_combine exec_dup section-and section-or section-or exec_dup)
                      ))

;(show (peek-stack (get-solution one-prog (load-initial-state empty-push-state (test-cases3)) test-cases3) :image))




;; This does some funky stuff but it works

;; I guess we have ot load the image as a buffered image, grab its pixels into a separate variable, and then manipulate that pixel list, and then set it using the set-pixels

;; Basically load the image we want
(def bi (first (load-images "cars.jpg")))
(def bi2 (first (load-images "arrow_up.jpg")))


;; gets the pixels of the image, as an int array
(def pixels (get-pixels bi))

;; fill some random pixels with colours
;; I guess this sets first 10  pixels with random colors but im not sure
(dotimes [i (count pixels)]
  (aset pixels i (rand-colour))) ;; sets a index in a list to a random color, like -3226114

;; update the image with the newly changed pixel values
(set-pixels bi pixels)

;; view our new work of art
;; the zoom function will automatically interpolate the pixel values
;(show bi2 :zoom 10.0 :title "Isn't it beautiful?")

;;(show (peek-stack (evaluate-one-case (prog-to-individual (make-random-push-program init-instructions 20)) empty-push-state (first test-cases)) :image))

(defn -main
  [& args]
  (let [input-images test-cases100
        target-image target-image100]
    (binding [*ns* (the-ns 'genetic_art.core)]
    (push-gp {:instructions init-instructions
              :error-function Euclidean-error-function
              :max-generations 20
              :population-size 20
              :max-initial-program-size 30
              :initial-push-state (load-initial-state empty-push-state (input-images))
              :input-images input-images
              :target-image target-image
              :parent-select-fn lexicase-selection}))))
