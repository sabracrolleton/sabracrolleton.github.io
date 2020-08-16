---
layout: default
title: Comparing Iterate v. Loop using Examples
---
# Table of Contents

- [Comparison of Iterate v. Loop using examples](#org99f9fa9)
- [Quick Summary](#org72510fd)
  - [Basic in list](#org8231028)
    - [Multiple Local Variables](#org43865fe)
- [Local Variables](#org4571c90)
  - [Destructuring](#orgc52fc08)
  - [As](#orgd32f55c)
  - [Variable Binding](#org4111c54)
    - [With](#org0559dea)
  - [Files and Streams](#org08f6a54)
  - [Package-Symbols](#org59de543)
- [Iteration Control](#org6fdefc7)
  - [While](#orgfc29918)
  - [Until (Early termination)](#orgf18f2d6)
  - [When](#orgcde6503)
  - [If - Else](#org23946d5)
  - [In](#orgc6d3da7)
  - [By](#org9ce1253)
  - [Below](#orgb6031f6)
  - [Above](#org3cf80af)
  - [Then](#org60c7f9a)
  - [On](#orgc3ac8a5)
  - [From](#org60ea0f4)
  - [Downfrom and Upfrom](#org1fed44a)
  - [Downto and upto](#orgbe1c786)
  - [Across](#org2356ee3)
  - [Collect](#org19ede00)
  - [From x to y](#orge7984ab)
  - [Initially](#orga9068cb)
  - [Always](#org7306718)
  - [Never](#org629e577)
  - [Thereis](#org66155c0)
  - [Named](#org609e91f)
- [Different Kinds of Sequences](#orgc224681)
  - [Strings](#org884603d)
  - [Sequences](#org66be94a)
  - [Vectors](#org29f7fe9)
    - [Index of Vectors](#org8100fac)
  - [Hash tables](#org8f7a647)
    - [Hash-key](#orga0bd77e)
    - [Hash-value](#org8e44c10)
    - [Key-Value Pair](#org0b168fb)
    - [Hash-keys](#org27d63fe)
- [Declaring Variable Types](#orgd565f2e)
- [Return Value Structures](#org7713146)
  - [Append/Appending](#org0a1defa)
  - [Unioning](#orgfbc7ec3)
  - [Nconcing](#orgd69ed67)
  - [Examples from Land of lisp](#org626ad8b)
  - [Finally](#org63f9c85)
- [New and Wonderful](#org4aa120d)
  - [Previous Values](#org96a3b2a)
  - [Generators](#org6a2baac)
  - [New Drivers](#org3f78f38)


<a id="org99f9fa9"></a>

# Comparison of Iterate v. Loop using examples

Learning lisp, I learned loop. It looks a bit strange, but there are plenty of places on the internet to view examples because it is in the language. See, e.g. <http://www.ai.sri.com/pkarp/loop.html>. <http://www.unixuser.org/~euske/doc/cl/loop.html>, <http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node235.html>, <http://www.gigamonkeys.com/book/loop-for-black-belts.html>, <http://sandbox.mc.edu/~bennet/cs231/examples/loops.html>, etc. Occasionally I would see complaints and suggestions to use iterate (usually shortened to iter) instead. See, e.g. <http://items.sjbach.com/211/comparing-loop-and-iterate>, <http://common-lisp.net/project/iterate/doc/Don_0027t-Loop-Iterate.html>. Then I started running into libraries in quicklisp that actually used iter and, in order to understand the libraries, I needed to learn iter.

So, what do you do if you are me? Compare the two. So, I do.


<a id="org72510fd"></a>

# Quick Summary

Loop is in the language. You do not have to add a library. It is also fractionally faster. On the other hand&#x2026;.. iter is better at everything else. And I mean everything else.


<a id="org8231028"></a>

## Basic in list

```lisp
(loop for i in '(0 2 4 555 6)
      do (format t " ~a " i))
 0  2  4  555  6

(iter (for i in '(0 2 4 555 6))
      (format t " ~a " i))
```

We already know that loop and iter create locally scoped variables; i in the above case.

Looking at the code, Iter puts parens around the for clause and drops the "do". Looks more lispy and I often catch myself forgetting to type the "do" in loop. One person referred to loop in lisp as half lisp, half pascal.

In timing this basic loop, loop seemed to be 3% faster on my sbcl box than iter. Is iter worth it?


<a id="org43865fe"></a>

### Multiple Local Variables

```lisp
(loop for i from 0
      for el in '(a b c d e f)
      collect (cons i el))
=> ((0 . A) (1 . B) (2 . C) (3 . D) (4 . E) (5 . F))

(iter (for i from 0)
      (for el in '(a b c d e f))
      (collect (cons i el)))
=> ((0 . A) (1 . B) (2 . C) (3 . D) (4 . E) (5 . F))
```


<a id="org4571c90"></a>

# Local Variables

This next group actually creates three local variables, looping through a list and assigning each member sequentially to the variable x, creating a local variable y which is calculated based on the value of x and finally local variable z which is counting where we are in the loop.

```lisp
(loop for x in '(1 4 9 13)
      for y = (* x 2)
      counting x into z
      collect (list x y z))
((1 2 1) (4 8 2) (9 18 3) (13 26 4))

(iter (for x in '(1 4 9 13))
      (for y = (* x 2))
      (counting x into z)
      (collect (list x y z)))
((1 2 1) (4 8 2) (9 18 3) (13 26 4))

(loop for i
      in '(3 8 73 4 -5)
      minimize i into smallest
      maximize i into biggest
      finally
      (return
       (values smallest biggest)))
-5
73

(iter (for i in '(3 8 73 4 -5))
      (minimize i into smallest)
      (maximize i into biggest)
      (finally (return (values smallest biggest))))
```

\#+END<sub>SRC</sub>


<a id="orgc52fc08"></a>

## Destructuring

Destructuring is the act of binding a set of variables to a set of values. Loop and iter allow you to do this in parallel with each iteration.

```lisp
(loop for (a b) on '(1 3 5 2 7 9)
      collect  (list a b))
((1 3) (3 5) (5 2) (2 7) (7 9) (9 NIL))

(iter (for (a b) on '(1 3 5 2 7 9))
      (collect  (list a b)))
((1 3) (3 5) (5 2) (2 7) (7 9) (9 NIL))

(loop for (a b) in '((1 3) (5 18) (9 -3))
      collect  (list b a))
((3 1) (18 5) (-3 9))

(iter (for (a b)  in '((1 3) (5 18) (9 -3)))
      (collect  (list b a)))
((3 1) (18 5) (-3 9))

(loop for (a b) on '(1 3 5 2 7 9) while b
      collect (+ a b))
(4 8 7 9 16)

(iter (for (a b) on '(1 3 5 2 7 9)) (while b)
      (collect (+ a b)))
(4 8 7 9 16)

(loop for (x . y) in '((1 . 1) (2 . 4) (3 . 9))
      collect y)

(1 4 9)
(iter (for (x . y) in '((1 . 1) (2 . 4) (3 . 9)))
      (collect y))

(1 4 9)
```


<a id="orgd32f55c"></a>

## As

```
(loop as x from 5 to 10
      collect x)
(5 6 7 8 9 10)

(iter (as x from 5 to 10)
      (collect x))
(5 6 7 8 9 10)
```


<a id="org4111c54"></a>

## Variable Binding


<a id="org0559dea"></a>

### With

```lisp
(loop with x = (+ 1 2) repeat 5
      do (format t "~a " x))
3 3 3 3 3

(iter (with x = (+ 1 2))
      (repeat 5)
      (format t "~a " x))
3 3 3 3 3
```


<a id="org08f6a54"></a>

## Files and Streams

Iter can iterate over the contents of a file or stream using the "in-file" or "in-stream" clause.


<a id="org59de543"></a>

## Package-Symbols

Did you know that you can loop through package symbols?

```lisp
(loop for x being the  external-symbols in :iter do (format t "~a " x))
MULTIPLY DISPLAY-ITERATE-CLAUSES LEAVE APPENDING THEREIS UNTIL COLLECT
IF-FIRST-TIME TERMINATE NEVER ACCUMULATE ACCUMULATING INITIALLY MULTIPLYING
DEFMACRO-DRIVER MAXIMIZE ELSE MINIMIZING REDUCING ITERATE AFTER-EACH FIRST-TIME-P
AS UNIONING FINALLY-PROTECTED NEXT FINISH DEFSYNONYM DSETQ NUNIONING COLLECTING
NEXT-ITERATION ITER GENERATE MAXIMIZING DEFCLAUSE-SEQUENCE FINALLY ADJOINING
NCONCING WHILE DECLARE-VARIABLES MINIMIZE COUNTING FIRST-ITERATION-P FINDING
SUM FOR REPEAT WITH SUMMING DEFMACRO-CLAUSE IN GENERATING ALWAYS

(iter (for x in-package :iter external-only t)
      (format t "~a " x))
MULTIPLY DISPLAY-ITERATE-CLAUSES LEAVE APPENDING THEREIS UNTIL COLLECT
IF-FIRST-TIME TERMINATE NEVER ACCUMULATE ACCUMULATING INITIALLY MULTIPLYING
DEFMACRO-DRIVER MAXIMIZE ELSE MINIMIZING REDUCING ITERATE AFTER-EACH FIRST-TIME-P
AS UNIONING FINALLY-PROTECTED NEXT FINISH DEFSYNONYM DSETQ NUNIONING COLLECTING
NEXT-ITERATION ITER GENERATE MAXIMIZING DEFCLAUSE-SEQUENCE FINALLY ADJOINING
NCONCING WHILE DECLARE-VARIABLES MINIMIZE COUNTING FIRST-ITERATION-P FINDING
SUM FOR REPEAT WITH SUMMING DEFMACRO-CLAUSE IN GENERATING ALWAYS
```


<a id="org6fdefc7"></a>

# Iteration Control


<a id="orgfc29918"></a>

## While

(loop for i in '(0 2 4 555 6) while (evenp i) do (format t " ~a " i)) 0 2 4

(iter (for i in '(0 2 4 555 6)) (while (evenp i)) (format t " ~a " i)) 0 2 4


<a id="orgf18f2d6"></a>

## Until (Early termination)

```lisp
(loop for i in '(1 2 4 555 6)
      until (evenp i)
      do (format t " ~a " i))
 1

(iter (for i in '(1 2 4 555 6))
      (until (evenp i))
      (format t " ~a " i))
 1
```


<a id="orgcde6503"></a>

## When

Look at one of the locally scoped variables and do something if the when clause is true:

```lisp
(loop for x in '(1 3 4 23 22 8)
                    when (evenp x)
                         do (format t "Even: ~a " x)
                    when (oddp x)
                         do (format t "Odd: ~a " x))
Odd: 1 Odd: 3 Even: 4 Odd: 23 Even: 22 Even: 8

(iter (for x in '(1 3 4 23 22 8))
                    (when (evenp x)
                          (format t "Even: ~a " x))
                    (when (oddp x)
                          (format t "Odd: ~a " x)))
Odd: 1 Odd: 3 Even: 4 Odd: 23 Even: 22 Even: 8

```


<a id="org23946d5"></a>

## If - Else

```lisp
(loop for x in '(1 3 4 23 22 8)
                    if (evenp x)
                    do (format t "Even: ~a " x)
                    else if (oddp x)
                    do (format t "Odd: ~a " x))
Odd: 1 Odd: 3 Even: 4 Odd: 23 Even: 22 Even: 8

(iter (for x in '(1 3 4 23 22 8))
                    (if (evenp x)
                        (format t "Even: ~a " x)
                        (if (oddp x)
                        (format t "Odd: ~a " x))))
Odd: 1 Odd: 3 Even: 4 Odd: 23 Even: 22 Even: 8
```

Ok. That works, but we did not use "else" in the iter version. What happens if we use "else" in the iter version?

```lisp
(iter (for x in '(1 3 4 23 22 8))
                    (if (evenp x)
                        (format t "Even: ~a " x)
                        (else (if (oddp x)
                                  (format t "Odd: ~a " x)))))
Even: 4 Even: 22 Even: 8
```

Ok. I did not expect that. What happened? <http://common-lisp.net/project/iterate/doc/Problems-with-Code-Movement.html#Problems-with-Code-Movement> explains that "else" and a few other terms are subject to code movement resulting in variable shadowing problems. Let's try something.

```lisp
(macroexpand-1 '(iter (for x in '(1 3 4 23 22 8))
                      (if (evenp x) (format t "Even: ~a " x)
                          (else (format t "In Else: ~a " x)))))
(LET* ((#:LIST344 NIL) (X NIL) (#:ELSE345 T))
  (BLOCK NIL (TAGBODY (PROGN (SETQ #:LIST344 (QUOTE (1 3 4 23 22 8))))
                      LOOP-TOP-NIL
                      (PROGN (IF (ENDP #:LIST344) (GO LOOP-END-NIL))
                             (SETQ X (CAR #:LIST344))
                             (SETQ #:LIST344 (CDR #:LIST344))
                             (IF (EVENP X) (FORMAT T "Even: ~a " X)
                                           (SETQ #:ELSE345 NIL)))
                      (PROGN) (GO LOOP-TOP-NIL)
                              LOOP-END-NIL
                              (PROGN (WHEN #:ELSE345 (FORMAT T "In Else: ~a " X))))
         NIL))
```

Even though it initially looked like the else clause was within the if clause, it really was not, the evenp test set else to nil and the else function did not get called. Consider the following from the iterate test suite:

```lisp
(iter (for i below -3)
    (else (return 2)))
2
```

According to the documentation, the &rest forms following else are placed in the epilogue section of the loop where they are executed if this else clause is never met during execution of the loop and the loop terminates normally. Basically, in iter, do no think that the else clause has anything to do with an explicit if clause earlier in the function. It really has a different meaning.

```lisp
(iter (for x in '(1 3 4 23 22 8))
                    (if (evenp x) (format t "Even: ~a " x)
                        (format t "Odd: ~a " x)))
Odd: 1 Odd: 3 Even: 4 Odd: 23 Even: 22 Even: 8
```

We get the expected result.


<a id="orgc6d3da7"></a>

## In

```lisp
(loop for i in '(100 20 3) sum i)
123

(iter (for i in '(100 20 3))
      (sum i))
123
```


<a id="org9ce1253"></a>

## By

```lisp
(loop for i from 6 to 8 by 2
      sum i)
14

(iter (for i from 6 to 8 by 2)
      (sum i))
14
```

You do not need to increment or decrement by whole integers

```lisp
(loop for i from 3.0 downto 1.0 by 0.5
      do (print i))

(iter (for i from 3.0 downto 1.0 by 0.5)
      (print i))
```

Note that loop can also use the term "upto" here, but iter does not have that synonym.


<a id="orgb6031f6"></a>

## Below

```lisp
(loop for i from 1 below 5 by 2
      do (print i))
1
3

(iter (for i from 1 below 5 by 2)
      (print i))
```


<a id="org3cf80af"></a>

## Above

```lisp
(loop for i from 3 above 1
      do (print i))
3
2

(iter (for i from 3 above 1)
      (print i))
```


<a id="org60c7f9a"></a>

## Then

We have a bit more of a difference in how to use the term "then". Loop can use an equal sign as the initial assignment, but iter uses the term "initially" instead.

```lisp
(loop repeat 5 for x = 10.0 then (/ x 2)
      collect x)
(10.0 5.0 2.5 1.25 0.625)

(iter (repeat 5)
      (for x initially 10.0 then (/ x 2))
      (collect x))
(10.0 5.0 2.5 1.25 0.625)
```


<a id="orgc3ac8a5"></a>

## On

In other words, the variable is set to successive sublists of the list provided.

```lisp
(loop for i on '(1 2 3)
      do (print i))
(1 2 3)
(2 3)
(3)

(iter (for i on '(1 2 3))
      (print i))
```


<a id="org60ea0f4"></a>

## From

```lisp
(loop as x from 5 to 10 collect x)
(5 6 7 8 9 10)

(iter (as x from 5 to 10)
      (collect x))
(5 6 7 8 9 10)
```


<a id="org1fed44a"></a>

## Downfrom and Upfrom

Important: iter's use of downfrom and upfrom does not allow an explicit end, so you must use a return clause. E.g.

```lisp
(loop for i downfrom 10 to 8
      do (print i))

(iter (for i downfrom 10)
      (print i)
      (when (= 8 i) (return "i")))

10
9
8
(loop for i upfrom 10 to 12
      do (print i))

(iter (for i upfrom 10)
      (print i)
      (when (= 12 i)
        (return "i")))
```


<a id="orgbe1c786"></a>

## Downto and upto

Loop uses "downto" and "upto" but while iter uses "downto" because of the directionality, iter just uses "to" for normal iteration.

```lisp
(loop for i from 10 downto 8
      do (print i))

(iter (for i from 10 downto 8)
      (print i))

(loop for i from 10 upto 12
      do (print i))

(iter (for i from 10 to 12)
      (print i))
```

Both can control the increments by the term "by".


<a id="org2356ee3"></a>

## Across

Loop uses the term "across" to iterate through arrays. Iter uses "in-vector".

```lisp
(loop for i across #(100 20 3)
      sum i)
123

(iter (for i in-vector #(100 20 3))
      (sum i))

```


<a id="org19ede00"></a>

## Collect

```lisp
(loop for x in '( 1 3 5 )
      collect (+ 1 x))
(2 4 6)

(iter (for x in '( 1 3 5 ))
      (collect (+ 1 x)))
(2 4 6)
```


<a id="orge7984ab"></a>

## From x to y

```lisp
(loop for i from 0 to 20
      if (oddp i)
        collect i into odds
      else
        collect i into evens
      finally (return (values evens odds)))
=> (0 2 4 6 8 10 12 14 16 18 20)
   (1 3 5 7 9 11 13 15 17 19)

;; The equivalent in ITERATE
(iter (for i from 0 to 20)
      (if (oddp i)
          (collect i into odds)
          (collect i into evens))
      (finally (return (values evens odds))))
=> (0 2 4 6 8 10 12 14 16 18 20)
   (1 3 5 7 9 11 13 15 17 19)
```

loop could also do: for i upto 20 iter is explicit: for i from 0 to 20


<a id="orga9068cb"></a>

## Initially

```lisp
(loop initially
      (format t "~a" 'loop-begin)
      for x below 3
      do (format t "~a " x))
LOOP-BEGIN0 1 2

(iter (initially (format t "~a" 'loop-begin))
      (for x below 3)
      (format t "~a " x))
LOOP-BEGIN0 1 2
```


<a id="org7306718"></a>

## Always

```lisp
(loop for x in '(foo 2)
      always (numberp x))
NIL

(iter (for x in '(foo 2))
      (always (numberp x)))
```


<a id="org629e577"></a>

## Never

```lisp
(loop for x in '(foo 2)
      never (numberp x))
NIL

(iter (for x in '(foo 2))
      (never (numberp x)))
```


<a id="org66155c0"></a>

## Thereis

```lisp
(loop for x in '(foo 2)
      thereis (numberp x))
T

(iter (for x in '(foo 2))
      (thereis (numberp x)))
```


<a id="org609e91f"></a>

## Named

```lisp
(loop named outer for i below 10
      do (progn (format t "~a" "outer")
             (loop named inner for x below i
                   do (format t " ~a ~%" " **inner ~%")
                   when (= x 2) do
                   (return-from outer 'kicked-out-all-the-way))))

outerouter  **inner ~%
outer  **inner ~%
  **inner ~%
outer  **inner ~%
  **inner ~%
  **inner ~%
KICKED-OUT-ALL-THE-WAY

(iter outer (for i below 10)
      (progn (format t "~a" "outer")
             (iter inner (for x below i)
                   (format t " ~a ~%" " **inner ~%")
                   (when (= x 2)
                     (return-from outer 'kicked-out-all-the-way)))))

outerouter  **inner ~%
outer  **inner ~%
  **inner ~%
outer  **inner ~%
  **inner ~%
  **inner ~%
KICKED-OUT-ALL-THE-WAY
```

I have to admit after using lisp for awhile, that naked "when" without parentheses in the loop version makes me uncomfortable.


<a id="orgc224681"></a>

# Different Kinds of Sequences


<a id="org884603d"></a>

## Strings

```lisp
(iter (for i in-string "forsooth the years break" )
      (format t "~a " i))
f o r s o o t h   t h e   y e a r s   b r e a k
```


<a id="org66be94a"></a>

## Sequences

```lisp
(iter (for i in-sequence "forsooth the years break" )
      (format t "~a " i))
f o r s o o t h   t h e   y e a r s   b r e a k
NIL
(iter (for i in-sequence '(2 5 89)) (format t "~a " i))
2 5 89
```


<a id="org29f7fe9"></a>

## Vectors

```lisp
(loop for i across #(1 2 3) do
      (print i))

(iter (for i in-vector  #(1 2 3))
      (format t "~a " i))
```

Note the change from "across" to "in-vector".


<a id="org8100fac"></a>

### Index of Vectors

```lisp
(iter (for i index-of-vector  #(1 2 3))
      (format t "~a " i))
0 1 2
```


<a id="org8f7a647"></a>

## Hash tables

Hash tables are where everyone is disgusted by loop.

Assume the following example: (defparameter **currencies** (make-hash-table)) (setf (gethash 'Italy **currencies**) 'Euro) (setf (gethash 'Sweden **currencies**) 'Krona)


<a id="orga0bd77e"></a>

### Hash-key

```lisp
(loop for x being each hash-key of *currencies*
      do (format t "~a " x))
ITALY SWEDEN

(iter (for (x y) in-hashtable *countrysummary-daos*)
      (format t "~a " x))
```


<a id="org8e44c10"></a>

### Hash-value

```lisp
(loop for v being the hash-value of h
      do (print v))

(iter (for (x y) in-hashtable *countrysummary-daos*)
      (print y))
```


<a id="org0b168fb"></a>

### Key-Value Pair

```
(loop for k being the hash-key using (hash-value v) of h
      do (format t "~a ~a~%" k v))

(iter (for (x y) in-hashtable *countrysummary-daos*)
      (format t "~a ~a" x y))

```


<a id="org27d63fe"></a>

### Hash-keys

1.  Using

    ```lisp
    (loop for key being the hash-keys in *countrysummary-daos*
              using (hash-value val)
          do (format t "~a ~a" key val))

    (iter (for (x y) in-hashtable *countrysummary-daos*)
          (format t "~a ~a" x y))
    ```

2.  The

    ```lisp
    (loop for x being the hash-keys of *countrysummary-daos*
          do (format t "~a" x))

    (iter (for (x y) in-hashtable *countrysummary-daos*)
          (format t "~a" x))
    ```

    In hashtable situations, iter looks more rational.


<a id="orgd565f2e"></a>

# Declaring Variable Types

You can declare the local variables to be of specific types in order to ensure that the variable is correctly typed.

```lisp
(loop for i of-type fixnum in '(1 3.1 4) counting (oddp i) into x
      do (print x))
;;Throws an error because 3.1 is not a fixnum
(iter (for el in '(1 3 4))
        (declare (fixnum el))
        (counting (oddp el)))
2
```


<a id="org7713146"></a>

# Return Value Structures


<a id="org0a1defa"></a>

## Append/Appending

Here there is a difference in that loop works with either "append" or "appending" but iter works only with "appending".

```lisp
(loop for i from 1 to 3 appending (list i i))
(1 1 2 2 3 3)

(iter (for i from 1 to 3) (appending (list i i)))
(1 1 2 2 3 3)

(iter (for l on '(1 2 3))
    (appending l :at #:end)
    (collect (first l)))
(1 2 3 1 2 3 2 3 3)

(iter (for l on '(1 2 3))
    (appending l into x))
NIL
```

But

```lisp
(loop for i from 1 to 3 append (list i i))
(1 1 2 2 3 3)

(iter (for i from 1 to 3) (append (list i i)))
NIL
```


<a id="orgfbc7ec3"></a>

## Unioning

```lisp
(iter (for l on '(a b c))
    (unioning l)
    (collect (first l)))
(A B C A B C)

(iter (for l on '(a b c))
    (collecting (first l))
    (unioning l :test #'eql))
(A B C B C)

(iter (for l in-vector '#("a" "A" "aB" "ab" "AB"))
    (unioning (list l) :test #'string-equal))
("a" "aB")

(iter (for l in-vector '#("a" "A" "aB" "ab" "AB"))
    (nunioning (list l) :test #'string-equal :at :start))
("aB" "a")
```


<a id="orgd69ed67"></a>

## Nconcing

```lisp
(iter (for l on (list 1 2 3))
    (collect (first l))
    (nconcing (copy-list l) at end))
(1 1 2 3 2 2 3 3 3)
```


<a id="org626ad8b"></a>

## Examples from Land of lisp

```lisp
(loop repeat 5 do (print "X"))
(iter (repeat 5) (print "X"))

(loop for i from 5 to 10 sum i)
45
(iter (for i from 5 to 10) (sum i))
45
(loop for i in '(100 20 3) sum i)
123
(iter (for i in '(100 20 3)) (sum i))
123
(loop for i below 5 do (print i))

(iter (for i below 5) (print i))

(loop for i below 10 when (oddp i) sum i)
25
(iter (for i below 10) (when (oddp i) (sum i)))
25
(loop for i from 0 do (print i) when (= i 5) return 'falafel)

(iter (for i from 0) (print i) (when (= i 5) (return 'falafel)))

(loop for i in '(2 3 4 5 6) collect (* i i))

(iter (for i in '(2 3 4 5 6)) (collect (* i i)))

(loop for x below 10 for y below 10 collect (+ x y))
(0 2 4 6 8 10 12 14 16 18)
(iter (for x below 10) (for y below 10) (collect (+ x y)))
(0 2 4 6 8 10 12 14 16 18)
(loop for x below 10 collect (loop for y below 10 collect (+ x y)))

(iter (for x below 10) (collect (iter (for y below 10) (collect (+ x y)))))

(loop for i below 5 if (oddp i) do (print i) else do (print "w00t"))

(iter (for i below 5) (if (oddp i) (print i) (print "w00t")))

(loop for i from 10 downto 7 do (print i))
(iter (for i from 10 downto 7) (print i))

```


<a id="org63f9c85"></a>

## Finally

```lisp
(loop for x below 3
      do (format t "~a " x)
      finally
      (format t "~a " 'loop-end))
0 1 2 LOOP-END

(iter (for x below 3)
      (format t "~a " x)
      (finally
       (format t "~a " 'loop-end)))
0 1 2 LOOP-END
```


<a id="org4aa120d"></a>

# New and Wonderful


<a id="org96a3b2a"></a>

## Previous Values

You can get to previous values in the loop? Well, in iter, yes, using the term "previous". Consider the following:

```lisp
(iter (for el in '(1 2 3 4))
             (for pp-el previous el back 2 initially 0)
             (collect pp-el))
(0 0 1 2)
```

You do need to set the previous-element (in this case pp-el) to an initial setting and the argument to back must be a constant positive integer and defaults to 1.


<a id="org6a2baac"></a>

## Generators

Generators are a feature of iterate and have no analogue in loop. As described in <http://items.sjbach.com/280/extending-the-iterate-macro>, a generator is a lazy evaluated variable. It will only change when directed to do so by a next clause. If you forget the next clause, it will never change (and you are looking for a specific change to get you out of the iteration, you will be waiting forever).

Here is an example:

```lisp
(iter (for i in '(1 2 3 4 5))
      (generate c in-string "black")
      (if (oddp i) (next c))
      (format t "~a " c))
b b l l a
```


<a id="org3f78f38"></a>

## New Drivers

Would you like to iterate over the leaves of a tree? You can write a new driver for iterate that will do that. Again, thank you Stephen Bach for the example.

```lisp
(iter:defmacro-driver (FOR leaf IN-TREE tree)
  "Iterate over the leaves in a tree"
  (let ((gtree (gensym))
        (stack (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,gtree = ,tree)
       (with ,stack = (list ,gtree))
       (,kwd ,leaf next
             (let ((next-leaf
                    (iter (while ,stack)
                          (for node = (pop ,stack))
                          (if (consp node)
                              (destructuring-bind (l . r)
                                  node
                                (unless (endp r)
                                  (push r ,stack))
                                (push l ,stack))
                              (return node)))))
               (or next-leaf (terminate)))))))

(iter (for leaf in-tree '(((2 3) (5) 1) 8 (4 (1 (2)) 2) 3))
      (collect leaf into leaves)
      (multiply leaf into product)
      (finally (return (values leaves product))))
(2 3 5 1 8 4 1 2 2 3)
11520
```

Can you do that in loop? Ok. Maybe not.

Sabra
