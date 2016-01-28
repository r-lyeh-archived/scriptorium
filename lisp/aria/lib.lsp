(do

  ; core

  (= caar (fn (x) (car (car x))))
  (= cadr (fn (x) (car (cdr x))))
  (= cddr (fn (x) (cdr (cdr x))))
  (= cdar (fn (x) (cdr (car x))))

  (= dostring (fn (str name)
    (default name "(string)")
    (eval (parse str name) global)))

  (= dofile (fn (filename)
    (dostring (loads filename) filename)))

  (= not (fn (x)
    (is x nil)))

  (= isnt (fn (a b)
    (not (is a b))))

  (= isa (fn (x y)
    (is (type x) y)))

  (= when (macro (x . body)
    (list if x (cons do body))))

  (= unless (macro (x . body)
    (list if (list not x) (cons do body))))

  (= whenlet (macro (x . body)
    (list let x
      (list if (car x) (cons do body)))))

  (= ++ (macro (x n)
    (list = x (list + x (list or n 1)))))

  (= -- (macro (x n)
    (list = x (list - x (list or n 1)))))

  (= default (macro (x val)
    (list = x (list or x val))))

  (= assert (fn (val msg)
    (if (not val)
        (error (or msg "assertion failed")))
        val))

  (= gensym (let (x 0)
    (fn ()
      (parse (string "G#" (++ x))))))


  ; math

  (= rand (let (seed 0)
    (fn (n)
      (= seed (mod (+ (* seed 196561) 1374) 2147483647))
      (if n (mod seed n) (/ seed 2147483647)))))

  (= abs (fn (n)
    (if (< n 0) (- 0 n) n)))

  (= floor (fn (n)
    (- n (mod n 1))))

  (= min (fn args
    (reduce (fn (a b) (if (< a b) a b)) args)))

  (= max (fn args
    (reduce (fn (a b) (if (> a b) a b)) args)))


  ; loop

  (= until (macro (x . body)
    (cons while (cons (list not x) body))))

  (= times (fn (n f)
    (let (i 0)
      (while (< i n)
        (f i)
        (++ i)))))


  ; list

  (= nth* (fn (n lst)
    (if (>= n 0) (do
      (while (> n 0)
        (= lst (cdr lst)
           n (- n 1)))
      lst))))

  (= nth (fn (n lst)
    (car (nth* n lst))))

  (= len (fn (lst)
    (let (res 0)
      (while lst
        (= res (+ res 1)
           lst (cdr lst)))
      res)))

  (= take (fn (n lst)
    (collect (fn (p)
      (while (and lst (> n 0))
        (p (car lst))
        (= lst (cdr lst))
        (-- n))))))

  (= reverse (fn (lst)
    (let (res nil)
      (each (fn (c) (= res (cons c res))) lst)
      res)))

  (= choice (fn (lst)
    (nth (rand (len lst)) lst)))

  (= each (fn (f lst)
    (while lst
      (f (car lst))
      (= lst (cdr lst)))))

  (= concat (fn args
    (collect (fn (p)
      (each (fn (c)
        (each p c)) args)))))

  (= map (fn (f lst)
    (collect (fn (p)
      (each (fn (c)
        (p (f c))) lst)))))

  (= filter (fn (f lst)
    (collect (fn (p)
      (each (fn (c)
        (if (f c) (p c))) lst)))))

  (= reject (fn (f lst)
    (collect (fn (p)
      (each (fn (c)
        (if (not (f c)) (p c))) lst)))))

  (= count (fn (f lst)
    (let (n 0)
      (each (fn (x)
        (if (f x) (= n (+ n 1)))) lst)
      n)))

  (= any (fn (f lst)
    (if (find* f lst) t)))
            
  (= all (fn (f lst)
    (let (res t)
      (while lst
        (if (not (f (car lst))) 
          (= res nil
             lst nil)
          (= lst (cdr lst))))
      res)))

  (= find* (fn (f lst)
    (let (res nil)
      (while lst
        (if (f (car lst))
          (= res lst
             lst nil)
          (= lst (cdr lst))))
      res)))
        
  (= find (fn (f lst)
    (car (find* f lst))))

  (= pos (fn (val lst)
    (let (i 0 found nil)
      (while lst
        (if (is (car lst) val)
          (= found t
             lst nil)
          (do (= lst (cdr lst))
              (++ i))))
      (if found i))))

  (= has (fn (val lst)
    (let (res nil)
      (while lst
        (if (is (car lst) val)
          (= lst nil
             res t)
          (= lst (cdr lst))))
      res)))

  (= reduce (fn (f lst)
    (let (res (car lst))
      (= lst (cdr lst))
      (while lst
        (= res (f res (car lst))
           lst (cdr lst)))
      res)))

  (= collect (fn (f)
    (let (res (cons) x res)
      (f (fn (val)
        (setcdr x (cons val))
        (= x (cdr x))))
      (cdr res))))

  (= range (fn (lo hi)
    (unless hi (= hi lo
                  lo 0))
    (let (res nil)
      (while (>= (-- hi) lo)
        (= res (cons hi res)))
      res)))

  (= push (macro (val sym)
    (list = sym (list cons val sym))))

  (= pop (macro (sym)
    (let (x (gensym))
      (list let (list x (list car sym))
        (list = sym (list cdr sym))
        x))))


  ; association list

  (= alist (fn x
    (let (res nil)
      (while (car x)
        (= res (alcons (car x) (cadr x) res)
           x (cddr x)))
      res)))

  (= alget (fn (key lst)
    (while (and lst (isnt (caar lst) key))
      (= lst (cdr lst)))
    (car lst)))

  (= alref (fn (key lst)
    (cdr (alget key lst))))

  (= alset (fn (key val lst)
    (let (x (alget key lst))
      (if x (do (setcdr x val) lst)
            (alcons key val lst)))))

  (= aldel (fn (key lst)
    (if (is (caar lst) key)
      (cdr lst)
      (let (x lst)
        (while x
          (if (is (caar (cdr x)) key)
            (do (setcdr x (cddr x))
                (= x nil))
            (= x (cdr x))))
        lst))))

  (= alcons (fn (key val lst)
    (cons (cons key val) lst)))


  ; string
 
  (= join (fn (lst sep)
    (default sep "")
    (apply string
      (let (x lst)
        (collect (fn (p)
          (while x
            (if (isnt x lst) (p sep))
            (p (car x))
            (= x (cdr x)))))))))
    
  (= split (fn (str delim)
    (default delim " ")
    (collect (fn (p)
      (let (offset 0 x nil)
        (while str
          (= x (strpos str delim offset))
          (if x (do
                  (p (substr str offset (- x offset)))
                  (= offset (+ x (strlen delim))))
                (do
                  (p (substr str offset))
                  (= str nil)))))))))

  (= replace (fn (str old new)
    (join (split str old) new)))

  (= ltrim (fn (str chr)
    (default chr " ")
    (let (i 0)
      (while (is (substr str i 1) chr)
        (++ i))
      (substr str i))))

  (= rtrim (fn (str chr)
    (default chr " ")
    (let (i (- (strlen str) 1))
      (while (is (substr str i 1) chr)
        (-- i))
      (substr str 0 (+ i 1)))))

  (= trim (fn (str chr)
    (ltrim (rtrim str chr) chr)))

  nil)  
