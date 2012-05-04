;KM base utils, incl gen alst2ins, &arys&larger-persist? built into base
;Michael bobak@computer.org copywrite is (left leaning &) not transferable
; especially if any of the code is a collected from other opensrc
(defvar *ins-pre* "*") ;instance prefix
;accessor might merge w/some garnet work someday  ;use triples
(defun evaluable-str (str) (rm_comma str))
;(defgeneric asrt (i s))
(defgeneric eval-km (s))
(defmethod eval-km ((str String))
   (eval-str (str-cat "(km '#$" (evaluable-str str) ")")))

(defgeneric gvl (i s))
(defmethod gvl (i s)
  (km-slotvals  i s)) ;get-vals i #$s
(defgeneric svl (i s v))
(defmethod svl (i s v)
  (put-vals i s v))
(defun svl-al (i alst)   ;SetValue/s from alist
  (mapcar #'(lambda (pr) (sv i (car pr) (cdr pr))) alst)) 

(defgeneric pin (n &optional p)) ;pre ins name
(defmethod pin  ((s String) &optional (pre *ins-pre*))
  (prefix pre s))
(defmethod pin ((s Symbol) &optional (pre *ins-pre*))
  (intern (pin (symbol-name s) pre)))
(defmethod pin ((l List) &optional (pre *ins-pre*))
  (mapcar #'(lambda (i) (pin i :pre pre))  l))
;defmethod pin ((s Number) &optional (pre *ins-pre*))
(defmethod pin (s  &optional (pre *ins-pre*))
  (prefix (to-str pre) s))

(defgeneric show (s))
(defmethod show (s)
   (showme (pin s)))
(defmethod show ((l List))
  (mapcar #'show l))

(defgeneric typ (i))
;classp is-an-instance
(defmethod typ (i)  
  (list+ (gvl i "instance-of")))
;(defun typ+ (i) (list+ (typ i)))
(defmethod typ ((l List))
  (flat1 (mapcar #'typ l)))
(defun typ-p (i c) (member c (typ i))) 
(defun is-a-p (ins cls)
  (km (list (kin ins) '|&+| (list '|a| cls))))
