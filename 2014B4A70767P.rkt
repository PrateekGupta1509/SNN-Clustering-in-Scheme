#lang racket
(provide (all-defined-out))

(define inputFile (vector-ref (current-command-line-arguments) 0))

(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

(define (numEle stringList)
	(if (null? stringList)
		'()
		(cons (string->number (car stringList)) (numEle (cdr stringList)))
	)
)
(define (numList str) (numEle (string-split str) ) )

(define getInp (file->lines inputFile))
(define firstLine (numList (car getInp)))

(define (get_nth lst  n) (if (= n 1) (car lst) (get_nth (cdr lst) (- n 1))))

(define N (get_nth firstLine 1) )
(define D (get_nth firstLine 2) )
(define K (get_nth firstLine 3) )
(define E (get_nth firstLine 4) )
(define P (get_nth firstLine 5) )

(define (makeMatrix lists)
	(if(null? lists)
		'()
		( cons (numList (car lists)) ( makeMatrix (cdr lists)) )
	)
)

(define (printMatrix matrix n)
	(if(null? matrix)
		'()
		(cons (list n (car matrix)) (printMatrix (cdr matrix) (+ n 1)))
	)
)

(define step1 (printMatrix (makeMatrix (cdr getInp)) 1 ))



(define (pointWiseDist list1 list2)
	(if (null? list1)
		0
		(+ (expt (- (car list1) (car list2)) 2) (pointWiseDist (cdr list1) (cdr list2)) )
 	)
)

(define (euclideanDist list1 list2)
	(sqrt (pointWiseDist list1 list2))
)

(define (getSimRow p1 l1)
	(if (null? l1)
	'()
	(if (= (car p1) (car (car l1)))
		(cons (list (car (car l1)) +inf.0) (getSimRow p1 (cdr l1)) )
		(cons (list (car (car l1)) (euclideanDist (cadr p1) (cadr (car l1)))) (getSimRow p1 (cdr l1)) )
	)
	)
)

(define (similarityMatrix m1)
	(if (null? m1)
		'()
		(cons (getSimRow (car m1) step1) (similarityMatrix (cdr m1)) )
	)
)

(define tempstep2 (similarityMatrix step1))

(define step2 (modify_precision tempstep2))

(define (sortFuncAsc row) (sort row < #:key(lambda(x) (cadr x))))
(define (sortFuncDesc row) (sort row > #:key(lambda(x) (cadr x))))

(define (getNNRow row n)
	(if(= K n)
		'()
		(cons (car (car row)) (getNNRow (cdr row) (+ n 1) ))
	)	
)

(define (nearestNN m1) 
	(if (null? m1)
		'()
		(cons (sort (getNNRow (sortFuncAsc (car m1)) 0) <) (nearestNN (cdr m1)) )
	)
)

(define step3 (nearestNN step2))


(define (getWeight l1 l2)
	(length(set-intersect l1 l2))
)

(define (getRowWeightList list1 list2 n)
	(if(null? list2)
		'()
		(if (member n (get_nth step3 (car list2)))
			(cons (list (car list2) (getWeight list1 (get_nth step3 (car list2)))) (getRowWeightList list1 (cdr list2) n) )
			(getRowWeightList list1 (cdr list2) n)
		)
	)
)

(define (getWeightMatrix lst n)
	(if (null? lst)
		'()
		(cons (sortFuncDesc (getRowWeightList (car lst) (car lst) n)) (getWeightMatrix (cdr lst) (+ n 1)) )
	)
)

(define step4 (getWeightMatrix step3 1))

(define (rowPD lst)
	(if(null? lst)
		0
		(if(< (cadr (car lst)) E)
			(+ 0 (rowPD (cdr lst)))
			(+ 1 (rowPD (cdr lst)))
		)
	)
) 

(define (pointDensity lst)
	(if (null? lst)
		'()
		(cons (rowPD (car lst)) (pointDensity (cdr lst)))
	)
)

(define step5 (pointDensity step4))

(define (getIndices lst n)
	(if(null? lst)
		'()
		(if(< (car lst) P)
			(getIndices (cdr lst) (+ n 1))
			(cons (+ n 1) (getIndices (cdr lst) (+ n 1)))
		)
	)
)

(define step6 (getIndices step5 0))



(define (getValidPoints lst)
	(if (null? lst)
		'()
		(if(>= (cadr (car lst)) E)
			(cons (car (car lst)) (getValidPoints (cdr lst)))
			(getValidPoints (cdr lst))
		)
	)
)

(define (ptCluster pt corePoints)
	(set-intersect (getValidPoints (get_nth step4 pt)) corePoints)
)

(define (getCluster core temp)
	(if(null? core)
		'()
		(cons (car core) (getCluster (append (cdr core) (ptCluster (car core) (set-subtract temp core))) (set-subtract temp core)))
	)
)


(define (getAllClusters core n)
	(if(null? core)
		'()
		(cons (list (+ n 1) (sort (getCluster (list (car core)) core) <)) (getAllClusters (sort (set-subtract core (getCluster (list (car core)) core)) <) (+ n 1) ))
	)
)

(define step7 (getAllClusters step6 0))


(define (noisePoint lst n)
	(if(null? lst)
		'()
		(if(= (car lst) 0)
			(cons (+ n 1) (noisePoint (cdr lst) (+ n 1)))
			(noisePoint (cdr lst) (+ n 1))
		)
	)
)

(define step8 (noisePoint step5 0))



(define (borderPoint lst n)
	(if (null? lst)
		'()
		(if(or (= (car lst) 0) (>= (car lst) P))
			(borderPoint (cdr lst) (+ n 1))
			(cons (+ n 1) (borderPoint (cdr lst) (+ n 1)))
		)
	)
)

(define step9 (borderPoint step5 0))


(define (addEleToClusEle ele clusele cluster)
	(if(null? cluster)
		'()
		(if(member clusele (cadr (car cluster)))
			(cons (list (car (car cluster)) (sort (cons ele (cadr (car cluster))) <)) (addEleToClusEle ele clusele (cdr cluster)) )
			(cons (car cluster) (addEleToClusEle ele clusele (cdr cluster)))
		)
	)
)

(define (maxFirst a b)
	(if (>= (car a) (car b))
		a
		b
	)
)

(define (findMax ele core)
	(if(null? core)
		'(-1 -1)
		(maxFirst (list (getWeight (get_nth step3 ele) (get_nth step3 (car core))) (car core)) (findMax ele (cdr core)) )
	)
)


(define (extendCluster bpList cluster)
	(if (null? bpList)
		cluster
		(extendCluster (cdr bpList) (addEleToClusEle (car bpList) (cadr (findMax (car bpList) step6)) cluster))
	)
)

(define step10 (extendCluster step9 step7))


; (display step1)
; (display step2)
; (display step3)
; (display step4)
; (display step5)
; (display step6)
; (display step7)
; (display step8)
; (display step9)
; (display step10)


