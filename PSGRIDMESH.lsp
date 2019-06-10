;Download Notepad++ to view
;Ensure AutoCAD Express tools is installed
;This AutoLISP grabs two levels of points (grid) used to mesh a stope and creates boxes (3d faces) between the levels. Similar to Examine3D 
;Grid MUST be square.

;-----------------------------------------------------------------------
; FUNCTION: 	MESH STOPE LEVELS USING 2 LEVELS OF POINTS (MUST BE SQUARe)
; AUTHOR: 		ALDO KATRAGJINI
; DATE:			OCTOBER 06 2017
; REV DATE:		JULY 27 2018
;-----------------------------------------------------------------------
;Revision to only select nodes once and have them filter by color into the upper / lower levels of the mesh object. Also incorporated proper error handling routine


(defun C:PSGRIDMESH ( / 

					  ;error handling
					  *error*
					  sys_var_1
					  sys_var_2
					  sys_var_3

					  ;selection set of points
					  slcn_set
					  slcn_set_lgth

					  ;matrix to run through each point in selection set and assing it to upper or lower
					  i
					  pt_name
					  pt_list
	
					  pt
					  pt_matrix
					 					  
					  pt_l
					  pt_matrix_l
			  
					  ;determine whether matrix is square
					  pt_rows													;Number of rows of points in grid
					  pt_cols													;Number of columns of points in grid
					 




					 
					  pt_matrix_y_sort											;xyz coordinate list sorted by y (upper)
					  pt_matrix_y_sort_l										;xyz coordinate list sorted by y (lower)
					 
					  j
					  k
					  
					  pt_hold													;point in row j column k of point matrix (upper)
					  pt_matrix_hold											;points in row j all columns (upper)					 
					  pt_matrix_final											;points in row j all columns sorted by x coordinate (upper)					 
					  pt_matrix_out												;points sorted by y and x coordinate (upper)
					 
					  pt_hold_l													;point in row j column k of point matrix (lower)
					  pt_matrix_hold_l											;points in row j all columns (lower)					 
					  pt_matrix_final_l											;points in row j all columns sorted by x coordinate (lower)					 
					  pt_matrix_out_l											;points sorted by y and x coordinate (lower)
					 
				 )

				 
				 
;;ERROR HANDLING;;
(setq sys_var_1 (getvar "CMDECHO"))
(setvar "CMDECHO" 0)					;supress prompt and input echoing during lisp execution

(setq sys_var_2 (getvar "NOMUTT"))
(setvar "NOMUTT" 1)						;supress muttering behaviour (note some commands cannot be supressed)

(setq sys_var_3 (getvar "OSMODE"))
(setvar "OSMODE" 0)						;turn on-snap off (causes issues when drawing lines)

(command "_.UCS" "w")

;;Error handling function. If error is evaluated this program gets called and passed the string err_msg;;
(defun *error* (err_msg)

	(if sys_var_1							;if sys_var_1 is non-nil (i.e. has some value) then
		(setvar "CMDECHO" sys_var_1)		;true then restore user environment
		(princ)								;false then do nothing
	)
	(if sys_var_2
		(setvar "NOMUTT" sys_var_2)
		(princ)
	)
	(if sys_var_3
		(setvar "OSMODE" sys_var_3)
		(princ)
	)
	(if (not (member err_msg '("Function cancelled" "quit / exit abort"))) 	;if err_msg is not any of these standard messages then
		(princ (strcat "\nError: " err_msg "\n"))							;true then print error message
		(princ)																;false then do nothing
	)
	
	(setq	err_msg nil
			sys_var_1 nil
			sys_var_2 nil
			sys_var_3 nil
	) 
)				 
;;END ERORR HANDLING;;

(princ "\n------Welcome to PSGRIDMESH---------")
(princ "\nSelect point cloud (upper and lower)")
(princ "\nUpper/Lower (Green), Lower (Magenta)")
(princ "\n------------------------------------\n")

(princ "\nSelect Points(upper AND lower):\n")																					;Select points to draw 3D Faces from
	(command "Select" pause)											
	(setq slcn_set (ssget "P" '((0 . "POINT") (-4 . "<OR") (62 . 3) (62 . 6) (-4 . "OR>"))))								;Get previous selection set filtering out entities /= point
	(setq slcn_set_lgth (sslength slcn_set))						    													;Length/entities in selection set (ie no. of points)

;sort each point into upper or lower matrices
(setq i 0)
(while (< i slcn_set_lgth)	
	
	(if (> i 32767)																;Numbers >32767 treated as float & <32767 treated as integers. 
		(float i)																
		(fix i) 																
	)	

	(setq pt_name (ssname slcn_set i))											;Point name
		(setq pt_list (entget pt_name))											;Point association list
	

	
	
	(cond
		(	(= (cdr(assoc 62 pt_list)) 3 )										;if green			
				(progn
					(setq pt (cdr(assoc 10 pt_list)))							;Point xyz coordinates
					(setq pt_matrix (cons pt pt_matrix))						;Build list of xyz points
				)
		)	
	
		(	(= (cdr(assoc 62 pt_list)) 6 )										;if magenta			
				(progn
					(setq pt_l (cdr(assoc 10 pt_list)))							;Point xyz coordinates
					(setq pt_matrix_l (cons pt_l pt_matrix_l))					;Build list of xyz points		
				)
		)	
		
		(t (princ))
		
	)

	(if (> i 32767)																;Increment count
		(setq i (+ i 1.0))													
		(setq i (+ i 1)) 
		
	)
)

(setq 	pt_name nil pt_list nil
		pt nil pt_l nil
)
		
	
;determine whether matrix is square
(setq pt_rows 2)
(setq pt_cols (/ (/ slcn_set_lgth 2) pt_rows))

(if (= (/ slcn_set_lgth 2) (* pt_rows pt_cols))										;Check if point matrix is square
	nil																			
	(progn
		(princ "\nBonehead\n")
		(quit)
	)
)	


;;sort matrices
;;upper
(setq pt_matrix_y_sort 															;Sort xyz points list by y coordinates (disclaimer: I have no idea how this function works just that it does)
	(vl-sort pt_matrix (function (lambda (y1 y2) (< (cadr y1) (cadr y2)))
					   )
	)				   
)

;;lower
(setq pt_matrix_y_sort_l 														;Sort xyz points list by y coordinates (disclaimer: I have no idea how this function works just that it does)
	(vl-sort pt_matrix_l (function (lambda (y1 y2) (< (cadr y1) (cadr y2)))
					   )
	)				   
)

;;upper
(setq pt_hold nil)										
(setq pt_matrix_hold nil)
(setq pt_matrix_final nil)
(setq pt_matrix_out nil)
(setq j 1)
(setq k 1)
(while (<= j pt_rows)															;Sort rows of point matrix by x coordinate. Loop through row j	

	(if (> j 32767)															
		(float j)																
		(fix j) 																
	)		

	(while (<= k pt_cols)														;Loop through row j column k	
	
		(if (> k 32767)														 
			(float k)																
			(fix k) 																
		)		
		
		(setq pt_hold (nth (- (+ k (* pt_cols (- j 1))) 1) pt_matrix_y_sort))	;Get row j column k point from sorted list	
		(setq pt_matrix_hold (cons pt_hold pt_matrix_hold))						;Group all points in row j into list
			
		(if (> k 32767)														
			(setq k (+ k 1.0))													
			(setq k (+ k 1)) 
			
		)
	)
				
	(setq pt_matrix_final 														;Sort all points in row j by x coordinate
		(vl-sort pt_matrix_hold (function (lambda (x1 x2) (< (car x1) (car x2)))
						)
		)				   
	)
		
	(setq pt_matrix_out (cons pt_matrix_final pt_matrix_out))					;Group all x coordinate sorted rows together
	
	(setq k 1)																	;Reset column count back to 1
	(setq pt_matrix_hold nil)													;Reset row list to nil
	(setq pt_matrix_final nil)													;Reset sorted row list to nil

	
	(if (> j 32767)														
		(setq j (+ j 1.0))													
		(setq j (+ j 1))
	)
)
;;


;;lower
(setq pt_hold_l nil)										
(setq pt_matrix_hold_l nil)
(setq pt_matrix_final_l nil)
(setq pt_matrix_out_l nil)
(setq j 1)
(setq k 1)
(while (<= j pt_rows)															;Sort rows of point matrix by x coordinate. Loop through row j	

	(if (> j 32767)															
		(float j)																
		(fix j) 																
	)		

	(while (<= k pt_cols)														;Loop through row j column k	
	
		(if (> k 32767)														 
			(float k)																
			(fix k) 																
		)		
		
		(setq pt_hold_l (nth (- (+ k (* pt_cols (- j 1))) 1) pt_matrix_y_sort_l))	;Get row j column k point from sorted list	
		(setq pt_matrix_hold_l (cons pt_hold_l pt_matrix_hold_l))						;Group all points in row j into list
			
		(if (> k 32767)														
			(setq k (+ k 1.0))													
			(setq k (+ k 1)) 
			
		)
	)
	
	(setq pt_matrix_final_l 														;Sort all points in row j by x coordinate
		(vl-sort pt_matrix_hold_l (function (lambda (x1 x2) (< (car x1) (car x2)))
						)
		)				   
	)
		
	(setq pt_matrix_out_l (cons pt_matrix_final_l pt_matrix_out_l))				;Group all x coordinate sorted rows together
	
	(setq k 1)																	;Reset column count back to 1
	(setq pt_matrix_hold_l nil)													;Reset row list to nil
	(setq pt_matrix_final_l nil)												;Reset sorted row list to nil

	
	(if (> j 32767)														
		(setq j (+ j 1.0))													
		(setq j (+ j 1))
	)
)


;FOR DEBUGGING
;(prin1 pt_matrix_out)
;(princ "\n")
;(prin1 pt_matrix_out_l)
;(princ "\n")

(setq j 1)
(setq k 1)

(command "LAYER" "M" "PSGRIDMESH" "")

(while (<= j (- pt_rows 1))														;Draw 3D Faces							

	(if (> j 32767)															
		(float j)																
		(fix j) 																
	)		
		
	(while (<= k (- pt_cols 1))														
	
		(if (> k 32767)														 
			(float k)																
			(fix k) 																
		)		
		
		;Compose box from 3DMESH
		(command "3DMESH" "2"
						  "4"
						  (nth (- k 1) (nth (- j 1) pt_matrix_out))				;Original Point Grid Matrix
						  (nth k (nth (- j 1) pt_matrix_out)) 
						  (nth k (nth j pt_matrix_out))
						  (nth (- k 1) (nth j pt_matrix_out)) 
						  ;(nth (- k 1) (nth (- j 1) pt_matrix_out))
						  
						  (nth (- k 1) (nth (- j 1) pt_matrix_out_l))			;Offset
						  (nth k (nth (- j 1) pt_matrix_out_l))
						  (nth k (nth j pt_matrix_out_l)) 
						  (nth (- k 1) (nth j pt_matrix_out_l))
						  ;(nth (- k 1) (nth (- j 1) pt_matrix_out_l))						  
						  )
						  				
		(command "PEDIT" (entlast) "NCLOSE" "X") 								;Close 3D MESH
																				;Note, for whatever reason a 2x5 mesh does not work, you MUST use pedit or Map3D encounters errors		
				
		(if (> k 32767)														
			(setq k (+ k 1.0))													
			(setq k (+ k 1)) 
			
		)
	)	

	(if (> j 32767)														
		(setq j (+ j 1.0))													
		(setq j (+ j 1))
	)
		
	(setq k 1)
)

(setvar "CMDECHO" sys_var_1)
(setvar "NOMUTT" sys_var_2)
(setvar "OSMODE" sys_var_3)
		 
)