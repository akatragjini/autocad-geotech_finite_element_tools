;Download Notepad++ to view
;Ensure AutoCAD Express tools is installed
;This AutoLISP grabs a gridded set of points (z-coordinates representing topo)offset by a predetermined z and draws boxes (3d faces)

;Grid MUST be square.

;-----------------------------------------------------------------------
; FUNCTION: 	MESH TOPO USING GRID OF POINTS
; AUTHOR: 		ALDO KATRAGJINI
; DATE:			MARCH 09 2017
;-----------------------------------------------------------------------

(defun C:PGRIDMESH ( / slcn_set 													;Selection set of points
					  slcn_set_lgth												;Selection set length
					 
					  i
					  pt_name													;Name of point i in the selection set
					  pt_list													;Association list of point i
					  pt														;Point i xyz coordinates
					  pt_matrix													;List containing xyz coordinates of all points in selection set
					 
					  pt_rows													;Number of rows of points in grid
					  pt_cols													;Number of columns of points in grid
					  z_offset													;Distance to offset point matrix by (prespecified by user)
					 
					  pt_matrix_y_sort											;xyz coordinate list sorted by y
					 
					  j
					  k
					  pt_hold													;point in row j column k of point matrix
					  pt_matrix_hold											;points in row j all columns					 
					  pt_matrix_final											;points in row j all columns sorted by x coordinate					 
					  pt_matrix_out												;points sorted by y and x coordinate
					 
					  pt_hold_row_1												;Row j of original point matrix
					  pt_hold_1													;Row j column k of original point matrix (xyz coordinates for 1 point)
					  x_hold													;Hold variable
					  y_hold													;Hold variable
					  z_hold													;Hold variable (used to offset z)

					  pt_matrix_hold_1											;Offset point matrix row j
					  pt_matrix_out_1											;Offset point matrix complete

				 )

				 
(setvar "CMDECHO" 0)															;Supress command window 
(command "UCS" "w")																;UCS World
(setvar "OSMODE" (boole 7 (getvar "OSMODE") 16384))								;On-snap button off. Causes issues when drawing lines	

(princ "\nGrid of points MUST be square\n")

							
(princ "\nSelect Points:\n")													;Select points to draw 3D Faces from
	(command "Select" pause)											
	(setq slcn_set (ssget "P" '((0 . "POINT"))))								;Get previous selection set filtering out entities /= point
	(setq slcn_set_lgth (sslength slcn_set))						    		;Length/entities in selection set (ie no. of points)

	
(initget (+ 1 2 4))														
	(setq pt_rows (getint "\nEnter Number of Rows in Point Matrix: "))			;Get number of rows in point matrix	
	
(setq pt_cols (/ slcn_set_lgth pt_rows))

(if (= slcn_set_lgth (* pt_rows pt_cols))										;Check if point matrix is square
	nil																			
	(progn
		(princ "\nBonehead\n")
		(quit)
	)
)	

(initget (+ 1 2))														
	(setq z_offset (getreal "\nEnter Mesh Height(+/-): "))						;Get z offset		
		

(setq i 0)																		;Count
(while (< i slcn_set_lgth)										   				;Loop through points in selection set and assign all xyz coordinates to list

	(if (> i 32767)																;Numbers >32767 treated as float & <32767 treated as integers. 
		(float i)																
		(fix i) 																
	)	
		
	
	(setq pt_name (ssname slcn_set i))											;Point name
	(setq pt_list (entget pt_name))												;Point association list
	(setq pt (cdr(assoc 10 pt_list)))											;Point xyz coordinates
	
	(setq pt_matrix (cons pt pt_matrix))										;Build list of xyz points

	(if (> i 32767)																;Increment count
		(setq i (+ i 1.0))													
		(setq i (+ i 1)) 
		
	)
	
)


(setq pt_matrix_y_sort 															;Sort xyz points list by y coordinates (disclaimer: I have no idea how this function works just that it does)
	(vl-sort pt_matrix (function (lambda (y1 y2) (< (cadr y1) (cadr y2)))
					   )
	)				   
)


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

(setq pt_hold_row_1 nil) 
(setq pt_hold_1 nil)
(setq x_hold nil)
(setq y_hold nil)
(setq z_hold nil)
(setq pt_matrix_hold_1 nil)
(setq pt_matrix_out_1 nil)
(setq j 1)
(setq k 1)
(while (<= j pt_rows)															;Create matrix of points offset by predetermined distance								

	(if (> j 32767)															
		(float j)																
		(fix j) 																
	)		

	(setq pt_hold_row_1 (nth (- j 1) pt_matrix_out))							;Get row j
		
	(while (<= k pt_cols)														
	
		(if (> k 32767)														 
			(float k)																
			(fix k) 																
		)		
		
		(setq pt_hold_1 (nth (- k 1) pt_hold_row_1))							;Get row j column k		
		(setq x_hold (car pt_hold_1))											;x coordinate
		(setq y_hold (cadr pt_hold_1))											;y coordinate		
		(setq z_hold (+ (caddr pt_hold_1) z_offset))							;z coordinate offset by specified amount	
		(setq pt_hold_1 (list x_hold y_hold z_hold))							;Regroup coordinates into list		
		(setq pt_matrix_hold_1 (cons pt_hold_1 pt_matrix_hold_1))				;Group all points in row j into list
		
		
		(if (> k 32767)														
			(setq k (+ k 1.0))													
			(setq k (+ k 1)) 
			
		)
	)	
	(setq pt_matrix_hold_1 (reverse pt_matrix_hold_1))
	(setq pt_matrix_out_1 (cons pt_matrix_hold_1 pt_matrix_out_1))				;Group rows together
	
	(if (> j 32767)														
		(setq j (+ j 1.0))													
		(setq j (+ j 1))
	)
	
	
	(setq pt_hold_row_1 nil)
	(setq k 1)
	(setq pt_matrix_hold_1 nil)
)
(setq pt_matrix_out_1 (reverse pt_matrix_out_1))

;FOR DEBUGGING
;(prin1 pt_matrix_out)
;(princ "\n")
;(prin1 pt_matrix_out_1)
;(princ "\n")

(setq j 1)
(setq k 1)

(command "LAYER" "M" "GRIDMESH" "")

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
						  
						  (nth (- k 1) (nth (- j 1) pt_matrix_out_1))			;Offset
						  (nth k (nth (- j 1) pt_matrix_out_1))
						  (nth k (nth j pt_matrix_out_1)) 
						  (nth (- k 1) (nth j pt_matrix_out_1))
						  ;(nth (- k 1) (nth (- j 1) pt_matrix_out_1))						  
						  )
						  				
		(command "PEDIT" (entlast) "NCLOSE" "X") 								;Close 3D MESH
																				;Note, for whatever reason a 2x5 mesh does not work, you MUST use pedit or Map3D encounters errors		
		;Compose box from 3D Faces
		;(command "3DFACE" (nth (- k 1) (nth (- j 1) pt_matrix_out))			;3D Face for original point matrix
		;				  (nth k (nth (- j 1) pt_matrix_out)) 
		;				  (nth k (nth j pt_matrix_out))
		;				  (nth (- k 1) (nth j pt_matrix_out)) 
		;				  "")
						  
		;(command "3DFACE" (nth (- k 1) (nth (- j 1) pt_matrix_out_1))			;3D Face for offset point matrix
		;				  (nth k (nth (- j 1) pt_matrix_out_1))
		;				  (nth k (nth j pt_matrix_out_1)) 
		;				  (nth (- k 1) (nth j pt_matrix_out_1)) 
		;				  "")			
						  
		;(command "3DFACE" (nth (- k 1) (nth (- j 1) pt_matrix_out))			;Side (North)
		;				  (nth (- k 1) (nth (- j 1) pt_matrix_out_1)) 
		;				  (nth k (nth (- j 1) pt_matrix_out_1))
		;				  (nth k (nth (- j 1) pt_matrix_out))
		;				  "")
						  
		;(command "3DFACE" (nth (- k 1) (nth j pt_matrix_out)) 					;Side (South)
		;				  (nth (- k 1) (nth j pt_matrix_out_1)) 
		;				  (nth k (nth j pt_matrix_out_1))
		;				  (nth k (nth j pt_matrix_out))
		;				  "")
						  
		;(command "3DFACE" (nth (- k 1) (nth (- j 1) pt_matrix_out))			;Side (West)
		;				  (nth (- k 1) (nth (- j 1) pt_matrix_out_1)) 
		;				  (nth (- k 1) (nth j pt_matrix_out_1)) 
		;				  (nth (- k 1) (nth j pt_matrix_out))		
		;				  "")
						  
		;(command "3DFACE" (nth k (nth (- j 1) pt_matrix_out)) 					;Side (East)
		;				  (nth k (nth (- j 1) pt_matrix_out_1)) 
		;				  (nth k (nth j pt_matrix_out_1)) 
		;				  (nth k (nth j pt_matrix_out)) 			
		;				  "")
		
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

(command "UCS" "previous")													;UCS previous					
(setvar "OSMODE" (boole 2 (getvar "OSMODE") 16384))							;On-snap button on.
		 
)