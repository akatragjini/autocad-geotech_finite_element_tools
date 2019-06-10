;Download Notepad++ to view
;This AutoLISP creates a grid of points at specified intervals based on 3d polylines
;Ensure number of vertices in 3D Polylines <32,767
;Ensure vertices in 3D Polyline are increasing in x coordinate

;-----------------------------------------------------------------------
; FUNCTION: 	PLOT POINT GRID ON SET INTERVAL SPACING (x y)
; AUTHOR: 		ALDO KATRAGJINI
; DATE:			SEPTEMBER 4 2017
;-----------------------------------------------------------------------

(defun C:PGRIDCREATE	 ( /	

								;error handling
								*error*
								sys_var_1
								sys_var_2
								sys_var_3

								;3d polyline selection set
								3dpl 											;3D Polyline selection set (singular)
								3dpl_l
								
								;3d polyline selection set (singular)
								3dpl_n 											;3d polyline name
								3dpl_n_0										;3d polyline data
								3dpl_n_0_lst									;List of polyline vertices
								3dpl_n_0_lst_dst
								
								pt_spac0
								pt_spac											;Point spacing
								pt_spac_rel
								
								lvar1
								lvar2
								
								k
								i
								
								3dpl_dst										;distance between 3d polyline nodes
								3dpl_dst_pr
								
								i_delta
								j_delta
								k_delta							
								i_delta_div
								j_delta_div
								k_delta_div
								
								s1
								
								i_ptspac
								j_ptspac
								k_ptspac	

								ijk_pt

						)
			   
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

(princ "\n-------Welcome to PGRIDCREATE--------")
(princ "\nSelect 3D Polylines & specify spacing")
(princ "\n-------------------------------------")

(princ "\nSelect 3D Polylines: ")										
	(command "Select" pause)											
	(setq 3dpl (ssget "P" '((0 . "POLYLINE"))))		
	(setq 3dpl_l (sslength 3dpl))

(initget (+ 1 2 4))
	(princ "\nEnter spacing: ")
	(setq pt_spac0 (getint))												;point spacing

(setq k 0)
(while k (< k 3dpl_l)

	(if (> k 32767)															;>32767 as float (3.0), <32767 as integer (3)
		(float k)
		(fix k)
	)

	(setq 3dpl_n (ssname 3dpl k))											;Get 3D Polyline entity name
		(setq 3dpl_n_0 (entget 3dpl_n))										;Get 3D Polyline group codes and associated data


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;BUILD LIST OF 3D POLYLINE VERTICES
	(setq i 1)
	(setq 3dpl_n_0_lst (list))

	(while i														;Prepare to loop through 3D Polyline vertex entities
	
		(setq 3dpl_n_0 (entget (entnext (cdr (car 3dpl_n_0)))))		;Get entity name of 3D Polyline (CAR and CDR of [-1 . <Entity name: 7ffffb12410>])		
																;Entity name ENTNEXT returns entity name of each # of vertices of 3D Polyline
																;ENTGET to return the group codes & associated data of each vertices
	
		(if (/= (cdr (assoc 0 3dpl_n_0)) "SEQEND")					;Check whether the Entity name = SEQEND meaning end of vertex list has been reached
			
				(progn												;IF NOT TRUE
				
					(setq 3dpl_n_0_lst (append 3dpl_n_0_lst (list (cdr (assoc 10 3dpl_n_0)))))
								
				)
			
				(setq i nil)										;IF TRUE
		
		)

	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;BUILD LIST OF DISTANCE BETWEEN 3D POLYLINE VERTICES
	(setq i 0)
	(setq 3dpl_n_0_lst_dst (list))

	(while 	(< i (- (length 3dpl_n_0_lst) 1))

		;Create direction vector from i to i+1
		(setq i_delta (- (car(nth (+ i 1) 3dpl_n_0_lst)) (car(nth i 3dpl_n_0_lst))))
		(setq j_delta (- (cadr(nth (+ i 1) 3dpl_n_0_lst)) (cadr(nth i 3dpl_n_0_lst))))
		(setq k_delta (- (caddr(nth (+ i 1) 3dpl_n_0_lst)) (caddr(nth i 3dpl_n_0_lst))))

		;2d distance between points
		(setq 3dpl_dst (sqrt(+ (expt i_delta 2) (expt j_delta 2))))		

		(setq 3dpl_n_0_lst_dst (append 3dpl_n_0_lst_dst (list 3dpl_dst)))
	
		(setq i (+ i 1))
	
		(setq i_delta 0)
		(setq j_delta 0)
		(setq k_delta 0)
		(setq 3dpl_dst 0)

	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;GO THROUGH LIST OF 3D POLYLINE VERTICES & CHECK XY DIST AND COMPARE TO ONE ENTERED BY USER
	(setq i 0)
	(setq j 0)
	(setq 3dpl_dst 0)
	(setq pt_spac pt_spac0)

	(while (< i (- (length 3dpl_n_0_lst) 1))					
	
		;Increment distance between nodes based on i and also increment count to increase pt_spacing
		(setq 3dpl_dst 	(+ 3dpl_dst (nth i 3dpl_n_0_lst_dst)))
	
		(cond																									
	
			;;;IF DIST BETWEEN NODES > USER SPECIFIED DIST, PERFORM OPERATION;;;
			((>= 3dpl_dst pt_spac)								
									
					;Create direction vector from i to i+1
					(setq i_delta (- (car(nth (+ i 1) 3dpl_n_0_lst)) (car(nth i 3dpl_n_0_lst))))
					(setq j_delta (- (cadr(nth (+ i 1) 3dpl_n_0_lst)) (cadr(nth i 3dpl_n_0_lst))))
					(setq k_delta (- (caddr(nth (+ i 1) 3dpl_n_0_lst)) (caddr(nth i 3dpl_n_0_lst))))			
		
					;Scale direction vectors down
					(setq i_delta_div (/ i_delta 1000))			
					(setq j_delta_div (/ j_delta 1000))
					(setq k_delta_div (/ k_delta 1000))
						
					;Find which combination of i and j vectors equal to user point distance specified
					;Reset point spacing relative to just node before it
				
					(cond
						((= i 0)
						(setq 3dpl_dst_pr 0)
						)
						(t nil)
					)				
										
					(setq pt_spac_rel (- pt_spac 3dpl_dst_pr))
		
					(setq s1 	(sqrt 	(/ (expt pt_spac_rel 2) (+ (expt i_delta_div 2) (expt j_delta_div 2)))				
								)				
					)
					(setq pt_spac_rel 0)
				
					;Get resultant  i j k (from i point of reference)
					(setq i_ptspac (* s1 i_delta_div))
					(setq j_ptspac (* s1 j_delta_div))
					(setq k_ptspac (* s1 k_delta_div))
				
					(setq i_ptspac (+ i_ptspac (car(nth i 3dpl_n_0_lst))))
					(setq j_ptspac (+ j_ptspac (cadr(nth i 3dpl_n_0_lst))))
					(setq k_ptspac (+ k_ptspac (caddr(nth i 3dpl_n_0_lst))))				
		
					(setq ijk_pt (list i_ptspac j_ptspac k_ptspac))
					;(prin1 ijk_pt)
					(princ "\n")
		
					(command "POINT" ijk_pt)	

					(setq pt_spac (+ pt_spac pt_spac0)) 
									
					;Reset values				
					(setq i_delta 0)
					(setq j_delta 0)
					(setq k_delta 0)
					(setq i_delta_div 0)
					(setq j_delta_div 0)
					(setq k_delta_div 0)				
					(setq i_ptspac 0)
					(setq i_ptspac 0)
					(setq i_ptspac 0)
					(setq s1 0)								
					
					;Check whether point spacing jumps to next node
					(cond	
						((< 3dpl_dst pt_spac)	
						
						(setq 3dpl_dst_pr 3dpl_dst)
						(setq i (+ i 1))
						
						)	
						(t
						
						(setq 3dpl_dst 	(- 3dpl_dst (nth i 3dpl_n_0_lst_dst)))	
						
						)
					)						
			)
			
			;;;IF DIST BETWEEN NODES < USER SPECIFIED DIST, PERFORM OPERATION;;;
			(t 		
					(setq i (+ i 1))
					(setq 3dpl_dst_pr 3dpl_dst)				
			)																				

		)	
	
	)











	(if (> k 32767)
		(setq k (+ k 1.0))
		(setq k (+ k 1))
	)
)

(setvar "CMDECHO" sys_var_1)
(setvar "NOMUTT" sys_var_2)
(setvar "OSMODE" sys_var_3)
(command "_.UCS" "previous")

)

;End of program
