(defun c:test (/ layer width entl dist ent pnt data)

 (setq layer "LAYER"
       width 2.
       entl  (entlast)
 )

 (initget 6)
 (if (and (setq dist (getdist "\nSpecify offset distance: "))
          (setq ent (entsel "\nSelect LWPolyline to offset: "))
          (or (eq (cdr (assoc 0 (entget (car ent)))) "LWPOLYLINE")
              (progn (princ "\nInvalid object!") nil)
          )
          (setq pnt (getpoint "\nSpecify point on side to offset: "))
     )
   (progn
     (command "_.offset" dist ent "_non" pnt "_EXIT")
     (if (not (equal entl (setq entl (entlast))))
       (entmod (subst (cons 8 layer)
                      (assoc 8 (setq data (entget entl)))
                      (subst (cons 43 width) (assoc 43 data) data)
               )
       )
     )
   )
 )
 (princ)
)