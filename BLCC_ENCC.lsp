(defun c:blcc () (pl:block-color) (princ))
 
(defun c:encc () (pl:block-ent-color) (princ))
 
;;;get from Alaspher http://forum.dwg.ru/showthread.php?t=1036
 
;;; http://forum.dwg.ru/showpost.php?p=166220&postcount=18
 
(vl-load-com)
 
(defun pl:block-ent-color (/ adoc blocks color ent lays)
 
(setq adoc (vla-get-activedocument (vlax-get-acad-object))
 
lays (vla-get-layers adoc)
 
color (acad_colordlg 256)
 
)
 
(if color
 
(progn (setvar "errno" 0)
 
(vla-startundomark adoc)
 
(while (and (not (vl-catch-all-error-p
 
(setq ent (vl-catch-all-apply
 
(function nentsel)
 
'("\nSelect entity <Exit>:")
 
)
 
)
 
)
 
)
 
(/= 52 (getvar "errno"))
 
)
 
(if ent
 
(progn (setq ent (vlax-ename->vla-object (car ent))
 
lay (vla-item lays (vla-get-layer ent))
 
)
 
(if (= (vla-get-lock lay) :vlax-true)
 
(progn (setq layloc (cons lay layloc))
 
(vla-put-lock lay :vlax-false)
 
)
 
)
 
(vl-catch-all-apply (function vla-put-color) (list ent color))
 
(vla-regen adoc acallviewports)
 
)
 
(princ "\nNothing selection! Try again.")
 
)
 
)
 
(foreach i layloc (vla-put-lock i :vlax-true))
 
(vla-endundomark adoc)
 
)
 
)
 
(princ)
 
)
 
(defun pl:block-color (/ adoc blocks color ins lays)
 
(setq adoc (vla-get-activedocument (vlax-get-acad-object))
 
blocks (vla-get-blocks adoc)
 
lays (vla-get-layers adoc)
 
color (acad_colordlg 256)
 
)
 
(if color
 
(progn (setvar "errno" 0)
 
(vla-startundomark adoc)
 
(while (and (not (vl-catch-all-error-p
 
(setq ins (vl-catch-all-apply
 
(function entsel)
 
'("\nSelect block <Exit>:")
 
)
 
)
 
)
 
)
 
(/= 52 (getvar "errno"))
 
)
 
(if ins
 
(progn (setq ins (vlax-ename->vla-object (car ins)))
 
(if (= (vla-get-objectname ins) "AcDbBlockReference")
 
(if (vlax-property-available-p ins 'path)
 
(princ "\nThis is external reference! Try pick other.")
 
(progn (_pl:block-color blocks ins color lays)
 
(vla-regen adoc acallviewports)
 
)
 
)
 
(princ "\nThis isn't block! Try pick other.")
 
)
 
)
 
(princ "\nNothing selection! Try again.")
 
)
 
)
 
(vla-endundomark adoc)
 
)
 
)
 
(princ)
 
)
 
(defun _pl:block-color (blocks ins color lays / lay layfrz layloc)
 
(vlax-for e (vla-item blocks (vla-get-name ins))
 
(setq lay (vla-item lays (vla-get-layer e)))
 
(if (= (vla-get-freeze lay) :vlax-true)
 
(progn (setq layfrz (cons lay layfrz)) (vla-put-freeze lay :vlax-false))
 
)
 
(if (= (vla-get-lock lay) :vlax-true)
 
(progn (setq layloc (cons lay layloc)) (vla-put-lock lay :vlax-false))
 
)
 
(vl-catch-all-apply (function vla-put-color) (list e color))
 
(if (and (= (vla-get-objectname e) "AcDbBlockReference")
 
(not (vlax-property-available-p e 'path))
 
)
 
(_pl:block-color blocks e color lays)
 
)
 
(foreach i layfrz (vla-put-freeze i :vlax-true))
 
(foreach i layloc (vla-put-lock i :vlax-true))
 
)
 
)
 
(progn
 
(princ "\BLCC - Changes color of the chosen blocks")
 
(princ "\nENCC - Changes color of the chosen objects (may be element of the block)")
 
(princ))