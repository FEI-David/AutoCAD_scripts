;;; CADALYST 08/08 www.cadalyst.com/code
;;; Tip 2305: LeaderToMleader.lsp Leader to Multileader (c) 2008 Lyle Hardin
;;; Pick an old style leader and text to create a new mleader entity and erase the old leader and text.
;;; March/2008
(prompt "\n LeaderToMleader.lsp loaded.... Enter L2ML to run ")
(defun c:L2ML ()
(setq leader (entsel "\nPick Leader") ; pick leader
leader2 (entget (car leader))
pt1 (dxf 10 leader2) ; get first point of leader
layer (dxf 8 leader2) ; get layer of leader

mtext (entsel "\nPick Text") ; pick text
mtext2 (entget (car mtext))
pt2 (dxf 10 mtext2) ; get point of text
text (dxf 1 mtext2) ; get
) ; setq

(command "-layer" "s" layer "") ; set layer of leader picked to current
(command "mleader" pt1 pt2 text) ; start mleader command
(COMMAND "ERASE" mtext "") ; erase text picked
(command "erase" leader "") ; erase leader picked

) ; defun

(defun dxf(code elist) ; define dxf function
(cdr (assoc code elist)) ;Finds the association pair, strips 1st element
);defun