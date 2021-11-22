 
;;; 
;;; http://forums.autodesk.com/t5/autocad-2013-2014-2015/mtext-to-multileaders/td-p/5336487
;;; 
;;; CADALYST 08/08 www.cadalyst.com/code
;;; Tip 2305: LeaderToMleader.lsp Leader to Multileader (c) 2008 Lyle Hardin
;;; Pick an old style Leader and Text to create a NEW Mleader entity and erase the old leader and text.
;;; March 2008 
;;; 
 
(prompt "\n LeaderToMleader.lsp loaded.... Enter L2ML to run ") 

(defun c:L2ML ()


(setq leader (entsel "\nPick Leader ")  ; pick leader
leader2 (entget (car leader))
pt1 (dxf 10 leader2)            ; get first point of leader
layer (dxf 8 leader2)           ; get layer of leader

mtext (entsel "\nPick Text ")   ; pick text
mtext2 (entget (car mtext))
pt2 (dxf 10 mtext2)             ; get point of text
text (dxf 1 mtext2)             ; get
)                               ; setq

(command "_-layer" "_s" layer "") ; set layer of leader picked to current
(command "_mleader" pt1 pt2 text)  ; start mleader command 

(COMMAND "_ERASE" mtext "")   ; erase text picked
(command "_erase" leader "")  ; erase leader picked


) ; defun


(defun dxf(code elist)    ; define dxf function
(cdr (assoc code elist))  ; Finds the association pair, strips 1st element
)                         ; defun

