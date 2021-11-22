(defun c:layj ( / col ent lay typ )
    (while
        (progn (setvar 'errno 0)
            (not
                (or (setq ent (car (nentsel "\nSelect object on layer to change: ")))
                    (= 52 (getvar 'errno))
                )
            )
        )
        (princ "\nMissed, try again.")
    )
    (if
        (and ent
            (setq col
                (acad_colordlg
                    (cdr
                        (assoc 62
                            (tblsearch "layer"
                                (setq lay (cdr (assoc 8 (entget ent))))
                            )
                        )
                    )
                    nil
                )
            )
            (setq typ
                (LM:listbox "Select Linetype"
                    (
                        (lambda ( / def lst )
                            (while (setq def (tblnext "ltype" (null def)))
                                (if (wcmatch (cdr (assoc 2 def)) "~*|*")
                                    (setq lst (cons (cdr (assoc 2 def)) lst))
                                )
                            )
                            (acad_strlsort lst)
                        )
                    )
                    nil
                )
            )
        )
        (command "_.-layer" "_C" col lay "_L" (car typ) lay "")
    )
    (princ)
)

;; List Box  -  Lee Mac
;; Displays a List Box allowing the user to make a selection from the supplied data.
;; msg - [str] Dialog label
;; lst - [lst] List of strings to display
;; mtp - [bol] t=allow multiple
;; Returns: [lst] List of selected items, else nil

(defun LM:listbox ( msg lst mtp / dch des tmp res )
    (cond
        (   (not
                (and
                    (setq tmp (vl-filename-mktemp nil nil ".dcl"))
                    (setq des (open tmp "w"))
                    (write-line
                        (strcat "listbox : dialog { label=\"" msg
                            "\"; spacer; : list_box { key=\"list\"; multiple_select="
                            (if mtp "true" "false") "; } spacer; ok_cancel; }"
                        )
                        des
                    )
                    (not (close des))
                    (< 0 (setq dch (load_dialog tmp)))
                    (new_dialog "listbox" dch)
                )
            )
            (prompt "\nError Loading List Box Dialog.")
        )
        (   t     
            (start_list "list")
            (foreach itm lst (add_list itm))
            (end_list)
            (setq res (set_tile "list" "0"))
            (action_tile "list" "(setq res $value)")
            (setq res
                (if (= 1 (start_dialog))
                    (mapcar '(lambda ( x ) (nth x lst)) (read (strcat "(" res ")")))
                )
            )
        )
    )
    (if (< 0 dch)
        (unload_dialog dch)
    )
    (if (and tmp (setq tmp (findfile tmp)))
        (vl-file-delete tmp)
    )
    res
)

(princ)