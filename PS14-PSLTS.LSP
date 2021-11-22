; PS14 definition
(defun c:PS14()
  (foreach lay (layoutlist)
    (command "_LAYOUT" "_Set" lay "PSLTSCALE" 0)
    (command "_LAYOUT" "_Set" lay "LTSCALE" 48)
    (command "_pspace")
    (command "_zoom" "extents")
  );end foreach
);endPS0 (c:PS14)