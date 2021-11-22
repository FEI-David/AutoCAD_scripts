; PS0 definition
(defun c:PS1()
  (foreach lay (layoutlist)
    (command "_LAYOUT" "_Set" lay "PSLTSCALE" 1)
    (command "_LAYOUT" "_Set" lay "LTSCALE" 0.375)
    (command "_pspace")
    (command "_zoom" "extents")
  );end foreach
);endPS0 (c:PS0)