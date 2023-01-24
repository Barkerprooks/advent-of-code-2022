(defun read-lines (path)
  "opens a file and returns the lines as a list"
  (with-open-file (s path)
	(loop for line = (read-line s nil)
		  while line
		  collect line)))

(defun split (string)
  "splits the string into a list using a single 
  space as a delimiter"
  (loop for i = 0 then (1+ j)
		as j = (position #\Space string :start i)
		collect (subseq string i j)
		while j))
