(defun sample-points ()
  (loop for i from 20 to 220 by 40
		collect i))

(defconstant *samples* (sample-points))

(defun read-lines (path)
  "opens a file and returns the lines as a list"
  (with-open-file (s path)
	(loop for line = (read-line s nil)
		  while line
		  collect line)))

(defun split (str)
  "splits the string into a list using a single 
  space as a delimiter"
  (loop for i = 0 then (1+ j)
		as j = (position #\Space str :start i)
		collect (subseq str i j)
		while j))

(defun cycles (str)
  (if (string= (car (split str)) "addx")
	2
	1))
