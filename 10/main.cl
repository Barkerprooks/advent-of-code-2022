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

(defun parse-instruction (str)
  (let ((tokens (split str)))
	(nth (- (length tokens) 1) tokens)))

(print (parse-instruction "addx 1"))
