;;; 2048-game.el --- 2048 in Emacs
;; Copyright (C) 2014 -- Use at 'yer own risk  -- NO WARRANTY!
;; Based on 1024 by Veewo Studio and conceptually similar to Threes by Asher Vollmer.  (see https://github.com/gabrielecirulli/2048 )
;; Author: sam fiechter sam.fiechter(at)gmail
;; Version: 0.01
;; Created: 2014-03-24
;; Keywords: games

;;;Code

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar 2048-empty-board [  0 0 0 0    0 0 0 0    0 0 0 0    0 0 0 0])
(defvar 2048-score 0)
(defvar 2048-board (copy-sequence 2048-empty-board))

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar 2048-mode-map  (make-sparse-keymap '2048-mode-map))

(define-key 2048-mode-map "n"                '2048-start-game)
(define-key 2048-mode-map "q"                '2048-end-game)

(define-key 2048-mode-map [left]        '2048-move-left)
(define-key 2048-mode-map [right]        '2048-move-right)
(define-key 2048-mode-map [up]                '2048-move-up)
(define-key 2048-mode-map [down]        '2048-move-down)


(defvar 2048-null-map
  (make-sparse-keymap '2048-null-map))
(define-key 2048-null-map "n"                '2048-start-game)

;; ;;;;;;;;;;;;; functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 2048-start-game ()
  "New 2048 Game!"
  (interactive)
  (setq 2048-board (copy-sequence 2048-empty-board))
  (2048-add-another-number)
  (setq 2048-score 0)
  (setq cursor-type nil)
  (2048-draw)
  )

(defun 2048-end-game ()
  "Kills the buffer."
  (interactive)
  (setq 2048-board (copy-sequence 2048-empty-board))
  (setq 2048-score 0)
  (setq cursor-type box)
  (kill-buffer "*2048*"))

(defun 2048-move-right ()
  "Move the board right."
  (interactive)
  (2048-move 0 1 4))


(defun 2048-move-left ()
  "Move the board left."
  (interactive)
  (2048-move 3 -1 4))

(defun 2048-move-up ()
  "Move the board up."
  (interactive)
  (2048-move 12 -4 1 ))

(defun 2048-move-down ()
  "Move the board down."
  (interactive)
  (2048-move 0 4 1))

(defun 2048-slide (b n ms)
  "slide from b n times step ms"
(if (> n 0)
    (let ((z 0) (a 0) (ar 0))
      (dotimes (z n)
	(setq a (- b ms))
	(setq ar (aref 2048-board a))
	(if (= 0 ar) nil (setq moved 1))
	(aset 2048-board b ar)
	(setq b a) )
      (aset 2048-board a 0) ) nil ) )

(defun 2048-move (init ms rs)
  "Move the board array init is start place of the var ms is step
   for the move rs is row change"
 ;; (message "New Move!")
  (let ((moved 0) (x 0) (y 0) (z 0) (xx (copy-sequence [ 3 3 3 3 ])))
    (dotimes (x 3)
      (dotimes (y 4)
        (let* ( (n (aref xx y))  
		(b (+ (+ init (* y rs)) (* n  ms))) (br (aref 2048-board b))
                (a (- b ms)) (ar (aref 2048-board a)) )
          (if (= 0 br)
	      (2048-slide b n ms) 		    
            (progn 
              (if (= br ar)
                  (progn
                    (setq 2048-score (+ 2048-score ar))
                    (message (concat "Score : " (number-to-string 2048-score)))
                    (aset 2048-board a (* 2 ar))
		    (2048-slide b n ms)  )
		(if (= 0 ar) 
		    (2048-slide a (- n 1) ms)
		  (aset xx y (- n 1 )) ) )) ) ))
      (2048-draw)
      (sit-for (expt 0.9 (* 20 (+ 1  y))))
      )
    (if (= moved 1) (2048-add-another-number) nil)
    (2048-draw)  ))


(defun 2048-add-another-number ()
  "Add a Random Number to the Board"
  (let ((a 0) ( i 0) (y (list) ) (tb 2048-board)
	(rr (list 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4 4 4 8)))
    (dotimes (a 16)
      (if (= 0 (aref 2048-board a))
          (setq y (add-to-list 'y a))
        nil ))
    (if (= 0 (length y))
        (message "Game Over!  Press 'n' for another try...")
      (progn
        (aset tb (elt y (random (length y))) (elt rr (random (length rr))))
        (setq 2048-board tb) ))))

(define-derived-mode 2048-mode tabulated-list-mode "*2048*"
  "2048 game mode
  Keybindings:
  \\{2048-mode-map} "
  (use-local-map 2048-mode-map)
  (unless (featurep 'emacs)
    (setq mode-popup-menu
          '("2048 Commands"
            ["Start new game"        2048-start-game]
            ["End game"                2048-end-game
             (2048-active-p)]
            )))
  (setq tabulated-list-format [("" 4 t)
                               ("" 4 t)
                               ("" 4 t)
                               ("" 4 t)])
  (setq tabulated-list-padding 0)
  (tabulated-list-init-header))

(defun 2048-draw ()
  (interactive)
  (2048-listing-command)

  (tabulated-list-print t))

(defun 2048-color (i)
  "get the right color for the tile"
(let ((black '(:foreground "black"))
      (col (assoc i '((4  '(:foreground "grey")) (8  '(:foreground "LimeGreen"))  (16  '(:foreground "orange")) 
		      (32  '(:foreground "dark orange"))  (64  '(:foreground "red")) (128  '(:foreground "indian red" )) 
		      (256  '(:foreground "peru"))  (512  '(:foreground "goldenrod"))   (1024  '(:foreground "gold"))  
		      (2048  '(:foreground "yellow")) ))))
  (if col 
      (elt col 1)
    black) ))

(defun 2048-listing-command ()
  (interactive)
  ;;  (2048-mode)
  (let ((tbl (list) )
        (i 0) (j 0) (k 1) (ir 0)
        (tline [1 2 3 4])  (c " "))
    (dotimes (i 16)
      (setq ir (aref 2048-board i))
      (if (= 0 ir)
          (setq c " ")
	(let ((color (2048-color ir)))
        (setq c (propertize (number-to-string ir) 'face  color )) ))
;;        (setq c (propertize (number-to-string ir) 'font-lock-face '(:foreground "red" ))) ))
      (aset tline j c)
      (if (= 3 j)
          (progn
            (add-to-list 'tbl (list  k (copy-sequence tline) ) 1 (lambda (a b) nil))
            (setq k (+ 1 k))
            (setq j 0)
            )
        (setq j (+ 1 j)) ))
    (setq tabulated-list-entries tbl) ))

;;;###autoload
(defun 2048-game ()
  "Play 2048
Emacs implementation of Gabriele Cirulli's. Based on 1024 by Veewo Studio and conceptually similar to Threes by Asher Vollmer.  
See https://github.com/gabrielecirulli/2048 

2048-mode keybindings:
   \\<2048-mode-map>
\\[2048-start-game]        Start a new game
\\[2048-end-game]        Terminate the current game
\\[2048-move-left]        Moves the board to the left
\\[2048-move-right]        Moves the board to the right
\\[2048-move-up]        Moves the board to the up
\\[2048-move-down]        Moves the board to the down
"
  (interactive)
  (pop-to-buffer "*2048*" nil)
  (2048-mode)
  (2048-start-game))


(provide '2048-game)

;;; 2048.el ends here
