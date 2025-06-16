;; line-rotate.lsp
;; Rotate text entities near a line when it is updated.
;;
;; Functions:
;;  - perpDistance:   return perpendicular distance from point to line.
;;  - rotateText:     rotate TEXT/MTEXT to given angle.
;;  - getTextsNearLine: collect TEXT/MTEXT within band of line.
;;  - c:UpdateLineText: example command to update a line and rotate nearby text.

(defun perpDistance (pt p1 p2 / area len)
  (setq area (abs (- (* (- (car pt) (car p1)) (- (cadr p2) (cadr p1)))
                     (* (- (cadr pt) (cadr p1)) (- (car p2) (car p1)))))
        len (distance p1 p2))
  (if (zerop len)
      0.0
      (/ area len)))

(defun rotateText (ent ang / ed)
  (if (setq ed (entget ent))
      (progn
        (setq ed (subst (cons 50 ang) (assoc 50 ed) ed))
        (entmod ed)
        (entupd ent)))
  ent)

(defun getTextsNearLine (pt1 pt2 band / minx maxx miny maxy ss res i ent p d)
  (setq minx (min (car pt1) (car pt2))
        maxx (max (car pt1) (car pt2))
        miny (min (cadr pt1) (cadr pt2))
        maxy (max (cadr pt1) (cadr pt2)))
  (setq ss (ssget "_W"
                  (list (- minx band) (- miny band))
                  (list (+ maxx band) (+ maxy band))
                  '((0 . "TEXT,MTEXT"))))
  (setq res '())
  (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq ent (ssname ss i))
          (setq p (cdr (assoc 10 (entget ent))))
          (setq d (perpDistance p pt1 pt2))
          (if (<= d band)
              (setq res (cons ent res)))
          (setq i (1+ i)))))
  (reverse res))

(defun c:UpdateLineText (/ ent ed pt1 pt2 ang mid near)
  (vl-load-com)
  (if (setq ent (car (entsel "\nSelect line: ")))
      (progn
        (setq ed (entget ent))
        (setq pt1 (cdr (assoc 10 ed)))
        (setq pt2 (cdr (assoc 11 ed)))
        (if (setq pt2 (getpoint "\nSpecify new endpoint: "))
            (progn
              (setq ed (subst (cons 11 pt2) (assoc 11 ed) ed))
              (entmod ed)
              (setq ang (angle pt1 pt2))
              ;; rotate text at midpoint if any
              (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))
              (setq near (getTextsNearLine pt1 pt2 4.0))
              (foreach t near (rotateText t ang))
              (princ "\nTexts rotated."))))))

(princ "\nType UPDATELINETEXT to run.")
