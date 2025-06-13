;;; streetinlet_install.lsp
;;; AutoCAD LISP script to automatically place street inlets along a pipe route.
;;; Command: STINLET
;;;
;;; Usage:
;;;   1. Select the main pipe polyline.
;;;   2. Select the left road edge polyline.
;;;   3. Select the right road edge polyline.
;;;   4. Enter spacing distance in drawing units (meters).
;;;
;;; The script will insert the block named "Street inlet" on both sides of the
;;; pipe at the specified interval. Optional perpendicular guide lines can be
;;; created by setting *stinlet-draw-line* to T.

(defun c:stinlet ( / pipe leftEdge rightEdge spacing totalLen curDist pt leftPt rightPt)
  ;; Load Visual LISP functions
  (vl-load-com)

  ;; Prompt user for objects and spacing
  (setq pipe     (car (entsel "\n관로 라인을 선택하세요: "))
        leftEdge (car (entsel "\n좌측 차도 끝 라인을 선택하세요: "))
        rightEdge(car (entsel "\n우측 차도 끝 라인을 선택하세요: "))
        spacing  (getreal "\n우수받이 간격(m): "))
  )

  ;; Validate input
  (if (not (and pipe leftEdge rightEdge spacing))
      (progn
        (princ "\n입력이 취소되었거나 잘못되었습니다.")
      )
    (progn
      ;; Calculate total length of the pipe polyline
      (setq totalLen (vlax-curve-getDistAtParam pipe
                          (vlax-curve-getEndParam pipe)))
      (setq curDist 0.0)

      ;; Loop over the pipe length at given spacing
      (while (<= curDist totalLen)
        (setq pt (vlax-curve-getPointAtDist pipe curDist))

        ;; Find perpendicular points on the left and right edges
        (setq leftPt  (vlax-curve-getClosestPointTo leftEdge pt))
        (setq rightPt (vlax-curve-getClosestPointTo rightEdge pt))

        ;; Optionally draw the perpendicular guide lines
        (when *stinlet-draw-line*
          (entmake (list '(0 . "LINE") (cons 10 pt) (cons 11 leftPt)))
          (entmake (list '(0 . "LINE") (cons 10 pt) (cons 11 rightPt))))

        ;; Insert the street inlet blocks
        (command "_.-insert" "Street inlet" leftPt "1" "1" "0")
        (command "_.-insert" "Street inlet" rightPt "1" "1" "0")

        ;; Advance to the next point
        (setq curDist (+ curDist spacing))
      )
    )
  )
  (princ)
)

;; Set to T to keep guide lines, NIL to delete/omit them
(setq *stinlet-draw-line* nil)

(princ "\nType STINLET to place street inlets along a pipe.")

