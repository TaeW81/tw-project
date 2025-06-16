;;-----------------------------------------------------------------------------
;; PROFILEEDIT - 종단면도 라인의 끝점을 숫자 값에 따라 이동
;;-----------------------------------------------------------------------------
(defun c:PROFILEEDIT (/ ent data pt1 pt2 info1 info2 val1 val2 txt1 txt2 ang)
  (vl-load-com)
  (prompt "\n종단면도 라인을 선택하세요: ")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (setq data (entget ent)
            pt1  (cdr (assoc 10 data))
            pt2  (cdr (assoc 11 data))
            info1 (getNearestNumericTextInfo pt1)
            info2 (getNearestNumericTextInfo pt2)
            val1  (car info1)
            txt1  (cadr info1)
            val2  (car info2)
            txt2  (cadr info2))
      (if (and val1 val2)
        (progn
          (cond
            ((> val1 val2)
             (setq pt1 (offsetPoint pt1 4)
                   pt2 (offsetPoint pt2 -4)))
            ((< val1 val2)
             (setq pt1 (offsetPoint pt1 -4)
                   pt2 (offsetPoint pt2 4))))
          (modifyLine ent pt1 pt2)
          (setq ang (angle pt1 pt2))
          (if txt1 (rotateText txt1 ang))
          (if txt2 (rotateText txt2 ang))
        )
        (prompt "\n끝점 근처에서 숫자 텍스트를 찾을 수 없습니다."))
    )
    (prompt "\n선택된 객체가 없습니다."))
  (princ))


;; 숫자 텍스트 중 가장 가까운 값을 값과 엔티티 형태로 반환한다.
(defun getNearestNumericTextInfo (pt / ss idx ent ed txt val ins dist bestVal bestEnt bestDist)
  (setq ss (ssget "_C"
                   (list (- (car pt) 10.0) (- (cadr pt) 10.0))
                   (list (+ (car pt) 10.0) (+ (cadr pt) 10.0))
                   '((0 . "TEXT,MTEXT"))))
  (setq idx 0)
  (while (and ss (< idx (sslength ss)))
    (setq ent (ssname ss idx)
          ed  (entget ent)
          txt (cdr (assoc 1 ed))
          val (distof txt 2))
    (if val
      (progn
        (setq ins (cdr (assoc 10 ed))
              dist (distance pt ins))
        (if (< dist 10.0)
          (if (or (not bestDist) (< dist bestDist))
            (setq bestDist dist bestVal val bestEnt ent)))))
    (setq idx (1+ idx)))
  (if bestVal (list bestVal bestEnt) nil))

;; 주어진 점의 Y 좌표를 dy 만큼 이동
(defun offsetPoint (pt dy)
  (list (car pt) (+ (cadr pt) dy) (if (caddr pt) (caddr pt) 0.0)))

;; 라인의 시작점과 끝점을 수정
(defun modifyLine (ent newPt1 newPt2 / obj)
  (setq obj (vlax-ename->vla-object ent))
  (vla-put-StartPoint obj (vlax-3d-point newPt1))
  (vla-put-EndPoint obj (vlax-3d-point newPt2)))

;; 텍스트 객체의 회전 각도를 지정
(defun rotateText (ent ang / obj)
  (setq obj (vlax-ename->vla-object ent))
  (vla-put-Rotation obj ang))

(princ "\nPROFILEEDIT 리습이 로드되었습니다. PROFILEEDIT 명령을 사용하세요.")
(princ)
