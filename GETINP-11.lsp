;;; GETINP-11.LSP 
;;; EPANET INP 파일 자동 생성 스크립트
;;; 작성일: 2024-03-21
;;; 수정일: 2024-03-22

(vl-load-com)

;; ACAD 문자열 변환 유틸리티 로드
(if (not acet-str-to-list)
    (vl-load-comfile "acet.vlx")
)

;; 메시지 상수 정의
(setq MSG_SELECT_ALL       "전체를 선택하세요: ")
(setq MSG_NO_SELECTION    "선택된 객체가 없습니다.")
(setq MSG_NO_OBJECTS      "Junction과 Pipeline이 모두 선택되어야 합니다.")
(setq MSG_JUNCTION_COUNT  "추출된 Junction 객체 수: ")
(setq MSG_PIPELINE_COUNT  "추출된 Pipeline 객체 수: ")
(setq MSG_COORDINATES     "Coordinates 정보를 기록 중...")
(setq MSG_PROCESSING      "처리 중...")
(setq MSG_COMPLETE        "완료되었습니다.")
(setq MSG_FILE_SAVED      "INP 파일이 생성되었습니다: ")
(setq MSG_PIPE_INFO       "파이프 정보: ")
(setq MSG_LENGTH          "길이: ")
(setq MSG_VERTICES        "정점 수: ")
(setq MSG_DATA_SORT       "데이터 정렬 중...")

;; 한글 출력을 위한 문자열 변환 함수
(defun convert-string (str)
  (acet-list-to-str 
    (acet-str-to-list str)
  )
)

;; 한글 메시지 출력 함수
(defun princ-kr (str)
  (princ str)
)

(defun C:GETINP ( / fname selected-ss junction-list pipeline-list file)
  ;; 선택 객체 생성
  (setq selected-ss (ssget))
  (if (null selected-ss)
      (progn
        (princ "\n선택된 객체가 없습니다.")
        (exit)
      )
  )
  ;; 선택된 객체들의 타입별 리스트 생성
  (setq junction-list '()
        pipeline-list '())
  (setq i 0)
  (repeat (sslength selected-ss)
    (setq ent (ssname selected-ss i))
    (setq ent-data (entget ent))
    (cond 
      ((and (= "TEXT" (cdr (assoc 0 ent-data)))
            (= "L_Junction" (cdr (assoc 8 ent-data))))
       (setq junction-list (cons ent junction-list)))
      ((and (= "LWPOLYLINE" (cdr (assoc 0 ent-data)))
            (= "L_pipeline" (cdr (assoc 8 ent-data))))
       (setq pipeline-list (cons ent pipeline-list)))
    )
    (setq i (1+ i))
  )
  (if (or (null junction-list) (null pipeline-list))
      (progn
        (princ "\nJunction과 Pipeline이 모두 선택되어야 합니다.")
        (exit)
      )
  )
  (setq junction-ss (ssadd))
  (foreach ent junction-list (ssadd ent junction-ss))
  (setq pipeline-ss (ssadd))
  (foreach ent pipeline-list (ssadd ent pipeline-ss))
  ;; 파일 생성 (동일 이름이 있으면 뒤에 연번 붙임)
  (setq dwg-path (getvar "DWGPREFIX"))
  (setq fname (vl-filename-base (getvar "DWGNAME")))
  (setq full-path (strcat dwg-path fname ".inp"))
  (setq idx 1)
  (while (findfile full-path)
    (setq full-path (strcat dwg-path fname "_" (rtos idx 2 0) ".inp"))
    (setq idx (1+ idx))
  )
  (setq file (open full-path "w"))
  ;; [TITLE]
  (write-line "[TITLE]" file)
  (write-line fname file)
  (write-line "" file)
  ;; [JUNCTIONS] & [RESERVOIRS]
  (write-junctions-and-reservoirs file junction-ss)
  ;; [COORDINATES]
  (write-coordinates file junction-ss)
  ;; [PIPES] & [VERTICES]
  (write-pipes-and-vertices file pipeline-ss)
  ;; 기타 정보 추가 생략
  (close file)
  (princ)
)

(defun write-junctions-and-reservoirs (file selected-ss / ss id elev junctions reservoirs i ent pt)
  (setq ss selected-ss)
  (if ss
    (progn
      (setq junctions '()
            reservoirs '())
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq id (cdr (assoc 1 (entget ent))))
        (setq pt (cdr (assoc 10 (entget ent))))
        (setq elev (get_text_at_point pt "L_FH"))
        (if (= id "1")
            (setq reservoirs (cons (list id elev) reservoirs))
            (setq junctions (cons (list id elev "0" "") junctions))
        )
        (setq i (1+ i))
      )
      (if junctions
          (progn
            (setq junctions 
                  (vl-sort junctions 
                          '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))
            (write-line "[JUNCTIONS]" file)
            (write-line ";ID              Elev            Demand" file)
            (foreach junction junctions
              (write-line (strcat (nth 0 junction) "               "
                                 (nth 1 junction) "              "
                                 (nth 2 junction))
                         file)
            )
            (write-line "" file)
          )
      )
      (if reservoirs
          (progn
            (write-line "[RESERVOIRS]" file)
            (write-line ";ID              Head" file)
            (foreach reservoir reservoirs
              (write-line (strcat (nth 0 reservoir) "               "
                                 (nth 1 reservoir))
                         file)
            )
            (write-line "" file)
          )
      )
    )
  )
)

(defun write-coordinates (file selected-ss / ss obj txt pt x y i)
  (setq ss selected-ss)
  (if ss
    (progn
      (write-line "[COORDINATES]" file)
      (write-line ";Node            X-Coord         Y-Coord" file)
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (ssname ss i))
        (setq obj-data (entget obj))
        (if (and (= (cdr (assoc 0 obj-data)) "TEXT")
                 (= (cdr (assoc 8 obj-data)) "L_Junction"))
          (progn
            (setq txt (cdr (assoc 1 obj-data)))
            (setq pt (cdr (assoc 10 obj-data)))
            (setq x (rtos (car pt) 2 12))
            (setq y (rtos (cadr pt) 2 12))
            (write-line (strcat txt "               " x "         " y) file)
          )
        )
        (setq i (1+ i))
      )
      (write-line "" file)
    )
  )
)

(defun get_text_at_point (pt layer / ss txt)
  (setq ss (ssget "C" (list (- (car pt) 2.0) (- (cadr pt) 2.0)) 
                   (list (+ (car pt) 2.0) (+ (cadr pt) 2.0)) 
                   (list '(0 . "TEXT") (cons 8 layer))))
  (if ss (setq txt (cdr (assoc 1 (entget (ssname ss 0))))) (setq txt "N/A"))
  txt
)

(defun get_text_near_point (pt layer / ss txt)
  (setq ss (ssget "C" (list (- (car pt) 1) (- (cadr pt) 1)) 
                   (list (+ (car pt) 1) (+ (cadr pt) 1)) 
                   (list '(0 . "TEXT") (cons 8 layer))))
  (if ss (setq txt (cdr (assoc 1 (entget (ssname ss 0))))) (setq txt "N/A"))
  txt
)

(defun get_polyline_vertices (pline / num-verts vertices i pt)
  (setq num-verts (vlax-curve-getendparam pline))
  (setq vertices '())
  (setq i 0)
  (while (<= i num-verts)
    (setq pt (vlax-curve-getpointatparam pline i))
    (if (not (member pt vertices)) (setq vertices (append vertices (list pt))))
    (setq i (1+ i)))
  vertices
)

(defun get_entity_vertices (ent / obj type pts startpt endpt midpt)
  (setq obj (vlax-ename->vla-object ent))
  (setq type (cdr (assoc 0 (entget ent))))
  (setq pts '())
  (cond
    ((= type "LWPOLYLINE") (setq pts (get_polyline_vertices ent)))
    ((= type "ARC")
      (setq startpt (vlax-get obj 'StartPoint))
      (setq endpt (vlax-get obj 'EndPoint))
      (setq midpt (vlax-curve-getPointAtDist ent (/ (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)) 2.0)))
      (setq pts (list startpt midpt endpt)))
    ((= type "LINE")
      (setq startpt (vlax-get obj 'StartPoint))
      (setq endpt (vlax-get obj 'EndPoint))
      (setq pts (list startpt endpt)))
    (T (setq pts '()))
  )
  pts
)

;; orientation helper
(defun orient-seg-points (pts startRef /)
  (cond
    ((equal (car pts) startRef 1e-8) pts)
    ((equal (last pts) startRef 1e-8) (reverse pts))
    (T pts)
  )
)

(defun write-pipes-and-vertices (file pipeline-ss / ent id node1 node2 len dia data_list vertices_list i segs segEnt segPts vtxlist polyStart lastPt midpt startpt endpt)
  (vl-load-com)
  (princ-kr MSG_PIPE_INFO)
  (princ-kr (strcat MSG_PIPELINE_COUNT (itoa (sslength pipeline-ss))))
  (if pipeline-ss
    (progn
      (setq data_list '() vertices_list '())
      (setq i 0)
      (repeat (sslength pipeline-ss)
        (setq ent (ssname pipeline-ss i))
        (setq segs (acet-geom-object-segments ent))
        (setq vtxlist '())
        (setq polyStart (vlax-curve-getstartpoint ent))
        ;; 첫 세그먼트 처리
        (if segs
          (progn
            (setq segEnt (car segs))
            (setq segPts (get_entity_vertices segEnt))
            (setq segPts (orient-seg-points segPts polyStart))
            (setq vtxlist segPts)
            (setq lastPt (last vtxlist))
            ;; 나머지 세그먼트 순환
            (foreach segEnt (cdr segs)
              (setq segPts (get_entity_vertices segEnt))
              (setq segPts (orient-seg-points segPts lastPt))
              (if (equal (car segPts) lastPt 1e-8)
                  (setq segPts (cdr segPts)))
              (setq vtxlist (append vtxlist segPts))
              (setq lastPt (last vtxlist))
            )
          )
        )
        (setq len (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent)))
        (setq midpt (vlax-curve-getpointatdist ent (/ len 2.0)))
        (setq id (get_text_near_point midpt "L_pipes"))
        (setq startpt (vlax-curve-getstartpoint ent))
        (setq node1 (get_text_near_point startpt "L_Junction"))
        (setq endpt (vlax-curve-getendpoint ent))
        (setq node2 (get_text_near_point endpt "L_Junction"))
        (setq dia (get_text_near_point midpt "L_diameter"))
        (setq data_list 
              (cons (list id node1 node2 (rtos len 2 2) dia "110" "0" "Open") data_list))
        (foreach vtx vtxlist
          (setq vertices_list (cons (list id (rtos (car vtx) 2 12) (rtos (cadr vtx) 2 12)) vertices_list)))
        (setq i (1+ i))
      )
      (setq data_list (vl-sort data_list '(lambda (a b) (< (car a) (car b)))))
      (setq vertices_list (vl-sort vertices_list '(lambda (a b) (< (car a) (car b)))))
      (princ-kr MSG_DATA_SORT)
      (write-line "[PIPES]" file)
      (write-line ";ID              Node1           Node2           Length          Diameter        Roughness       MinorLoss       Status" file)
      (foreach data data_list
        (write-line (strcat (nth 0 data) "               "
                           (nth 1 data) "              "
                           (nth 2 data) "              "
                           (nth 3 data) "              "
                           (nth 4 data) "              "
                           (nth 5 data) "              "
                           (nth 6 data) "              "
                           (nth 7 data))
                   file)
      )
      (write-line "" file)
      (write-line "[VERTICES]" file)
      (write-line ";Link            X-Coord         Y-Coord" file)
      (foreach vtx vertices_list
        (write-line (strcat (nth 0 vtx) "               "
                           (nth 1 vtx) "         "
                           (nth 2 vtx))
                   file)
      )
      (write-line "" file)
      (princ-kr MSG_COMPLETE)
    )
    (princ-kr "\n추출된 Pipeline 객체가 없습니다.")
  )
)
