; 73-74

  ; 这两题目是一个题目，代码放在一起

; A Posn represents the state of the world

; ===图形常量
; number->image
(define MTS (empty-scene 300 300))
(define DOT (circle 3 "solid" "red"))

; ===提醒文案
(define REMIND-MSG 
  (place-image (text "程序已停止 \n\n   注意:\n\n   x 坐标值位于 [0,297] \n   y 坐标值位于 [0,300]" 14 " black") 150 200 MTS))
; \n 是指换行

; 时钟函数
; 时钟滴答一次，红点 x + 3，y 值保持不变
; Posn -> Posn
(define (x+ p)
  (posn-up-x p (+ (posn-x p) 3)))

; 定义更新器
; Posn Number -> Posn
(define (posn-up-x p n )
  (make-posn n (posn-y p)))

; 这题目比较简单，引入了更新器，有点多余。估计该题目的意图，正是为了引入更新器这概念。
; 本题中
; 时钟函数：传递参数给更新器
; 更新器：承担了数据计算的任务
; 二者配合完成了任务
; 复杂的程序，是不是也是这样子，待验证

; 鼠标事件函数
; poson number number mouseevent -> posn 
; 鼠标点击，重置红点
; 题目中并没有详细解释，重置是什么定义，从代码来看，所谓的重置：鼠标点在那里，红点就出现在那里

(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-down")
  (make-posn 29 31))

(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-up")
  (make-posn 10 20))

(define (reset-dot p x y me)
  (cond
    [(mouse=? me "button-down") (make-posn x y)]
    [else p]))

; 注：鼠标事件函数这里，按下鼠标后，立即重置红点，其实不算太严谨，更为严谨的方式
; 按下鼠标，再松开鼠标，这时才重置红点
; 但这样子，相对麻烦，而且这一点，也不是本题的重点，所以，还是采用书上的方法。

; 绘制图像函数
; posn-> image 
(check-expect (scene+dot (make-posn 30 20))
  (place-image DOT 30 20 MTS))

(define (draw-dot-on-scene p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

(check-expect (in-bounds? (make-posn 0 0)) #true)
(check-expect (in-bounds? (make-posn 297  300)) #true)
(check-expect (in-bounds? (make-posn 298  300)) #false)
(check-expect (in-bounds? (make-posn 297  301)) #false)

(define (in-bounds? p)
  (and
    (>=(posn-x p) 0) (<= (posn-x p) 297)
    (>= (posn-y p) 0) (<= (posn-y p) 300)))

(check-expect (scene+dot (make-posn 0 0))
              (place-image DOT 0 0 MTS)) 
(check-expect (scene+dot (make-posn 298 301))
              REMIND-MSG)  

(define (scene+dot p)
  (cond
    [(in-bounds? p) (draw-dot-on-scene p)]
    [else REMIND-MSG]))

; 停止函数
; posn -> boolean 
(check-expect (end-condition? (make-posn 10 300)) #false)

(define (end-condition? p)
  (> (posn-x p) 298))
; 注
; 显示提醒文案之后，停止函数才停止程序。

; 定义世界程序
; Posn -> Posn

(define (main p)
  (big-bang p
    [on-tick x+]
    [on-mouse reset-dot]
    [to-draw scene+dot]
    [stop-when end-condition?]))
    
(main (make-posn 10 150 ))

