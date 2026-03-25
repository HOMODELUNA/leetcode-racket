#lang racket
;https://leetcode.cn/problems/reverse-words-in-a-string/?envType=study-plan-v2&envId=top-interview-150
; 2026.03.26

(define/contract (reverse-words s)
  (-> string? string?)
  (string-join (reverse (string-split s))))
