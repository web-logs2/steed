;; Copyright (c) 2023 Yi Yang <kelthuzadx@qq.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>

(defmacro when (cond body)
    `(if ,cond (block ,body))
)

(defmacro unless (cond body)
    `(if (not ,cond) (block ,body))
)

(defmacro dotimes (name limit body)
    `(for (,name 0) (< ,name ,limit) (+ ,name 1) ,body)
)

(defmacro rcons (rest first)
     `(cons ,first ,rest)
)