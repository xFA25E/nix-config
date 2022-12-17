;;; tempo-ext-test.el --- Tests for tempo-ext        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Valeriy Litkovskyy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'tempo-ext)

(ert-deftest tempo-ext-test-between ()
  (let ((tree (avl-tree-create #'<)))
    (avl-tree-enter tree 3)
    (avl-tree-enter tree 7)
    (avl-tree-enter tree 5)
    (avl-tree-enter tree 1)
    (avl-tree-enter tree 9)

    (should (equal (avl-tree-between tree 0) (cons nil 1)))
    (should (equal (avl-tree-between tree 1) (cons nil 3)))
    (should (equal (avl-tree-between tree 2) (cons 1 3)))
    (should (equal (avl-tree-between tree 3) (cons 1 5)))
    (should (equal (avl-tree-between tree 4) (cons 3 5)))
    (should (equal (avl-tree-between tree 5) (cons 3 7)))
    (should (equal (avl-tree-between tree 6) (cons 5 7)))
    (should (equal (avl-tree-between tree 7) (cons 5 9)))
    (should (equal (avl-tree-between tree 8) (cons 7 9)))
    (should (equal (avl-tree-between tree 9) (cons 7 nil)))
    (should (equal (avl-tree-between tree 10) (cons 9 nil)))))

(provide 'tempo-ext-test)
;;; tempo-ext-test.el ends here
