;;; ParserMonad.el --- Parser monad that parses Lisp lists      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ernests Kuznecovs

;; Author: Ernests Kuznecovs <ernestkuznecovs@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-package monad
  :straight
  (monad
   :type git
   :host github
   :repo "cute-jumper/monad.el"))

(require 'cl-lib)

;; The type { a, b } denotes that the function accepts two arguments of type a and b

;; Parser :: { LispList -> Maybe (a . LispList) } -> Parser a
(defun Parser (f) (cons 'Parser f))

;; Parser-zero :: Parser a
(setq Parser-zero (Parser (lambda (_) Nothing)))

;; item :: Parser a
(setq Parser-item (Parser (lambda (inp) (if inp (Just (cons (car inp) (cdr inp))) Nothing))))

;; Parser-run :: { Parser a, LispList } -> Maybe (a . LispList)
(defun Parser-run (parser inp) (funcall (cdr parser) inp))

;; Parser-return :: { a } -> Parser a
(defun Parser-return (v) (Parser (lambda (inp) (Just (cons v inp)))))

;; Parser-bind :: { Parser a, (a -> Parser b) } -> Parser b
(defun Parser-bind (p f)
  (Parser (lambda (inp) (pcase (Parser-run p inp)
			  (`(Just . ,pair) (Parser-run (funcall f (car pair)) (cdr pair)))
			  (_               (Parser-run Parser-zero inp))))))

;; Parser-blind :: { Parser a, Parser b } -> Parser b
(defun Parser-blind (p1 p2) (Parser-bind p1 (lambda (_) p2)))

;; Parser-sat :: { a -> Bool } -> Parser a
(defun Parser-sat (p)
  (monad-do Parser
    (x Parser-item)
    ((guard (funcall p x)))
    (return x)))

;; Parser-plus :: { Parser a, Parser a } -> Parser a
(defun Parser-plus (p1 p2) (Parser (lambda (inp) (Maybe-plus (Parser-run p1 inp) (Parser-run p2 inp)))))

;; Parser-lispItem :: { LispItem } -> Parser LispItem
(defun Parser-lispItem (item) (Parser-sat (lambda (x) (equal item x))))

;; Parser-list :: { LispList } -> Parser LispList
(defun Parser-list (list)
  (if list
  (monad-do Parser
    (x  (Parser-lispItem (car list)))
    (xs (Parser-list     (cdr list)))
    (return (cons x xs)))
  (Parser-return nil)))

;; Parser-many :: { Parser a } -> Parser [a]
(defun Parser-many (p)
  (Parser-plus
   (monad-do Parser
     (x p)
     (xs (Parser-many p))
     (return (cons x xs)))
   (Parser-return nil)))

;; Parser-many :: { Parser a } -> Parser [a]
(defun Parser-many1 (p)
  (monad-do Parser
     (x p)
     (xs (Parser-many p))
     (return (cons x xs))))

;; Parser-sepby :: { Parser a, Parser b } -> Parser [a]
(defun Parser-sepby (p sep)
  (Parser-plus (Parser-sepby1 p sep) (Parser-return nil)))

;; Parser-sepby1 :: { Parser a, Parser b } -> Parser [a]
(defun Parser-sepby1 (p sep)
  (monad-do Parser
    (x p)
    (xs (Parser-many (Parser-blind sep p)))
    (return (cons x xs))))

;; Parser-bracket :: { Parser a, Parser b, Parser c } -> Parser b
(defun Parser-bracket (open p close)
  (monad-do Parser
    (open)
    (x p)
    (close)
    (return x)))

;; Parser-symbol :: Parser Symbol
(setq Parser-symbol (Parser-sat 'symbolp))
;; Parser-list   :: Parser LispList
(setq Parser-list   (Parser-sat 'listp))
;; Parser-number :: Parser Number
(setq Parser-number (Parser-sat 'numberp))
;; Parser-string :: Parser String
(setq Parser-string (Parser-sat 'stringp))

;; Maybe-plus :: { Maybe a, Maybe a } -> Maybe a
(defun Maybe-plus (m1 m2)
  (pcase (list m1 m2)
    (`((Just . ,x)  ,_)   m1)
    (`(Nothing      ,x)   m2)))
