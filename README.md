# Prolog

Базовая версия пролога, написанная на языке Haskell.

Возможности программы:

* загрузка базы знаний из внешнего файла;
* поиск информации в базе знаний;
* интерактивный режим (REPL), позволяющий выполнять запросы к базе знаний.

Остутствующие на данный момент возможности:

* incremental mode;
* работа со списками;
* работа с числами;
* обработка анонимных переменных;
* обработка комментариев в программе.

## Usage

Для сборки и запуска проекта используется `stack`. Чтобы запустить программу
используйте команду `stack run путь_до_базы_знаний`, например:

```bash
stack run example/likes.prolog
```

Пользователям GNU/Linux рекоммендуется использовать программу [rlwrap](https://github.com/hanslub42/rlwrap)
для более удобной работы с REPL:

```bash
rlwrap stack run example/likes.prolog
```

Вводите запросы к базе знаний на языке пролог. Запрос должен заканчиваться точкой.
Для выхода из программы введите `:q` или `:quit`.

## Примеры работы программы

```bash
$ rlwrap stack run example/likes.prolog
likes(kim, robin).
likes(sandy, lee).
likes(sandy, kim).
likes(robin, cats).
likes(sandy, X) :- likes(X, cats).
likes(kim, X) :- likes(X, lee), likes(X, kim).
likes(X, X).

?- likes(kim, robin).
true
?- likes(robin, kim).

?- likes(robin).

?- likes(sandy, Who).
Who = lee.
Who = kim.
Who = robin.
Who = sandy.
Who = cats.
Who = sandy.
?- likes(X, Y).
X = kim, Y = robin.
X = sandy, Y = lee.
X = sandy, Y = kim.
X = robin, Y = cats.
X = sandy, Y = robin.
X = sandy, Y = sandy.
X = sandy, Y = cats.
X = kim, Y = sandy.
X = X~1, Y = X~1.
?- :q
```

## References

1. [Paradigms of Artificial Intelligence Programming](https://github.com/norvig/paip-lisp) by Peter Norvig.
2. [Learn Prolog NOW!](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-html) by Patrick Blackburn, Johan Bos, and Kristina Striegnitz.
3. [Write Yourself a Scheme in 48 hours](http://web.archive.org/web/20131030041702/http://jonathan.tang.name:80/files/scheme_in_48/tutorial/parser.html) by Jonathan Tang.
4. [Simple REPL in Haskell](https://github.com/joelchelliah/simple-repl-in-haskell) by Joel Chelliah.
5. [Learn You a Haskell for Great Good](http://learnyouahaskell.com/chapters) by Miran Lipovača.
