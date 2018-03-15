hs2s-lab  
========
![](http://www.haskell.org/wikiupload/8/8d/Dhug.png)
### Что делать и как?
В этом задании придётся вытаскивать данные из html-страниц, преобразовывать их, фильтровать, компоновать и выводить на экран.
Для того, чтобы взять себе задание, сделайте сюда pull-request с этим файлом, вписав своё имя в таблицу (она находится внизу) напротив интересующего вас пункта. Одно задание можно взять нескольким студентам.

### Немоного о монадах
Для выполнения это части лабораторной работы нужно понимать принцип работы монад, в частности монады IO. В лецкиях мы придём к монадам через теорию категорий и функторы с моноидами, поэтому здесь будет маленький ликбез на эту тему. Хотя, многие, судя по первой лабе, уже разобрались сами и могут его пропустить.  
В Хаскелле монады изначально появились чтобы как-то вписать грязный процесс ввода-вывода в детемнированную чистоту этого функционального языка, и конструкция do является лишь синтаксическим сахаром, прячущим от вас цепочку монадических преобразований.  
Любой тип, принадлежащий к классу монад описывается как-то так: `m a`  
Посмотрите типы у выражений:  
`:t return`  
`:t putStrLn "hi!"`  
`:t Just 1`  
Для каждой монады определены две функции: `>>=  :: Monad m => m a -> (a -> m b) -> m b` и `>>  :: Monad m => m a -> m b -> m b` Первая передаёт некие значения из монады дальше в цепочку вычислений, возвращающей монады, вторая - нет.
Давайте посмотрим, как это работает для ввода-вывода:  
`putStr "hi " >> putStr "there!"`  
`getLine >>= (\a -> print a)`  
Это тоже самое, если бы мы в нашей программе написали бы:  
```haskell
do
  putStr "hi "
  putStr "there!"
```
и  
```haskell
do
  a <- getLine
  print a
```
И да, блок do тоже должен возвращать какую-либо монаду. Если вы пишете чистую функцию, которую затем планируете использовать в блоке `do`, то можете использовать `return ()`, которая создаёт монаду из ничего.  
Хорошо про монады на примере ввода-вывода в хаскелле написал Евгений Кирпичёв: http://rsdn.ru/article/funcprog/monad.xml

### Модули и пакеты
Кроме того, для выполнения лабы придётся разобраться с пакетным менеджером. В хаскелле он называется cabal, а само хранилище репозиториев - hackage. Поиск по пакетам - hoogle. Аналоги для других ЯП: Ruby - gems, Node.js - npm, , Python - pypi, Perl - cpan, PHP - pecl, или что там ещё придумали.  
Установка cabal проста. Я подключил себе ppa hvr/ghc, настроил симлинки и работал.  
Поставить любой пакет можно выполнив команду
`cabal install <packet_name>`

Вам, скорее всего, придётся поставить пакеты `http-conduit html-conduit xml-conduit`. О них можно почитать [тут](http://www.yesodweb.com/book/http-conduit), [тут](http://hackage.haskell.org/package/xml-conduit-0.7.0.1/docs/Text-XML-Cursor.html) и [тут](https://hackage.haskell.org/package/html-conduit) 
Кроме того, мы будем использовать стандартный модуль Data.Text, про него [здесь](http://hackage.haskell.org/package/text-0.11.2.0/docs/Data-Text.html).

Импорт модулей, отдельных функций выглядит следующим образом:
```haskell
import qualified Data.Text as T -- модуль у нас будет называться просто T
import Data.Text (replace) -- если мы хотим использовать функцию replace из модуля T не как T.replace, а просто replace 
```
Кстати, в модуле Data.Text есть практически все функции высших порядков, которые мы использовали, но для юникодных строк.

### Ещё
Всю остальную информацию можно узнать из шаблона программы, который собирает список преподавателей кафедры 22 и выводит их на экран.

### Задание
|Задание|Студент|
|---|---|
|По списку языков программирования википедии (http://en.wikipedia.org/wiki/List_of_programming_languages)  составить список императивных, не функциональных ЯП.|Знаменок Ксения|
|По списку языков программирования википедии (http://en.wikipedia.org/wiki/List_of_programming_languages) составить список кортежей: год  появления, названия. Языки без указания годов появления исключить.|Сергей Горюшкин, Кузнецов Константин|
|Составить отсортированный список преподавателей кафедры 22 и их хобби||
|Составить список 50 худших преподавателей (http://www.mephist.ru/mephist/prepods.nsf/teachers) Тут придётся использовать http://hackage.haskell.org/package/iconv-0.4||
|Составить отсортированный список преподавателей (http://www.mephist.ru/mephist/prepods.nsf/teachers) по количеству изречённых перлов. Тут придётся использовать http://hackage.haskell.org/package/iconv-0.4|Анатолий Мальцев|
|Составить список преподавателей кафедры 22 и дисциплин, которые они сейчас ведут (Список брать запросом http://eis.mephi.ru/TimeTable/timetableshow.aspx?gr=&prep=%D0%A0%D1%8B%D0%B1%D0%B8%D0%BD%D0%B0&typ=prep)|Баладурин Константин|

### Заключение
Вопросы можно-нужно оставлять как issue для этого репозитория.
Решённые задания оставляются как pull request'ы поддиректории с вашей программой.