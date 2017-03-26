# 1. Машина фон Неймана
## Язык псевдоассемблера
### Общие правила
* Строки-комментарии начинаются с символа '#'.
* Строковые литералы должны заключаться в двойную пару кавычек разного типа: '"hello"' или "'hello'".
* Все команды и логические операторы пишутся большими буквами.
* Названия функций и меток желательно начинать с точки и составлять из маленьких букв.
* Использование отступов желательно, но не обязательно.

### Команды



### Переменные
* Локальные переменные объявляются в начале функции.
* Глобальные переменные объявляются в начале программы.

### Команды
* После команд ... необходимо указывать, в какую переменную надо сохранить результат; для этого используется оператор ->.

### Логические операторы

### Функции и метки
* Каждая программа должна содержать функцию .main.



Инструкция -- 6 байт.
Пример: 12 34 56 78 9A BC
12 -- номер команды
34 -- флаг команды
если флаг равен 0, значит берется значение
1 -- берется адрес
2 -- берется адрес, лежащий по адресу
56 78 9A BC -- значение (4 байта ~ int) или адрес

Инструкции:
mov addr1 addr2

Устройство образа программы:
1. ip
2. sp
3. arithmetic result
4. glob_1
...
n+3. glob_n

n+4. instr_1
...
n+m+3. instr_m

n+m+4. stack_1
...
n+m+k+3. stack_k

GLOB name
VAR name
FUN name arg1 ... argN
LABEL name
IF operator varName labelNameTrue [labelNameFalse]

ADD varName1 varName2 -> resultVarName
ADD varName1 number -> resultVarName
SUB varName1 varName2 -> resultVarName
SUB varName1 number -> resultVarName
MUL varName1 varName2 -> resultVarName
MUL varName1 number -> resultVarName
DIV varName1 varName2 -> resultVarName
DIV varName1 number -> resultVarName
MOD varName1 varName2 -> resultVarName
MOD varName1 number -> resultVarName

READ -> varName
PRINT '"stringContents"'
PRINT name
