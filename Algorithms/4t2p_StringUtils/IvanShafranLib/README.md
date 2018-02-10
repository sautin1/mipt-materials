<h1>Suffix tree library</h1>

<h2>Автор</h2>

<p>Шафран Иван Мирославович, МФТИ(ГУ), ФИВТ, 393 группа</p>

<h2>Что реализовано?</h2>

<p>В заголовочном файле <code>suffix_tree.h</code> реализован класс <code>SuffixTree</code>
c публичным конструктором <code>SuffixTree(const std::string& string, std::string last_symbol='$')</code> и публичным методом <code>void TreeTraversal(T* visitor) const</code>.

<h3>Суффиксное дерево</h3>

<h4>Что это?</h4>

<p>Для ознакомления с суффиксным деревом рекомендуется прочесть <a href="http://goo.gl/UaAdbx">статью на википедии</a>.</p>

<h4>Строка</h4>

<p>В данной реализации суффиксное дерево строится от строки <code>string + last_symbol</code>. <code>last_symbol</code> должен отличаться от 
всех символов строки <code>string</code>. Такой прием удобен для реализации алгоритмов с использованием суффиксного дерева.</p>

<h4>Вершина пустышка</h4>

<p>Для удобства также добавлена вершина пустышка. Она является родителем корня. Из неё все переходы по алфавиту
ведут в корень. Суффиксная ссылка корня ведёт в пустышку. Суффиксная ссылка пустышки ведёт в <code>NULL_VERTEX</code>.
(константа <code>NULL_VERTEX == -1</code> обозначает несуществующую вершину)</p>

<h4>Реализация</h4>

<p>Для построения суффиксного дерева используется <a href="http://habrahabr.ru/post/111675/">алгоритм Укконена</a>.</p>

<h4>Алфавит</h4>

<p>Для ускорения построения дерева предварительно строится алфавит, который состоит из различных символов строки,
по которой строится суффиксное дерево. Благодаря этому достаточно лишь хранить переходы по алфавиту.</p>

<h4>Время построения дерева</h4>

<p>Время работы оценивается как <code>O(N * K)</code>, где <code>N</code> - размер строки, <code>K</code> - мощность алфавита.</p>

<h4>Дополнительная память</h4>

<p>Размер дополнительной памяти оценивается как <code>O(N * K)</code>.</p>

<h3>Метод <code>void TreeTraversal(T* visitor) const</code></h2>

<h4>Зачем?</h4>

<p>Этот метод является инструментом для обхода суффиксного дерева. Его можно использлвать для реализации таких алгоритмов как:</p>

<ol class="task-list">
<li>Дана строка, найти максимальный подпалиндром за O(n).</li>
<li>Дана строка, найти количество различных подстрок в ней за O(n).</li>
<li>Дана строка, построить по ней суффиксный массив за O(n).</li>
<li>Дано n строк суммарной длины S. Для каждого k от 2 до n найти максимальную длину строки, являющейся подстрокой как минимум для k строк из набора.</li>
</ol>

<h4>Как использовать?</h4>

<p>Тип объекта visitor должен быть наследником класса <code>SuffixTreeVisitor</code> (который также релизован в <code>suffix_tree.h</code>).</p>

<p><code>SuffixTreeVisitor</code> содержит поля:</p>
<ol class="task-list">
<li><code>const std::string* suffix_tree_string_</code> - указатель на строку, по которой построенно суффиксное дерево.</li>
<li><code>const std::vector<int>* distance_from_root_</code> - указатель на вектор расстояний от корня до вершин.</li>
<li><code>const std::vector<int>* parent_</code> - указатель на вектор родителей вершин.</li>
<li><code>int root_</code> - индекс корня</li>
<li><code>int dummy_</code> - индекс пустышки</li>
<li><code>int number_of_vertices</code> - количество вершин в суффиксном дереве</li>
</ol>

<p>При необходимости должны переопределяться методы:</p>

<pre><code>
//вызывается однажды, перед обходом дерева
void InitVisitor() {}

//вызывается перед обработкой ссылок(переходов) вершины vertex;
//vertex - индекс вершины во внутренней структуре SuffixTree;
//индекс вершины является уникальным неотрицательным числом типа int;
void BeforeVertexProcessing(int vertex) {}

//вызывается в процессе метода TreeTraversal;
//подстрока suffix_tree_string_, начинающаяся с индекса begin_substring_index(включительно) 
//и заканчивающаяся индексом end_substring_index "лежит" на ребре из vertex в incidence_vertex
//(является переходом из vertex в incidence_vertex);
//также принимает bool* do_transition, если TreeTraversal должен перейти по ребру(ссылке,
// переходу), то в методе ProcessLink do_transtition
// должен стать равным true, иначе false.(обязательно)
void ProcessLink(int vertex, int incidence_vertex,
                 int begin_substring_index, int end_substring_index,
                 bool* do_transition) {}
  
//вызывается после обработки всех рёбер(ссылок, переходов) из vertex;
//смысл do_transition аналогичен;
//по сути у Вас спрашивается, нужно ли пройти по суфф. ссылке
void ProcessSuffixLink(int vertex, int incidence_vertex,
                         bool* do_transition) {}

//вызывается после обработки вершины vertex
void AfterVertexProcessing(int vertex) {}
</code> </pre>

<h5>Важные замечания</h5>

<ol class="task-list">
<li>Первая посещенная вершина является корнем.</li>
<li>Метод <code>ProcessSuffixLink</code> в случае, если <code>vertex</code> является листом, возвращет значение NULL_VERTEX
в качестве <code>incidence_vertex</code>.</li>
<li>Расстояние до вершины пустышки от корня равное -1.</li>
<li>Обход ребёр производится в лексикографическом порядке</li>
<li>Если вершина посещается повторно, то все ребра(ссылки, переходы) просматривася ещё раз.</li>
<li>Если переменная <code>do_transition</code> помечена как true, то обход сразу переходит в <code>incidence_vertex</code>. При этом
<code>vertex</code> остается в стеке обхода.</li>
<li>Обходом полностью управляет <code>visitor</code>, поэтому он должен следить за остановкой обхода</li>
</ol>

<h3>Пример использования</h3>

<p>В качестве примера использования библиотеки реализован поиск всех вхождений некоторой строки <code>pattern</code>
в исходную строку <code>string</code> за <code>O(|pattern|)</code>. Смотреть в файле <code>find_all_occurrences.h</code></p>

<h2>Тесты</h2>

<p>В исходных файлах: <code>test_suffix_tree.cpp</code> и <code>test_find_all_occurences.cpp</code> реализованы тесты 
к классу <code>SuffixTree</code> и функции <code>FindAllOccurences()</code>.</p>

<p>Для отдельного запуска необходимо подключить соответсвующие заголовчные файлы 
<code>test_suffix_tree.h</code> и <code>test_find_all_occurences.h</code>
и вызвать функции <code>void TestSuffixTreeUnitTests(std::ostream& out)</code>, 
<code>void TestFindAllOccurences(std::ostream& out)</code>. 
<code>std::ostream& out</code> - поток вывода сообщений о результатах
тестированиия</p>