<h1>Suffix tree library</h1>

<h2>�����</h2>

<p>������ ���� ������������, ����(��), ����, 393 ������</p>

<h2>��� �����������?</h2>

<p>� ������������ ����� <code>suffix_tree.h</code> ���������� ����� <code>SuffixTree</code>
c ��������� ������������� <code>SuffixTree(const std::string& string, std::string last_symbol='$')</code> � ��������� ������� <code>void TreeTraversal(T* visitor) const</code>.

<h3>���������� ������</h3>

<h4>��� ���?</h4>

<p>��� ������������ � ���������� ������� ������������� �������� <a href="http://goo.gl/UaAdbx">������ �� ���������</a>.</p>

<h4>������</h4>

<p>� ������ ���������� ���������� ������ �������� �� ������ <code>string + last_symbol</code>. <code>last_symbol</code> ������ ���������� �� 
���� �������� ������ <code>string</code>. ����� ����� ������ ��� ���������� ���������� � �������������� ����������� ������.</p>

<h4>������� ��������</h4>

<p>��� �������� ����� ��������� ������� ��������. ��� �������� ��������� �����. �� �� ��� �������� �� ��������
����� � ������. ���������� ������ ����� ���� � ��������. ���������� ������ �������� ���� � <code>NULL_VERTEX</code>.
(��������� <code>NULL_VERTEX == -1</code> ���������� �������������� �������)</p>

<h4>����������</h4>

<p>��� ���������� ����������� ������ ������������ <a href="http://habrahabr.ru/post/111675/">�������� ��������</a>.</p>

<h4>�������</h4>

<p>��� ��������� ���������� ������ �������������� �������� �������, ������� ������� �� ��������� �������� ������,
�� ������� �������� ���������� ������. ��������� ����� ���������� ���� ������� �������� �� ��������.</p>

<h4>����� ���������� ������</h4>

<p>����� ������ ����������� ��� <code>O(N * K)</code>, ��� <code>N</code> - ������ ������, <code>K</code> - �������� ��������.</p>

<h4>�������������� ������</h4>

<p>������ �������������� ������ ����������� ��� <code>O(N * K)</code>.</p>

<h3>����� <code>void TreeTraversal(T* visitor) const</code></h2>

<h4>�����?</h4>

<p>���� ����� �������� ������������ ��� ������ ����������� ������. ��� ����� ������������ ��� ���������� ����� ���������� ���:</p>

<ol class="task-list">
<li>���� ������, ����� ������������ ������������ �� O(n).</li>
<li>���� ������, ����� ���������� ��������� �������� � ��� �� O(n).</li>
<li>���� ������, ��������� �� ��� ���������� ������ �� O(n).</li>
<li>���� n ����� ��������� ����� S. ��� ������� k �� 2 �� n ����� ������������ ����� ������, ���������� ���������� ��� ������� ��� k ����� �� ������.</li>
</ol>

<h4>��� ������������?</h4>

<p>��� ������� visitor ������ ���� ����������� ������ <code>SuffixTreeVisitor</code> (������� ����� ��������� � <code>suffix_tree.h</code>).</p>

<p><code>SuffixTreeVisitor</code> �������� ����:</p>
<ol class="task-list">
<li><code>const std::string* suffix_tree_string_</code> - ��������� �� ������, �� ������� ���������� ���������� ������.</li>
<li><code>const std::vector<int>* distance_from_root_</code> - ��������� �� ������ ���������� �� ����� �� ������.</li>
<li><code>const std::vector<int>* parent_</code> - ��������� �� ������ ��������� ������.</li>
<li><code>int root_</code> - ������ �����</li>
<li><code>int dummy_</code> - ������ ��������</li>
<li><code>int number_of_vertices</code> - ���������� ������ � ���������� ������</li>
</ol>

<p>��� ������������� ������ ���������������� ������:</p>

<pre><code>
//���������� �������, ����� ������� ������
void InitVisitor() {}

//���������� ����� ���������� ������(���������) ������� vertex;
//vertex - ������ ������� �� ���������� ��������� SuffixTree;
//������ ������� �������� ���������� ��������������� ������ ���� int;
void BeforeVertexProcessing(int vertex) {}

//���������� � �������� ������ TreeTraversal;
//��������� suffix_tree_string_, ������������ � ������� begin_substring_index(������������) 
//� ��������������� �������� end_substring_index "�����" �� ����� �� vertex � incidence_vertex
//(�������� ��������� �� vertex � incidence_vertex);
//����� ��������� bool* do_transition, ���� TreeTraversal ������ ������� �� �����(������,
// ��������), �� � ������ ProcessLink do_transtition
// ������ ����� ������ true, ����� false.(�����������)
void ProcessLink(int vertex, int incidence_vertex,
                 int begin_substring_index, int end_substring_index,
                 bool* do_transition) {}
  
//���������� ����� ��������� ���� ����(������, ���������) �� vertex;
//����� do_transition ����������;
//�� ���� � ��� ������������, ����� �� ������ �� ����. ������
void ProcessSuffixLink(int vertex, int incidence_vertex,
                         bool* do_transition) {}

//���������� ����� ��������� ������� vertex
void AfterVertexProcessing(int vertex) {}
</code> </pre>

<h5>������ ���������</h5>

<ol class="task-list">
<li>������ ���������� ������� �������� ������.</li>
<li>����� <code>ProcessSuffixLink</code> � ������, ���� <code>vertex</code> �������� ������, ��������� �������� NULL_VERTEX
� �������� <code>incidence_vertex</code>.</li>
<li>���������� �� ������� �������� �� ����� ������ -1.</li>
<li>����� ���� ������������ � ������������������ �������</li>
<li>���� ������� ���������� ��������, �� ��� �����(������, ��������) ������������� ��� ���.</li>
<li>���� ���������� <code>do_transition</code> �������� ��� true, �� ����� ����� ��������� � <code>incidence_vertex</code>. ��� ����
<code>vertex</code> �������� � ����� ������.</li>
<li>������� ��������� ��������� <code>visitor</code>, ������� �� ������ ������� �� ���������� ������</li>
</ol>

<h3>������ �������������</h3>

<p>� �������� ������� ������������� ���������� ���������� ����� ���� ��������� ��������� ������ <code>pattern</code>
� �������� ������ <code>string</code> �� <code>O(|pattern|)</code>. �������� � ����� <code>find_all_occurrences.h</code></p>

<h2>�����</h2>

<p>� �������� ������: <code>test_suffix_tree.cpp</code> � <code>test_find_all_occurences.cpp</code> ����������� ����� 
� ������ <code>SuffixTree</code> � ������� <code>FindAllOccurences()</code>.</p>

<p>��� ���������� ������� ���������� ���������� �������������� ����������� ����� 
<code>test_suffix_tree.h</code> � <code>test_find_all_occurences.h</code>
� ������� ������� <code>void TestSuffixTreeUnitTests(std::ostream& out)</code>, 
<code>void TestFindAllOccurences(std::ostream& out)</code>. 
<code>std::ostream& out</code> - ����� ������ ��������� � �����������
�������������</p>