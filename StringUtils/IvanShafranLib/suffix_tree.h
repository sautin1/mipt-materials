#pragma once

#include <string>
#include <algorithm>
#include <vector>
#include <limits>
#include <stack>
#include <map>


namespace {

const int NULL_VERTEX = -1;


class SuffixTreeVisitor {
 public:
  const std::string* suffix_tree_string_;
  const std::vector<int>* distance_from_root_;
  const std::vector<int>* parent_;
   
  void set_suffix_tree_string(const std::string* suffix_tree_string) {
    this->suffix_tree_string_ = suffix_tree_string;
  }

  void set_distance_from_root(const std::vector<int>* distance) {
    this->distance_from_root_ = distance;
  }

  void set_parent(const std::vector<int>* parent) { 
    this->parent_ = parent;
  }

  void BeforeVertexProcessing(int vertex) {}

  void ProcessLink(int vertex, int incidence_vertex,
                   int begin_substring_index, int end_substring_index,
                   bool* do_transition) {
    *do_transition = true;
  }

  void ProcessSuffixLink(int vertex, int incidence_vertex, bool* do_transition) {
    *do_transition = false;
  }

  void AfterVertexProcessing(int vertex) {}
};

class SuffixTree {
public:
  SuffixTree(const std::string& string, std::string last_symbol="$") {
    string_ = string;
    string_ += last_symbol;

    BuildAlphabet(string_);

    InitTree();

    BuildTree();
  }

  template <class TreeTraversalVisitor>
  void TreeTraversal(TreeTraversalVisitor* visitor) const {
    visitor->set_suffix_tree_string(&(this->get_string()));
    visitor->set_distance_from_root(&(this->distance_from_root_));
    visitor->set_parent(&(this->parent_));

    size_t number_of_vertices = this->tree_.size();
    int new_layer_index = 0;
    std::stack<int> layer_stack;
    std::vector<int> layer_vertex;
    enum colors { WHITE, GREY, BLACK };
    std::vector<colors> layer_color;
    std::vector<std::vector<int> > link_lists(number_of_vertices);
    std::vector<int> layer_next_link;

    layer_stack.push(new_layer_index);
    layer_vertex.push_back(this->root_);
    layer_color.push_back(colors::WHITE);
    layer_next_link.push_back(0);
    ++new_layer_index;

    while (!layer_stack.empty()) {
      int layer = layer_stack.top();
      int vertex = layer_vertex[layer];

      if (layer_color[layer] == WHITE) {
        visitor->BeforeVertexProcessing(vertex);

        if (link_lists[vertex].empty()) {
          link_lists[vertex] = GetIncidenceList(vertex);
        }

        layer_color[layer] = GREY;
      }

      if (layer_next_link[layer] == link_lists[vertex].size()) {
        layer_stack.pop();
        layer_color[layer] = BLACK;

        visitor->AfterVertexProcessing(vertex);
      }
      else {
        bool do_transition;
        int incidence_vertex;

        if ((layer_next_link[layer] + 1) == link_lists[vertex].size()) {
          //last edge is suffix link
          incidence_vertex = link_lists[vertex][layer_next_link[layer]];
          visitor->ProcessSuffixLink(vertex, incidence_vertex, &do_transition);
        }
        else
        {
          Link link = tree_[vertex].links[link_lists[vertex][layer_next_link[layer]]];
          incidence_vertex = link.incidence_vertex;

          visitor->ProcessLink(vertex,
            link.incidence_vertex,
            link.begin_substring,
            link.end_substring,
            &do_transition);
        }

        if (do_transition) {
          layer_stack.push(new_layer_index);
          layer_vertex.push_back(incidence_vertex);
          layer_color.push_back(colors::WHITE);
          layer_next_link.push_back(0);
          ++new_layer_index;
        }

        ++layer_next_link[layer];
      }
    }
  }

#ifdef SUFFIX_TREE_TESTING
 public:
#else
 private:
#endif
  struct Link {
    int begin_substring, end_substring, incidence_vertex;

    Link() : incidence_vertex(NULL_VERTEX) {}
    Link(int begin, int end, int to) : begin_substring(begin),
      end_substring(end),
      incidence_vertex(to) {}
  };

  struct Vertex {
    std::vector<Link> links;
    int suffix;

    Vertex(int number_of_links) : links(number_of_links, Link()), suffix(NULL_VERTEX) {}
  };

  struct Point {
    int vertex;
    int begin;

    Point() {}
    Point(int _vertex, int _begin) : vertex(_vertex), begin(_begin) {}
  };

  std::vector<Vertex> tree_;
  std::string string_;
  int root_, dummy_;
  int next_new_vertex_;
  Point active_point_;
  std::string alphabet_;
  std::map<int, int> map_of_alphabet_index_;
  std::vector<int> distance_from_root_;
  std::vector<int> parent_;

  const std::string& get_string() const {
    return string_;
  }

  void BuildAlphabet(const std::string& string) {
    for (size_t i = 0; i < string.size(); ++i) {
      if (std::find(alphabet_.begin(), alphabet_.end(), string[i]) == alphabet_.end()) {
        alphabet_ += string[i];
      }
    }

    std::sort(alphabet_.begin(), alphabet_.end());

    for (size_t i = 0; i < alphabet_.size(); ++i) {
      map_of_alphabet_index_[alphabet_[i]] = i;
    }
  }

  int GetSizeOfAlhpabet() const {
    return alphabet_.size();
  }

  int& suffix_link(int v) {
    return tree_[v].suffix;
  }

  int GetEdgeIndex(int symbol_index) {
    return map_of_alphabet_index_[string_[symbol_index]];
  }

  void LinkVertex(int from, int begin_substring, int end_substring, int to) {
    tree_[from].links[GetEdgeIndex(begin_substring)] = Link(begin_substring, end_substring, to);
    distance_from_root_[to] = distance_from_root_[from] + (end_substring - begin_substring);
    parent_[to] = from;
  }

  int NewVertex() {
    int index = this->next_new_vertex_;
    this->next_new_vertex_++;
    return index;
  }

  void InitTree() {
    int max_vertex_number = 2 * this->string_.size() + 2;
    this->tree_.resize(max_vertex_number, Vertex(this->GetSizeOfAlhpabet()));
    this->distance_from_root_.resize(max_vertex_number);
    this->parent_.resize(max_vertex_number, NULL_VERTEX);
    this->next_new_vertex_ = 0;

    dummy_ = NewVertex();
    root_ = NewVertex();
    active_point_ = Point(root_, 0);

    suffix_link(root_) = dummy_;
    for (int i = 0; i < GetSizeOfAlhpabet(); ++i) {
      int edge_begin = string_.find(alphabet_[i]);
      LinkVertex(dummy_, edge_begin, edge_begin + 1, root_);
    }

    distance_from_root_[root_] = 0;
    distance_from_root_[dummy_] = -1;

    parent_[root_] = dummy_;
  }

  Point Canonicalize(int vertex, int begin, int end) {
    if (end <= begin) {
      return Point(vertex, begin);
    }
    else {
      Link link = tree_[vertex].links[GetEdgeIndex(begin)];
      while (end - begin >= link.end_substring - link.begin_substring) {
        begin += link.end_substring - link.begin_substring;
        vertex = link.incidence_vertex;
        if (end > begin) {
          link = tree_[vertex].links[GetEdgeIndex(begin)];
        }
      }
      return Point(vertex, begin);
    }
  }

  bool Split(int vertex, int begin, int end, int link_index,
    int* next_branching_vertex) {
    if (end <= begin) {
      *next_branching_vertex = vertex;
      return tree_[vertex].links[link_index].incidence_vertex == NULL_VERTEX;
    }
    else {
      Link link = tree_[vertex].links[GetEdgeIndex(begin)];
      if (link_index == GetEdgeIndex(link.begin_substring + end - begin)) {
        *next_branching_vertex = vertex;
        return false;
      }

      int middle = NewVertex();
      LinkVertex(vertex, link.begin_substring, link.begin_substring + end - begin, middle);
      LinkVertex(middle, link.begin_substring + end - begin, link.end_substring, 
                 link.incidence_vertex);

      *next_branching_vertex = middle;
      return true;
    }
  }

  Point CreateNewBranches(int vertex, int begin, int end) {
    int next_branching_vertex;
    int previous_branching_vertex = root_;

    while (Split(vertex, begin, end, GetEdgeIndex(end), &next_branching_vertex)) {
      LinkVertex(next_branching_vertex, end, string_.size(), NewVertex());

      if (previous_branching_vertex != root_) {
        suffix_link(previous_branching_vertex) = next_branching_vertex;
      }
      previous_branching_vertex = next_branching_vertex;

      Point newPoint = Canonicalize(suffix_link(vertex), begin, end);
      vertex = newPoint.vertex;
      begin = newPoint.begin;
    }

    if (previous_branching_vertex != this->root_) {
      suffix_link(previous_branching_vertex) = next_branching_vertex;
    }

    return Point(vertex, begin);
  }

  void AddSymbol(int index_of_next_symbol) {
    int end = index_of_next_symbol;

    this->active_point_ = CreateNewBranches(active_point_.vertex, active_point_.begin, end);
    this->active_point_ = Canonicalize(active_point_.vertex, active_point_.begin, end + 1);
  }

  void BuildTree() {
    for (size_t i = 0; i < string_.size(); ++i) {
      AddSymbol(i);
    }
  }

  std::vector<int> GetIncidenceList(int vertex) const {
    std::vector<int> incidence_list;
    for (int index = 0; index < GetSizeOfAlhpabet(); ++index) {
      if (tree_[vertex].links[index].incidence_vertex != NULL_VERTEX) {
        incidence_list.push_back(index);
      }
    }

    incidence_list.push_back(tree_[vertex].suffix);

    return incidence_list;
  }

};

}
