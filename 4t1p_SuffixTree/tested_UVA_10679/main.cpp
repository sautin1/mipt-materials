#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <set>
#include <limits>

class SuffixTree
{
private:
	const int INFINITY = std::numeric_limits<int>::max();
	struct Link {
		int target_node_index;
		int sample_start_index, sample_end_index;

		Link();
		Link(int _target_node_index, int _sample_start_index, int _sample_end_index);
	};

	struct Node {
		std::vector<Link> links;
		int suffix_link_node_index;
		Node(int alphabet_size);
	};

	struct NodeReference {
		int closest_ancestor;
		int sample_start_index;
		int sample_end_index;
		NodeReference(int _ancestor_node, int _sample_start_index, int _sample_end_index);
	};

	struct TestAndSplitResult {
		bool reached_endpoint;
		int node_index; // if reached_endpoint is true, then node_index is end_point
		TestAndSplitResult(bool _is_split, int _node_index);
	};

	std::vector<Node> nodes_;
	int dummy_, root_;
	std::string sample_string_;
	std::unordered_map<char, int> alphabet_;

	int CreateNode();
	int GetLetterCode(char letter) const;
	void LinkNodes(int source_node_index, int target_node_index, int sample_start_index, int sample_end_index);
	bool HasLink(int node_index, char letter) const;
	Link GetLink(int node_index, char letter) const;

	void InitDummy();
	void InitAlphabet();
	void InitTree();

	void CanonicalizeNodeReference(NodeReference* node_reference) const;
	TestAndSplitResult TestAndSplit(const NodeReference& node_reference);

	NodeReference AddNewLetter(NodeReference active_point);

	void BuildTree();
public:
	SuffixTree(const std::string& sample);
	bool IsSubstring(const std::string& substring) const;
	//DepthFirstSearch() const;
};

//std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string);

SuffixTree::Link::Link()
	: target_node_index (-1) {}

SuffixTree::Link::Link(int _target_node_index, int _sample_start_index, int _sample_end_index)
	: target_node_index(_target_node_index), sample_start_index(_sample_start_index), sample_end_index(_sample_end_index) {}

SuffixTree::Node::Node(int alphabet_size)
	: links(alphabet_size, Link()) {}

SuffixTree::NodeReference::NodeReference(int _closest_ancestor_node, int _sample_start_index, int _sample_end_index)
	: closest_ancestor(_closest_ancestor_node), sample_start_index(_sample_start_index), sample_end_index(_sample_end_index) {}

SuffixTree::TestAndSplitResult::TestAndSplitResult(bool _is_split, int _node_index)
	: reached_endpoint(_is_split), node_index(_node_index) {}

SuffixTree::SuffixTree(const std::string& sample)
	: sample_string_(sample) {
	InitAlphabet();
	BuildTree();
}

int SuffixTree::GetLetterCode(char letter) const {
	int result;
	try {
		result = alphabet_.at(letter);
	} catch (std::out_of_range exception) {
		result = -1;
	}
	return result;
}

int SuffixTree::CreateNode() {
	int node_index = nodes_.size();
	nodes_.push_back(Node(alphabet_.size()));
	return node_index;
}

void SuffixTree::LinkNodes(int source_node_index, int target_node_index, int sample_start_index, int sample_end_index) {
	int letter_code = GetLetterCode(sample_string_[sample_start_index]);
	nodes_[source_node_index].links[letter_code] = Link(target_node_index, sample_start_index, sample_end_index);
}

void SuffixTree::InitAlphabet() {
	std::set<char> letter_set;
	for (size_t letter_index = 0; letter_index < sample_string_.size(); ++letter_index) {
		letter_set.insert(sample_string_[letter_index]);
	}
	int letter_counter = 0;
	while (!letter_set.empty()) {
		std::set<char>::iterator it = letter_set.begin();
		alphabet_.insert(std::make_pair(*it, letter_counter++));
		letter_set.erase(it);
	}
}


SuffixTree::Link SuffixTree::GetLink(int node_index, char letter) const {
	int letter_code = GetLetterCode(letter);
	return nodes_[node_index].links[letter_code];
}

bool SuffixTree::HasLink(int node_index, char letter) const {
	return (GetLink(node_index, letter).target_node_index != -1);
}

void SuffixTree::InitDummy() {
	for (size_t letter_index = 0; letter_index < sample_string_.size(); ++letter_index) {
		if (!HasLink(dummy_, sample_string_[letter_index])) {
			LinkNodes(dummy_, root_, letter_index, letter_index + 1);
		}
	}
}

void SuffixTree::InitTree() {
	dummy_ = CreateNode();
	root_ = CreateNode();
	InitDummy();
	nodes_[root_].suffix_link_node_index = dummy_;
}

// if v is explicit and ==0 then returns v. Otherwise, returns closest ancestor of v
void SuffixTree::CanonicalizeNodeReference(NodeReference* node_reference) const {
	Link link_from_ancestor = GetLink(node_reference->closest_ancestor, sample_string_[node_reference->sample_start_index]);
	while (node_reference->sample_end_index - node_reference->sample_start_index >=
		   link_from_ancestor.sample_end_index - link_from_ancestor.sample_start_index) {
		node_reference->closest_ancestor = link_from_ancestor.target_node_index;
		node_reference->sample_start_index += link_from_ancestor.sample_end_index - link_from_ancestor.sample_start_index;
		if (node_reference->sample_start_index < node_reference->sample_end_index) {
			link_from_ancestor = GetLink(node_reference->closest_ancestor, sample_string_[node_reference->sample_start_index]);
		}
	}
}

SuffixTree::TestAndSplitResult SuffixTree::TestAndSplit(const NodeReference& node_reference) {
	if (node_reference.sample_end_index == node_reference.sample_start_index) {
		// explicit node
		return TestAndSplitResult(HasLink(node_reference.closest_ancestor, sample_string_[node_reference.sample_end_index]), node_reference.closest_ancestor);
	} else {
		// implicit node
		Link link_from_node = GetLink(node_reference.closest_ancestor, sample_string_[node_reference.sample_start_index]);
		int link_letter_index = link_from_node.sample_start_index + node_reference.sample_end_index - node_reference.sample_start_index;
		if (GetLetterCode(sample_string_[node_reference.sample_end_index]) == GetLetterCode(sample_string_[link_letter_index])) {
			return TestAndSplitResult(true, node_reference.closest_ancestor);
		}

		int split_node = CreateNode();
		LinkNodes(node_reference.closest_ancestor, split_node, link_from_node.sample_start_index, link_letter_index);
		LinkNodes(split_node, link_from_node.target_node_index, link_letter_index, link_from_node.sample_end_index);
		return TestAndSplitResult(false, split_node);
	}
}

// runs through the nodes between active_point till end_point and adds new link. Returns end_point.
SuffixTree::NodeReference SuffixTree::AddNewLetter(NodeReference active_point) {
	int previous_node = root_;
	TestAndSplitResult test_and_split_result = TestAndSplit(active_point);
	while (!test_and_split_result.reached_endpoint) {
		// create infinite branch
		LinkNodes(test_and_split_result.node_index, CreateNode(), active_point.sample_end_index, INFINITY);

		// create suffix link for previous node
		if (previous_node != root_) {
			nodes_[previous_node].suffix_link_node_index = test_and_split_result.node_index;
		}
		previous_node = test_and_split_result.node_index;

		// go by suffix_link to next vertex
		active_point.closest_ancestor = nodes_[active_point.closest_ancestor].suffix_link_node_index;
		CanonicalizeNodeReference(&active_point);

		test_and_split_result = TestAndSplit(active_point);
	}
	if (previous_node != root_) {
		nodes_[previous_node].suffix_link_node_index = test_and_split_result.node_index;
	}
	return active_point;
}

void SuffixTree::BuildTree() {
	InitTree();
	NodeReference active_point(root_, 0, 0);
	for (size_t letter_index = 0; letter_index < sample_string_.size(); ++letter_index) {
		active_point = AddNewLetter(active_point);
		++active_point.sample_end_index;
		CanonicalizeNodeReference(&active_point);
	}
}

bool SuffixTree::IsSubstring(const std::string& substring) const {
	int current_node = root_;
	int sample_start_index = 0;
	int sample_end_index = 0;
	Link current_link;
	for (size_t letter_index = 0; letter_index < substring.size(); ++letter_index) {
		if (GetLetterCode(substring[letter_index]) == -1) {
			return false;
		}
		if (sample_end_index == sample_start_index) {
			// in explicit node
			current_link = GetLink(current_node, substring[letter_index]);
			if (current_link.target_node_index == -1) {
				return false;
			}
			sample_start_index = current_link.sample_start_index;
			sample_end_index = sample_start_index + 1;
		} else {
			if (sample_string_[sample_end_index] != substring[letter_index]) {
				return false;
			}
			++sample_end_index;
		}
		if (sample_end_index == current_link.sample_end_index) {
			current_node = current_link.target_node_index;
			sample_start_index = 0;
			sample_end_index = 0;
		}
	}
	return true;
}


/*std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string) {

}*/


int main()
{
	int test_quantity;
	std::cin >> test_quantity;
	for (int test_number = 0; test_number < test_quantity; ++test_number) {
		std::string sample;
		std::cin >> sample;
		SuffixTree tree(sample);
		int query_quantity;
		std::cin >> query_quantity;
		for (int query_number = 0; query_number < query_quantity; ++query_number) {
			std::string substring;
			std::cin >> substring;
			if (tree.IsSubstring(substring)) {
				std::cout << "y\n";
			} else {
				std::cout << "n\n";
			}
		}
	}
}

