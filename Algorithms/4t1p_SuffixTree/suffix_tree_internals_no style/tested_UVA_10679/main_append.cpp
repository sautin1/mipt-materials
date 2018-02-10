#include <string>
#include <vector>
#include <unordered_map>
#include <set>
#include <limits>
#include <stdexcept>

#include <iostream>

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
		std::unordered_map<char, Link> links;
		int suffix_link_node_index;
		Node();
	};

	struct NodeReference {
		int closest_ancestor;
		int sample_start_index;
		int sample_end_index;
		NodeReference();
		NodeReference(int _ancestor_node, int _sample_start_index, int _sample_end_index);
	};

	struct TestAndSplitResult {
		bool reached_endpoint;
		int node_index; // if reached_endpoint is true, then node_index is end_point
		TestAndSplitResult(bool _is_split, int _node_index);
	};

	std::vector<Node> nodes_;
	std::set<char> letter_set_;
	int dummy_, root_;
	std::string sample_string_;
	NodeReference active_point_;

	int CreateNode();
	void InitDummy(int sample_start_index, int sample_end_index);
	void InitLetterSet(int start_index, int sample_end_index);
	void InitTree();

	void LinkNodes(int source_node_index, int target_node_index, int sample_start_index, int sample_end_index);
	Link GetLink(int node_index, char letter) const;
	bool HasLink(int node_index, char letter) const;

	void CanonicalizeNodeReference(NodeReference* node_reference) const;
	TestAndSplitResult TestAndSplit(const NodeReference& node_reference);

	NodeReference AddNextLetter(NodeReference active_point);
	void BuildTree();
public:
	SuffixTree(const std::string& sample);
	void AppendSample(const std::string& append_sample);
	void PrintTree(std::ostream& fout) const;
	bool IsSubstring(const std::string& substring) const;
	//DepthFirstSearch() const;
};

//std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string);


SuffixTree::Link::Link()
	: target_node_index (-1) {}

SuffixTree::Link::Link(int _target_node_index, int _sample_start_index, int _sample_end_index)
	: target_node_index(_target_node_index), sample_start_index(_sample_start_index), sample_end_index(_sample_end_index) {}

SuffixTree::Node::Node()
	: links(), suffix_link_node_index(-1) {}

SuffixTree::NodeReference::NodeReference() {}

SuffixTree::NodeReference::NodeReference(int _closest_ancestor_node, int _sample_start_index, int _sample_end_index)
	: closest_ancestor(_closest_ancestor_node), sample_start_index(_sample_start_index), sample_end_index(_sample_end_index) {}

SuffixTree::TestAndSplitResult::TestAndSplitResult(bool _is_split, int _node_index)
	: reached_endpoint(_is_split), node_index(_node_index) {}

SuffixTree::SuffixTree(const std::string& sample)
	: sample_string_(sample) {
	InitLetterSet(0, sample_string_.size());
	BuildTree();
}

int SuffixTree::CreateNode() {
	int node_index = nodes_.size();
	nodes_.push_back(Node());
	return node_index;
}

void SuffixTree::InitDummy(int sample_start_index, int sample_end_index) {
	for (int letter_index = sample_start_index; letter_index < sample_end_index; ++letter_index) {
		if (!HasLink(dummy_, sample_string_[letter_index])) {
			LinkNodes(dummy_, root_, letter_index, letter_index + 1);
		}
	}
}

void SuffixTree::InitLetterSet(int sample_start_index, int sample_end_index) {
	for (int letter_index = sample_start_index; letter_index < sample_end_index; ++letter_index) {
		letter_set_.insert(sample_string_[letter_index]);
	}
}

void SuffixTree::InitTree() {
	dummy_ = CreateNode();
	root_ = CreateNode();
	active_point_ = NodeReference(root_, 0, 0);
	InitDummy(0, sample_string_.size());
	nodes_[root_].suffix_link_node_index = dummy_;
}

void SuffixTree::LinkNodes(int source_node_index, int target_node_index, int sample_start_index, int sample_end_index) {
	char letter = sample_string_[sample_start_index];
	nodes_[source_node_index].links[letter] = Link(target_node_index, sample_start_index, sample_end_index);
}

SuffixTree::Link SuffixTree::GetLink(int node_index, char letter) const {
	Link link = nodes_[node_index].links.at(letter);
	return link;
}

bool SuffixTree::HasLink(int node_index, char letter) const {
	bool has_link = true;
	try {
		GetLink(node_index, letter);
	} catch (const std::out_of_range& exception) {
		has_link = false;
	}
	return has_link;
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
		if (sample_string_[node_reference.sample_end_index] == sample_string_[link_letter_index]) {
			return TestAndSplitResult(true, node_reference.closest_ancestor);
		}

		int split_node = CreateNode();
		LinkNodes(node_reference.closest_ancestor, split_node, link_from_node.sample_start_index, link_letter_index);
		LinkNodes(split_node, link_from_node.target_node_index, link_letter_index, link_from_node.sample_end_index);
		return TestAndSplitResult(false, split_node);
	}
}

// runs through the nodes between active_point till end_point and adds new link. Returns end_point.
SuffixTree::NodeReference SuffixTree::AddNextLetter(NodeReference active_point) {
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
	for (size_t letter_index = 0; letter_index < sample_string_.size(); ++letter_index) {
		active_point_ = AddNextLetter(active_point_);
		++active_point_.sample_end_index;
		CanonicalizeNodeReference(&active_point_);
	}
}

void SuffixTree::AppendSample(const std::string& append_sample) {
	size_t old_sample_size = sample_string_.size();
	sample_string_.append(append_sample);
	InitDummy(old_sample_size, sample_string_.size());
	InitLetterSet(old_sample_size, sample_string_.size());
	for (size_t letter_index = old_sample_size; letter_index < sample_string_.size(); ++letter_index) {
		active_point_ = AddNextLetter(active_point_);
		++active_point_.sample_end_index;
		CanonicalizeNodeReference(&active_point_);
	}
}

void SuffixTree::PrintTree(std::ostream& fout) const {
	for (size_t node_index = 0; node_index < nodes_.size(); ++node_index) {
		fout << "Node " << node_index << "\n";
		for (auto it = nodes_[node_index].links.begin(); it != nodes_[node_index].links.end(); ++it) {
			Link link = it->second;
			fout << "\t" << it->first << ": " << link.target_node_index << ". sample[ " << link.sample_start_index << " - ";
			if (link.sample_end_index != INFINITY) {
				fout << link.sample_end_index << " ].\n";
			} else {
				fout << "INF ].\n";
			}
		}
		fout << "\tsufflink: " << nodes_[node_index].suffix_link_node_index << "\n";
	}
	fout << "__________\n";
}

bool SuffixTree::IsSubstring(const std::string& substring) const {
	int current_node = root_;
	int sample_start_index = 0;
	int sample_end_index = 0;
	Link current_link;
	for (size_t letter_index = 0; letter_index < substring.size(); ++letter_index) {
		if (sample_end_index == sample_start_index) {
			// in explicit node
			if (!HasLink(current_node, substring[letter_index])) {
				return false;
			}
			current_link = GetLink(current_node, substring[letter_index]);
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
	/*SuffixTree tree("abcdefghijklmnop");
	std::cout << tree.IsSubstring("ghij") << "\n";
	std::cout << tree.IsSubstring("opq") << "\n";
	tree.AppendSample("qrstuvwxyz");
	std::cout << tree.IsSubstring("opq") << "\n";
	std::cout << tree.IsSubstring("vwx") << "\n";*/
	int test_quantity;
	std::cin >> test_quantity;
	for (int test_number = 0; test_number < test_quantity; ++test_number) {
		std::string sample;
		std::cin >> sample;
		SuffixTree tree(std::string(1, sample[0]));
		for (size_t letter_index = 1; letter_index < sample.size(); ++letter_index) {
			tree.AppendSample(std::string(1, sample[letter_index]));
		}
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

