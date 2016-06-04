#pragma once
#define NOMINMAX

#include <algorithm>
#include <fstream>
#include <limits>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <Windows.h>

#include "DebugUtils.h"

struct Point {
	int row, col;
	Point() = default;
	Point(int _row, int _col) : row(_row), col(_col) {}
	bool operator==(const Point other) const { return row == other.row && col == other.col; }
	bool operator<(const Point other) const { return row < other.row || (row == other.row && col < other.col); }
};

enum EdgeState {
	EDGE_STATE_OFF,
	EDGE_STATE_ON,
	EDGE_STATE_ERR
};

struct Edge {
	Point from;
	Point to;
	Edge() = default;
	Edge(Point _from, Point _to) : from(_from), to(_to) {}
	Edge ordered() const { return from < to ? *this : Edge(to, from); }
	bool operator==(const Edge other) const { return from == other.from && to == other.to; }
};

namespace std {
	template <>
	struct hash<Point>
	{
		std::size_t operator()(const Point& k) const
		{
			using std::hash;
			return ((hash<int>()(k.row) ^ (hash<int>()(k.col) << 1)) >> 1);
		}
	};

	template <>
	struct hash<Edge>
	{
		std::size_t operator()(const Edge& k) const
		{
			using std::hash;
			return hash<Point>()(k.from) ^ hash<Point>()(k.to);
		}
	};
}

using EdgeSet = std::unordered_set<Edge>;
using PointGraph = std::unordered_map<Point, EdgeSet>;
using Matrix = std::vector<std::vector<int>>;

class CSlitherlinkGame
{
public:
	CSlitherlinkGame();
	CSlitherlinkGame(const Matrix& matr, const std::unordered_map<Edge, EdgeState>& states);
	CSlitherlinkGame(const CSlitherlinkGame& game);
	~CSlitherlinkGame();

	static const int INF;
	static CSlitherlinkGame StartNewGame(const std::wstring& filename);

	std::vector<Edge> GetEdgesByState(EdgeState state) const;
	const Matrix& GetNumbers() const;
	int GetRowCount() const;
	int GetColCount() const;
	bool IsShown(int row, int col) const;
	bool IsSatisfiedCell(int row, int col) const;

	bool ToggleEdgeStatus(Edge edge);
	void EraseBorders();

	bool SaveToFile(const std::wstring& filename) const;
private:
	int rowCount;
	int colCount;
	Matrix numbersInitial;
	Matrix numbersBorderCount;
	int numbersCountUnsatisfied;
	int edgesCountWrong;
	PointGraph graph;
	std::unordered_map<Edge, EdgeState> edgeStates;

	static void loadGameMatrix(const std::wstring& filename, Matrix* matr, std::unordered_map<Edge, EdgeState>* edgeStates);

	void initMembers();
	void initUnsatisfiedNumbersCount();
	void initNumberBorderCount();
	void initEdgeStates();
	void initGraph();
	void initEdgesCountWrong();

	bool checkGameEnd() const;
	bool checkIntersection(Edge edge) const;

	int getNeighborsCountByState(Point p, EdgeState state) const;
	Point getCyclePoint() const;

	bool dfs(const Point& p, const Point& prev, const Point& pointStart, std::unordered_set<Point>& visited,
		bool isExitOnCycle, bool isExitOnIntersection) const;
	
	void updateNeighborStates(Point point);
	void updateEdgeState(EdgeState stateOld, std::unordered_map<Edge, EdgeState>::iterator it);
	void updateEdgeState(EdgeState stateOld, Edge edge);
	void updateUnsatisfiedNumbersCount(int borderCountOld, int row, int col);
	void updateNumbersBorderCount(Edge edge, EdgeState newState);

	bool isValidEdge(Edge edge) const;
};