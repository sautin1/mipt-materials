#include "SlitherlinkGame.h"

const int CSlitherlinkGame::INF = std::numeric_limits<int>::max();

CSlitherlinkGame::CSlitherlinkGame()
	: rowCount(5), colCount(5), numbersInitial(rowCount, std::vector<int>(colCount, 3))
{
	numbersBorderCount.resize(numbersInitial.size(), std::vector<int>(numbersInitial.front().size(), 0));
	initMembers();
	edgesCountWrong = 0;
}

CSlitherlinkGame::CSlitherlinkGame(const Matrix& matr, const std::unordered_map<Edge, EdgeState>& states)
	: rowCount(matr.size()), colCount(matr.front().size()), numbersInitial(matr), edgeStates(states)
{
	initGraph();
	numbersBorderCount.resize(numbersInitial.size(), std::vector<int>(numbersInitial.front().size(), 0));
	initNumberBorderCount();
	initUnsatisfiedNumbersCount();
	initEdgesCountWrong();
	initIsCrossed();
}

CSlitherlinkGame::CSlitherlinkGame(const CSlitherlinkGame& game)
	: rowCount(game.GetRowCount()), colCount(game.GetColCount()), numbersInitial(game.numbersInitial), 
	numbersBorderCount(game.numbersBorderCount), edgeStates(game.edgeStates), edgesCountWrong(edgesCountWrong),
	graph(game.graph), numbersCountUnsatisfied(game.numbersCountUnsatisfied), isCrossed(game.isCrossed)
{
}

CSlitherlinkGame::~CSlitherlinkGame()
{
}

void CSlitherlinkGame::initGraph()
{
	for( int row = 0; row < rowCount + 1; ++row ) {
		for( int col = 0; col < colCount + 1; ++col ) {
			if( row < rowCount || col < colCount ) {
				Point pointCur(row, col);
				EdgeSet set = graph[pointCur];
				if( row < rowCount ) {
					Point pointNext(row + 1, col);
					EdgeSet setInverted = graph[pointNext];
					Edge edge = Edge(pointCur, pointNext);
					set.insert(edge);
					setInverted.insert(Edge(pointNext, pointCur));
					graph[pointCur] = set;
					graph[pointNext] = setInverted;
				}
				if( col < colCount ) {
					Point pointNext(row, col + 1);
					EdgeSet setInverted = graph[pointNext];
					Edge edge = Edge(pointCur, pointNext);
					set.insert(edge);
					setInverted.insert(Edge(pointNext, pointCur));
					graph[pointCur] = set;
					graph[pointNext] = setInverted;
				}
			}
		}
	}
}

void CSlitherlinkGame::initEdgeStates()
{
	edgeStates.clear();
	for( auto itSet = graph.begin(); itSet != graph.end(); ++itSet ) {
		for( auto itEdge = itSet->second.begin(); itEdge != itSet->second.end(); ++itEdge ) {
			if( itEdge->from < itEdge->to ) {
				edgeStates[*itEdge] = EDGE_STATE_OFF;
			}
		}
	}
}

void CSlitherlinkGame::initEdgesCountWrong()
{
	edgesCountWrong = 0;
	for( auto itSet = graph.begin(); itSet != graph.end(); ++itSet ) {
		for( auto itEdge = itSet->second.begin(); itEdge != itSet->second.end(); ++itEdge ) {
			if( itEdge->from < itEdge->to && edgeStates[*itEdge] == EDGE_STATE_ERR ) {
				++edgesCountWrong;
			}
		}
	}
}

void CSlitherlinkGame::initIsCrossed()
{
	for( auto itSet = graph.begin(); itSet != graph.end(); ++itSet ) {
		for( auto itEdge = itSet->second.begin(); itEdge != itSet->second.end(); ++itEdge ) {
			if( itEdge->from < itEdge->to ) {
				isCrossed[*itEdge] = false;
			}
		}
	}
}

void CSlitherlinkGame::initNumberBorderCount()
{
	for( auto itSet = graph.begin(); itSet != graph.end(); ++itSet ) {
		for( auto itEdge = itSet->second.begin(); itEdge != itSet->second.end(); ++itEdge ) {
			EdgeState state;
			if( itEdge->from < itEdge->to ) {
				state = edgeStates[*itEdge];
				if( state != EDGE_STATE_OFF ) {
					updateNumbersBorderCount(*itEdge, state);
				}
			}
		}
	}
}

void CSlitherlinkGame::initUnsatisfiedNumbersCount()
{
	numbersCountUnsatisfied = 0;
	for( int row = 0; row < rowCount; ++row ) {
		for( int col = 0; col < colCount; ++col ) {
			if( numbersInitial[row][col] < INF && numbersInitial[row][col] != numbersBorderCount[row][col] ) {
				++numbersCountUnsatisfied;
			}
		}
	}
}

void CSlitherlinkGame::initMembers()
{
	initGraph();
	initEdgeStates();
	initUnsatisfiedNumbersCount();
}

bool CSlitherlinkGame::IsShown(int row, int col) const
{
	return numbersInitial.at(row).at(col) < INF;
}

bool CSlitherlinkGame::IsCrossed(Edge edge) const
{
	return isCrossed.at(edge);
}

bool CSlitherlinkGame::IsSatisfiedCell(int row, int col) const
{
	return !IsShown(row, col) || numbersInitial.at(row).at(col) == numbersBorderCount.at(row).at(col);
}

bool CSlitherlinkGame::isValidEdge(Edge edge) const
{
	int neighborCountFrom = getNeighborsCountByState(edge.from, EDGE_STATE_ON) + getNeighborsCountByState(edge.from, EDGE_STATE_ERR);
	int neighborCountTo = getNeighborsCountByState(edge.to, EDGE_STATE_ON) + getNeighborsCountByState(edge.to, EDGE_STATE_ERR);
	
	return neighborCountFrom <= 2 && neighborCountTo <= 2 && !checkIntersection(edge);
}

bool CSlitherlinkGame::dfs(const Point& p, const Point& prev, const Point& pointStart, std::unordered_set<Point>& visited, 
	bool isExitOnCycle, bool isExitOnIntersection) const
{
	visited.insert(p);
	auto itSet = graph.find(p);
	for( auto itEdge = itSet->second.begin(); itEdge != itSet->second.end(); ++itEdge ) {
		Point neighbor = itEdge->to;
		if( neighbor == prev || edgeStates.at(itEdge->ordered()) != EDGE_STATE_ON ) {
			continue;
		}
		if( visited.find(neighbor) == visited.end() ) {
			return dfs(neighbor, p, pointStart, visited, isExitOnCycle, isExitOnIntersection);
		} else if( isExitOnCycle && neighbor == pointStart ) {
			return true;
		} else if( isExitOnIntersection ) {
			return true;
		}
	}
	return false;
}

Point CSlitherlinkGame::getCyclePoint() const
{
	auto itPoint = graph.begin();
	for( ; itPoint != graph.end(); ++itPoint ) {
		bool isCyclePoint = std::any_of(itPoint->second.begin(), itPoint->second.end(), 
			[this](Edge edge) { return edgeStates.at(edge.ordered()) == EDGE_STATE_ON; });
		if( isCyclePoint ) {
			break;
		}
	}
	return (itPoint != graph.end()) ? itPoint->first : Point(-1, -1);
}

bool CSlitherlinkGame::checkIntersection(Edge edge) const
{
	std::unordered_set<Point> pointsVisited;
	return dfs(edge.from, edge.from, edge.from, pointsVisited, false, true);
}

bool CSlitherlinkGame::checkGameEnd() const
{
	if( edgesCountWrong > 0 || numbersCountUnsatisfied > 0 ) {
		return false;
	}
	std::unordered_set<Point> pointsVisited;

	Point pointInCycle = getCyclePoint();
	if( pointInCycle.row < 0 || pointInCycle.col < 0 ) {
		return false;
	}
	bool isCycle = dfs(pointInCycle, pointInCycle, pointInCycle, pointsVisited, true, false);
	return isCycle;
}

void CSlitherlinkGame::updateUnsatisfiedNumbersCount(int borderCountOld, int row, int col)
{
	if( borderCountOld == numbersInitial[row][col] && numbersBorderCount[row][col] != numbersInitial[row][col] ) {
		++numbersCountUnsatisfied;
	}
	if( borderCountOld != numbersInitial[row][col] && numbersBorderCount[row][col] == numbersInitial[row][col] ) {
		--numbersCountUnsatisfied;
	}
}

void CSlitherlinkGame::updateNumbersBorderCount(Edge edge, EdgeState newState)
{
	Point referPoint = std::min(edge.from, edge.to);
	if( edge.from.row - edge.to.row == 0 ) {
		// horizontal border
		if( referPoint.row > 0 ) {
			int valOld = numbersBorderCount[referPoint.row - 1][referPoint.col];
			numbersBorderCount[referPoint.row - 1][referPoint.col] += (newState == EDGE_STATE_OFF) ? -1 : 1;
			updateUnsatisfiedNumbersCount(valOld, referPoint.row - 1, referPoint.col);
		}
		if( referPoint.row < rowCount ) {
			int valOld = numbersBorderCount[referPoint.row][referPoint.col];
			numbersBorderCount[referPoint.row][referPoint.col] += (newState == EDGE_STATE_OFF) ? -1 : 1;
			updateUnsatisfiedNumbersCount(valOld, referPoint.row, referPoint.col);
		}
	} else {
		// vertical border
		if( referPoint.col > 0 ) {
			int valOld = numbersBorderCount[referPoint.row][referPoint.col - 1];
			numbersBorderCount[referPoint.row][referPoint.col - 1] += (newState == EDGE_STATE_OFF) ? -1 : 1;
			updateUnsatisfiedNumbersCount(valOld, referPoint.row, referPoint.col - 1);
		}
		if( referPoint.col < colCount ) {
			int valOld = numbersBorderCount[referPoint.row][referPoint.col];
			numbersBorderCount[referPoint.row][referPoint.col] += (newState == EDGE_STATE_OFF) ? -1 : 1;
			updateUnsatisfiedNumbersCount(valOld, referPoint.row, referPoint.col);
		}
	}
}

void CSlitherlinkGame::updateNeighborStates(Point point)
{
	auto itSet = graph.find(point);
	for( const Edge& edge : itSet->second ) {
		auto itState = edgeStates.find(edge.ordered());
		if( itState->second == EDGE_STATE_ERR ) {
			itState->second = isValidEdge(edge) ? EDGE_STATE_ON : EDGE_STATE_ERR;
			if( itState->second == EDGE_STATE_ON ) {
				--edgesCountWrong;
			}
		}
	}
}

void CSlitherlinkGame::updateEdgeState(EdgeState stateOld, std::unordered_map<Edge, EdgeState>::iterator it)
{
	if( it->second != EDGE_STATE_OFF ) {
		it->second = isValidEdge(it->first) ? EDGE_STATE_ON : EDGE_STATE_ERR;
		if( stateOld == EDGE_STATE_OFF && it->second == EDGE_STATE_ERR ) {
			++edgesCountWrong;
		}
	}
}

void CSlitherlinkGame::updateEdgeState(EdgeState stateOld, Edge edge)
{
	auto it = edgeStates.find(edge.ordered());
	updateEdgeState(stateOld, it);
}

bool CSlitherlinkGame::ToggleEdgeStatus(Edge edge)
{
	auto it = edgeStates.find(edge.ordered());
	auto isCrossedIt = isCrossed.find(edge);
	if( isCrossedIt->second ) {
		isCrossedIt->second = false;
		return false;
	}
	
	bool isEnd = false;
	if( it->second == EDGE_STATE_ERR || it->second == EDGE_STATE_ON ) {
		if( it->second == EDGE_STATE_ERR ) {
			--edgesCountWrong;
		}
		it->second = EDGE_STATE_OFF;
		updateNeighborStates(edge.from);
		updateNeighborStates(edge.to);
		updateNumbersBorderCount(edge, it->second);
		isEnd = checkGameEnd();
	} else {
		it->second = EDGE_STATE_ON;
		updateNumbersBorderCount(edge, it->second);
		isEnd = checkGameEnd();
		if( !isEnd ) {
			updateEdgeState(EDGE_STATE_OFF, it);
		}
	}
	return isEnd;
}

bool CSlitherlinkGame::ToggleEdgeCross(Edge edge)
{
	EdgeState state = edgeStates.at(edge);
	auto it = isCrossed.find(edge);
	bool isEnd = false;
	if( !it->second && state != EDGE_STATE_OFF ) {
		isEnd = ToggleEdgeStatus(edge);
	}
	if( !isEnd ) {
		it->second = !it->second;
	}
	return isEnd;
}

void CSlitherlinkGame::EraseBorders()
{
	numbersBorderCount = Matrix(numbersInitial.size(), std::vector<int>(numbersInitial.front().size(), 0));
	for( auto it = edgeStates.begin(); it != edgeStates.end(); ++it ) {
		it->second = EDGE_STATE_OFF;
	}
	initUnsatisfiedNumbersCount();
	edgesCountWrong = 0;
}

void CSlitherlinkGame::EraseCrosses()
{
	for( auto it = isCrossed.begin(); it != isCrossed.end(); ++it ) {
		it->second = false;
	}
}

std::vector<Edge> CSlitherlinkGame::GetEdgesByState(EdgeState state) const
{
	std::vector<Edge> edgesAll = GetEdges();
	std::vector<Edge> edgesRes(edgesAll.size());
	auto it = std::copy_if(edgesAll.begin(), edgesAll.end(), edgesRes.begin(), [state, this](Edge edge) { return state == edgeStates.at(edge); });
	edgesRes.resize(std::distance(edgesRes.begin(), it));
	return edgesRes;
}

std::vector<Edge> CSlitherlinkGame::GetEdges() const
{
	std::vector<Edge> edges;
	edges.reserve((rowCount + 1) * (colCount + 1));
	for( auto itMap = graph.begin(); itMap != graph.end(); ++itMap ) {
		for( auto itSet = itMap->second.begin(); itSet != itMap->second.end(); ++itSet ) {
			if( itSet->from < itSet->to ) {
				edges.push_back(*itSet);
			}
		}
	}
	return edges;
}

const Matrix& CSlitherlinkGame::GetNumbers() const
{
	return numbersInitial;
}

int CSlitherlinkGame::GetRowCount() const
{
	return rowCount;
}

int CSlitherlinkGame::GetColCount() const
{
	return colCount;
}

int CSlitherlinkGame::getNeighborsCountByState(Point p, EdgeState state) const
{
	int count = 0;
	auto itSet = graph.find(p);
	for( auto it = itSet->second.begin(); it != itSet->second.end(); ++it ) {
		if( edgeStates.at(it->ordered()) == state ) {
			++count;
		}
	}
	return count;
}

void CSlitherlinkGame::loadGameMatrix(const std::wstring& filename, Matrix* matr, std::unordered_map<Edge, EdgeState>* edgeStates)
{
	std::ifstream fileIn(filename);
	if( !fileIn.good() ) {
		throw std::runtime_error("Cannot read file");
	}
	int rowCount, colCount;
	fileIn >> rowCount >> colCount;
	matr->resize(rowCount, std::vector<int>(colCount, 0));
	for( int row = 0; row < rowCount; ++row ) {
		for( int col = 0; col < colCount; ++col ) {
			fileIn >> (*matr)[row][col];
			(*matr)[row][col] = (*matr)[row][col] < 0 ? INF : (*matr)[row][col];
		}
	}

	// horizontal
	for( int row = 0; row < rowCount + 1; ++row ) {
		for( int col = 0; col < colCount; ++col ) {
			int tmp;
			fileIn >> tmp;
			(*edgeStates)[Edge(Point(row, col), Point(row, col + 1))] = static_cast<EdgeState>(tmp);
		}
	}
	// vertical
	for( int row = 0; row < rowCount; ++row ) {
		for( int col = 0; col < colCount + 1; ++col ) {
			int tmp;
			fileIn >> tmp;
			(*edgeStates)[Edge(Point(row, col), Point(row + 1, col))] = static_cast<EdgeState>(tmp);
		}
	}
	fileIn.close();
}

CSlitherlinkGame CSlitherlinkGame::StartNewGame(const std::wstring& filename)
{
	Matrix matr;
	std::unordered_map<Edge, EdgeState> edgeStates;
	loadGameMatrix(filename, &matr, &edgeStates);
	return CSlitherlinkGame(matr, edgeStates);
}

bool CSlitherlinkGame::SaveToFile(const std::wstring& filename) const
{
	std::ofstream fileOut(filename);
	if( !fileOut.good() ) {
		return false;
	}

	fileOut << rowCount << ' ' << colCount << std::endl;
	for( int row = 0; row < rowCount; ++row ) {
		for( int col = 0; col < colCount; ++col ) {
			fileOut << ((numbersInitial[row][col] < INF) ? numbersInitial[row][col] : -1) << ' ';
		}
		fileOut << std::endl;
	}

	// horizontal
	for( int row = 0; row < rowCount + 1; ++row ) {
		for( int col = 0; col < colCount; ++col ) {
			fileOut << static_cast<int>(edgeStates.at(Edge(Point(row, col), Point(row, col + 1)))) << ' ';
		}
		fileOut << std::endl;
	}
	// vertical
	for( int row = 0; row < rowCount; ++row ) {
		for( int col = 0; col < colCount + 1; ++col ) {
			fileOut << static_cast<int>(edgeStates.at(Edge(Point(row, col), Point(row + 1, col)))) << ' ';
		}
		fileOut << std::endl;
	}
	fileOut.close();
	return true;
}
