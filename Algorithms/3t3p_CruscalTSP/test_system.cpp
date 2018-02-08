#include "test_system.h"

city_t create_random_city(size_t sigma)
{
	city_t random_city;
	double x, y, distance;
	do{
		x = ((double)rand() / RAND_MAX) * 2.0 - 1.0;
		y = ((double)rand() / RAND_MAX) * 2.0 - 1.0;
		distance = x*x + y*y;
	} while (distance > 1.0 || distance == 0);
	double tmp = std::sqrt(((-2) * std::log(distance)) / distance);
	random_city.x = x * tmp * sigma;
	random_city.y = y * tmp * sigma;
	return random_city;
}

void tester()
{
	//generate cities
	std::vector<city_t> cities;
	weight_t distance = 0;
	for (size_t centre_index = 0; centre_index < CENTRES_Q; ++centre_index){
		city_t new_centre = create_random_city(SIGMA1);
		//std::cout << "Centre[" << centre_index << "] = ( " << new_centre.x << "; " << new_centre.y << " )\n\n";
		for (size_t city_index = 0; city_index < CITIES_Q; ++city_index){
			city_t new_city = create_random_city(SIGMA2);
			new_city.x += new_centre.x;
			new_city.y += new_centre.y;
			cities.push_back(new_city);
			if (city_index > 0){
				distance += euclidean_distance(cities[city_index - 1], new_city);
			}
			//std::cout << "City[" << cities.size() << "] = ( " << cities[city_index].x << "; " << cities[city_index].y << " )\n";
		}
	}
	distance += euclidean_distance(cities.front(), cities.back());

	std::vector<node_t> path;
	weight_t result = solveTSP(CITIES_Q, cities, path);
	if (path.size() != CITIES_Q + 1){
		throw std::logic_error(std::string("Wrong number of cities in path"));
	}
	if (result > distance){
		throw std::logic_error(std::string("Path is too long"));
	}
	std::cout << "Cities: " << TOTAL_CITIES_Q << ". Path_length: " << result << ". Distance: " << distance << ".\n";
}

void testTSPSolver(size_t test_quantity)
{
	srand(time(NULL));
	for (size_t test_index = 0; test_index < test_quantity; ++test_index){
		tester();
		std::cout << "Test #" << test_index << " passed!\n\n";
	}
	std::cout << "All tests passed!\n";
}

void testTSPSolver()
{
	testTSPSolver(TEST_Q);
}

