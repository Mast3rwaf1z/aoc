#include <fstream>
#include <iostream>
#include <ostream>
#include <string>
#include <vector>

// Utils
template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T> rhs) {
    os << "[";
    for (const auto& line : rhs) {
        if(line == rhs.back())
            os << line;
        else
           os << line << ", ";
    }
    os << "]";
    return os;
}

template <typename T>
const std::vector<std::tuple<int, T&>> enumerate(std::vector<T>& vec) {
    std::vector<std::tuple<int, T&>> result;
    for (auto i = 0; i < vec.size(); i++) {
        result.push_back({i, vec[i]});
    }
    return result;
}

std::vector<std::tuple<int, char>> enumerate(std::string vec) {
    std::vector<std::tuple<int, char>> result;
    for (auto i = 0; i < vec.size(); i++) {
        result.push_back({i, vec[i]});
    }
    return result;
}

typedef std::tuple<unsigned int, unsigned int> coordinate;

template <typename T>
bool contains(std::vector<T> vec, T item) {
    return std::find(vec.begin(), vec.end(), item) != vec.end();
}


class AntiNode {
public:
    const coordinate position;
    const char frequency;
    AntiNode(coordinate position, char freq) : position(position), frequency(freq) {}
    friend bool operator==(const AntiNode& lhs, const AntiNode& rhs) {
        const auto& [x1, y1] = lhs.position;
        const auto& [x2, y2] = rhs.position;
        return x1 == x2 && y1 == y2;
    }
    const bool isValid(const unsigned int width, const unsigned int height) const {
        const auto& [x, y] = position;
        return x >= 0 && x < width && y >= 0 && y < height;
    }
    friend std::ostream& operator<<(std::ostream& os, const AntiNode& rhs) {
        os << '(' << std::get<0>(rhs.position) << ", " << std::get<1>(rhs.position) << ')';
        return os;
    }
};

class Antenna {
public:
    const coordinate position;
    const coordinate size;
    const char frequency;

    Antenna(coordinate position, char freq, coordinate size) : position(position), size(size), frequency(freq) {}
    friend std::ostream& operator<<(std::ostream& os, const Antenna& rhs) {
        os << '(' << std::get<0>(rhs.position) << ", " << std::get<1>(rhs.position) << ')';
        return os;
    }
    friend bool operator==(const Antenna& lhs, const Antenna& rhs) {
        return lhs.position == rhs.position;
    }

    std::vector<AntiNode> operator-(const Antenna& rhs) {
        const auto& [x1, y1] = position;
        const auto& [x2, y2] = rhs.position;
        const auto& [X, Y] = size;
        std::vector<AntiNode> result;
        coordinate coords{x1, y1};
        for(auto i = 1; AntiNode(coords, frequency).isValid(X, Y); i++) {
            result.push_back(AntiNode(coords, frequency));
            coords = {x1 + ((x1-x2)*i), y1 + ((y1-y2)*i)};
        }
        return result;
    }

};

class AntennaMap {
    const std::vector<std::string> map;
    std::vector<Antenna> antennas;
    std::vector<AntiNode> antiNodes;
public:
    AntennaMap(std::vector<std::string> map) : map(map) {
        for (const auto& [i, line] : enumerate(map)) {
            for (const auto& [j, c] : enumerate(line)) {
                if(c != '.' && c != '\n')
                    antennas.push_back(Antenna({i, j}, c, {map.size(), map[0].size()}));
            }
        }
        for(auto& antenna1 : antennas) {
            for(auto& antenna2 : antennas) {
                if(antenna1 == antenna2)
                    continue;
                if(antenna1.frequency != antenna2.frequency)
                    continue;
                const auto antiNodes1 = antenna1 - antenna2;
                const auto antiNodes2 = antenna2 - antenna1;
                
                for(auto& antiNode1 : antiNodes1)
                    if(!contains(antiNodes, antiNode1) && antiNode1.isValid(map.size(), map[0].size()))
                        antiNodes.push_back(antiNode1);
                for(auto& antiNode2 : antiNodes2)
                    if(!contains(antiNodes, antiNode2) && antiNode2.isValid(map.size(), map[0].size()))
                        antiNodes.push_back(antiNode2);
            }
        }
    }
    std::vector<AntiNode>& getAntiNodes() {
        return antiNodes;
    }

    friend std::ostream& operator<<(std::ostream& os, const AntennaMap& rhs) {
        std::vector<std::vector<std::string>> editedMap;
        for(const auto& line : rhs.map){
            std::vector<std::string> editedLine;
            for(const auto& c : line)
                editedLine.push_back({c});
            editedMap.push_back(editedLine);
        }
        for(const auto& antiNode : rhs.antiNodes) {
            const auto& [x, y] = antiNode.position;
            if (editedMap[x][y] == ".")
                editedMap[x][y] = "\033[38;2;255;255;0m#\033[0m";
            else
                editedMap[x][y] = std::format("\033[38;2;255;255;0m{}\033[0m", editedMap[x][y]);
        }
        for(const auto& antenna : rhs.antennas) {
            const auto& [x, y] = antenna.position;
            if(editedMap[x][y][0] == antenna.frequency)
                editedMap[x][y] = std::format("\033[38;2;0;255;0m{}\033[0m", antenna.frequency);
        }
        for(auto& line : editedMap) {
            for(auto& pos : line) {
                os << pos;
            }
            os << std::endl;
        }
        std::cout << "total antennas:  " << rhs.antennas.size() << std::endl;
        std::cout << "total antiNodes: " << rhs.antiNodes.size();
        return os;
    }
};

std::vector<std::string> testData = {
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
};

int main() {
    std::cout << "Day 8" << std::endl;
    AntennaMap testMap(testData);
    std::cout << "Test Data" << std::endl;
    std::cout << testMap << std::endl;
    std::cout << testMap.getAntiNodes() << std::endl;
       
    std::ifstream stream("inputs/day8_input.txt");
    std::vector<std::string> input;
    std::string line;
    while(std::getline(stream, line))
        input.push_back(line);
    AntennaMap map(input);
    std::cout << map << std::endl;
    return EXIT_SUCCESS;
}
