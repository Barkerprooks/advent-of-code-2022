#include <iostream>
#include <fstream>
#include <sstream>
#include <deque>
#include <map>

class Window {
private:
	std::map<char, int> letters;
	std::deque<char> deque;
	size_t capacity;
public:
	Window(size_t);
	bool next(char);
};

Window::Window(size_t capacity) {
	this->capacity = capacity;
}

bool Window::next(char letter) {	
	
	if (deque.size() >= capacity) {
		
		char front = deque.front();
		
		letters[front] -= 1;
		if (letters[front] == 0)
			letters.erase(front);
		
		deque.pop_front();
	}
	
	deque.push_back(letter);
	
	if (letters.find(letter) == letters.end()) {
		letters[letter] = 1;
	} else {
		letters[letter] += 1;
	}

	return letters.size() == capacity;
}

int main(void) {

	std::fstream file("input.txt");	
	std::stringstream buffer;

	unsigned int p1 = 0, p2 = 0, n = 0;

	Window w1(4), w2(14);

	if (!file.good()) {
		std::cout << "file not found" << std::endl;
		return 0;
	}

	buffer << file.rdbuf();

	for(char letter : buffer.str()) {
		
		n++;
		
		if(p1 == 0 && w1.next(letter))
			p1 = n;

		if(p2 == 0 && w2.next(letter))
			p2 = n;

		if(p1 * p2 > 0)
			break;
	}

	std::cout << "part 1: " << p1 << std::endl;
	std::cout << "part 2: " << p2 << std::endl;

	return 0;
}
