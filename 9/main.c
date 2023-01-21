#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <sys/stat.h>

typedef struct v2 { int x, y; } v2_t;
typedef struct v2_list {
	v2_t *data;
	ssize_t size, capacity;
} v2_list_t;

ssize_t insert_v2(v2_list_t *, v2_t);
v2_t parse_input(const char *, v2_list_t *);

void chain_link_update(v2_t *, v2_t *);
void chain_update();

void print_grid(char *, v2_t, v2_t, v2_t);

int main(void) {

	v2_list_t list = {NULL, 0, 0};
	v2_t delta, head = {0, 0}, tail = {0, 0}, dim;
	ssize_t i, p1 = 0;

	dim = parse_input("input.txt", &list);

	char *grid = calloc(sizeof(char), dim.x * dim.y);
	memset(grid, '.', sizeof(char) * dim.x * dim.y);

	head = list.data[0];
	tail = head;

	for (i = 1; i < list.size; i++) {
		
		head = list.data[i];
		
		delta.x = head.x - tail.x;
		delta.y = head.y - tail.y;

		if (abs(delta.x) > 1) {
			tail.x += delta.x > 0 ? 1 : -1;
			if (abs(delta.y) >= 1)
				tail.y += delta.y > 0 ? 1 : -1;
		} else if (abs(delta.y) > 1) {
			tail.y += delta.y > 0 ? 1 : -1;
			if (abs(delta.x) >= 1)
				tail.x += delta.x > 0 ? 1 : -1;
		}

		grid[tail.x + dim.x * tail.y] = '*';
	}

	for (ssize_t i = 0; i < dim.x * dim.y; i++)	
		if (grid[i] == '*') p1++;

	printf("part 1: %d\n", p1);

	return 0;
}

ssize_t insert_v2(v2_list_t *list, v2_t v2) {
	
	ssize_t size = list->size, capacity = list->capacity;

	if (size == capacity) {
	
		capacity = capacity ? capacity * 2 : 32;
		v2_t *p = realloc(list->data, sizeof(v2_t) * capacity);
		
		if (p == NULL)
			return -1;

		list->capacity = capacity;
		list->data = p;
	}

	list->data[size] = v2;
	return list->size++;
}

v2_t parse_input(const char *path, v2_list_t *list) {

	v2_t dim, min = {INT_MAX, INT_MAX}, max = {0, 0}, v2 = {0, 0};
	
	char *buf, *cpy, *ref;
	ssize_t bytes;
	int fd, moves;
	
	struct stat info;

	if (stat(path, &info) != 0)
		return dim;

	buf = calloc(sizeof(char), info.st_size + 1);
	fd = open(path, O_RDONLY);
	bytes = read(fd, buf, info.st_size);

	close(fd);

	if (bytes != info.st_size)
		return dim;

	insert_v2(list, v2);

	cpy = buf;
	while ((ref = strsep(&cpy, "\n"))) {
		
		moves = atoi(ref + 2);
		
		while (moves--) {
			switch (ref[0]) {
				case 'U': v2.y--;
					break;
				case 'D': v2.y++;
					break;
				case 'L': v2.x--;
					break;
				case 'R': v2.x++;
					break;
			}
			insert_v2(list, v2);
		}

		if (min.x > v2.x) min.x = v2.x;
		if (min.y > v2.y) min.y = v2.y;
		if (max.x < v2.x) max.x = v2.x;
		if (max.y < v2.y) max.y = v2.y;
	}

	free(buf);

	dim.x = abs(min.x) + max.x + 1;
	dim.y = abs(min.y) + max.y + 1;

	for (ssize_t i = 0; i < list->size; i++) {
		list->data[i].x += abs(min.x);
		list->data[i].y += abs(min.y);
	}

	return dim;
}

void print_grid(char *grid, v2_t dim, v2_t head, v2_t tail) {
	for (ssize_t y = 0; y < dim.y; y++) {
		for (ssize_t x = 0; x < dim.x; x++) {
			if (head.x == x && head.y == y)
				printf("#");
			else if (tail.x == x && tail.y == y)
				printf("$");
			else
				printf("%c", grid[x + dim.x * y]);
		}
		printf("\n");
	}
}
