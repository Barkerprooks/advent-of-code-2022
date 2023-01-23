#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <sys/stat.h>


#define EMPTY_V2_LIST {NULL, 0, 0}


typedef struct v2 { int x, y; } v2_t;
typedef struct v2_list {
	v2_t *data;
	ssize_t size, capacity;
} v2_list_t;


// insert an item into the list
ssize_t insert_v2(v2_list_t *, v2_t);

// allocates a new grid
char *create_grid(v2_t);

// chain for rope logic
v2_list_t create_chain(size_t, v2_t);
void chain_link_update(v2_t, v2_t *);
v2_t chain_update(v2_list_t *, v2_t);

// util
v2_t parse_input(const char *, v2_list_t *);


int main(void) {

	v2_list_t chain, list = EMPTY_V2_LIST;
	v2_t d, head, p1_tail, p2_tail;

	ssize_t i, p1 = 0, p2 = 0;
	char *p1_grid, *p2_grid;

	d = parse_input("input.txt", &list);
	p1_grid = create_grid(d);
	p2_grid = create_grid(d);
	chain = create_chain(10, list.data[0]);
	
	p1_tail = p2_tail = list.data[0];

	for (i = 1; i < list.size; i++) {

		chain_link_update(list.data[i], &p1_tail);
		p2_tail = chain_update(&chain, list.data[i]);

		p1_grid[p1_tail.x + d.x * p1_tail.y] = '*';
		p2_grid[p2_tail.x + d.x * p2_tail.y] = '*';
	}

	for (ssize_t i = 0; i < d.x * d.y; i++)	{
		if (p1_grid[i] == '*') p1++;
		if (p2_grid[i] == '*') p2++;
	}

	free(chain.data);
	free(list.data);
	free(p1_grid);
	free(p2_grid);

	printf("part 1: %d\n", p1);
	printf("part 2: %d\n", p2);

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


char *create_grid(v2_t d) {

	char *grid = (char *) malloc(sizeof(char) * d.x * d.y);
	memset(grid, '.', sizeof(char) * d.x * d.y);

	return grid;
}


v2_list_t create_chain(size_t links, v2_t head) {
	
	v2_list_t chain = EMPTY_V2_LIST;
	
	for (ssize_t i = 0; i < links; i++)
		insert_v2(&chain, head);
	
	return chain;
}


void chain_link_update(v2_t head, v2_t *tail) {
	
	v2_t delta = { head.x - tail->x, head.y - tail->y };

	if (abs(delta.x) > 1) {
		
		tail->x += delta.x > 0 ? 1 : -1;
		
		if (abs(delta.y) >= 1) tail->y += delta.y > 0 ? 1 : -1;

	} else if (abs(delta.y) > 1) {
		
		tail->y += delta.y > 0 ? 1 : -1;
		
		if (abs(delta.x) >= 1) tail->x += delta.x > 0 ? 1 : -1;
	}
}


v2_t chain_update(v2_list_t *chain, v2_t head) {
	
	chain->data[0] = head;

	for (ssize_t i = 0; i < chain->size; i++)
		chain_link_update(chain->data[i], &chain->data[i + 1]);

	return chain->data[chain->size - 1];
}


v2_t parse_input(const char *path, v2_list_t *list) {

	v2_t d, min = {INT_MAX, INT_MAX}, max = {0, 0}, v2 = {0, 0};
	
	char *buf, *cpy, *ref;
	ssize_t bytes;
	int fd, moves;
	
	struct stat info;

	if (stat(path, &info) != 0)
		return d;

	buf = calloc(sizeof(char), info.st_size + 1);
	fd = open(path, O_RDONLY);
	bytes = read(fd, buf, info.st_size);

	close(fd);

	if (bytes != info.st_size)
		return d;

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

	d.x = abs(min.x) + max.x + 1;
	d.y = abs(min.y) + max.y + 1;

	for (ssize_t i = 0; i < list->size; i++) {
		list->data[i].x += abs(min.x);
		list->data[i].y += abs(min.y);
	}

	return d;
}
