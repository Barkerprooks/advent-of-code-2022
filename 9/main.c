#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>


enum direction { U = 85, D = 68, L = 76, R = 82};
struct cell { int x, y; };
struct list {
	struct cell *cells;
	int length, capacity;
};
struct move {
	enum direction direction;
	int count;
};

int create_cell_list(struct list *, int);
int expand_cell_list(struct list *, int);
int append_cell_list(struct list *, int, int);
void delete_cell_list(struct list *);

struct cell find_max_cell(struct list);
struct cell find_min_cell(struct list);

void parse(const char *, struct move *);
int parse_input(const char *, struct list *);
int update_cell(struct cell *, struct move *);

int main(void) {
	
	struct list list;
	struct cell max, min;

	parse_input("input.txt", &list);

	max = find_max_cell(list);
	min = find_min_cell(list);

	printf("max (x: %d, y: %d)\n", max.x, max.y);
	printf("min (x: %d, y: %d)\n", min.x, min.y);

	for(int i = 0; i < list.length; i++)
		printf("%d: %d\n", i, list.cells[i].x);

	delete_cell_list(&list);

	return 0;
}

int create_cell_list(struct list *list, int n) {
	
	list->cells = malloc(sizeof(struct cell) * n);
	
	if (!list->cells)
		return 0;
	
	list->length = 0;
	list->capacity = n;

	return 1;
}

int expand_cell_list(struct list *list, int n) {
	
	struct cell *new = realloc(list->cells, sizeof(struct cell) * n);
	
	if (!new)
		return 0;
	
	list->cells = new;
	list->capacity += n;
	
	return 1;
}


int append_cell_list(struct list *list, int x, int y) {
	
	if (list->length + 1 > list->capacity)
		expand_cell_list(list, 64);

	struct cell cell = {x, y};

	list->cells[list->length++] = cell;
}

void delete_cell_list(struct list *list) {
	free(list->cells);
	list->length = 0;
	list->capacity = 0;
}


struct cell find_max_cell(struct list list) {
	
	struct cell cell, max = {0, 0};
	
	for(int i = 0; i < list.length; i++) {
		cell = list.cells[i];
		
		if (cell.x > max.x)
			max.x = cell.x;

		if (cell.y > max.y)
			max.y = cell.y;
	}

	return max;
}

struct cell find_min_cell(struct list list) {
	
	struct cell cell, min = {INT_MAX, INT_MAX};
	
	for(int i = 0; i < list.length; i++) {
		cell = list.cells[i];
		
		if (cell.x < min.x)
			min.x = cell.x;

		if (cell.y < min.y)
			min.y = cell.y;
	}
	
	return min;
}


void parse(const char *line, struct move *move) {
	move->direction = (enum direction) line[0];
	move->count = atoi(line + (sizeof(char) * 2));
}

int parse_input(const char *path, struct list *list) {
	
	struct cell cell = { 0, 0 };
	struct move move;
	
	char *ref, *cpy, buffer[8192] = { 0 };
	int index = 0, fd = open(path, O_RDONLY);

	if (fd < 0)
		return fd;

	read(fd, buffer, 8192);
	close(fd);

	create_cell_list(list, 64);

	cpy = buffer;

	while((ref = strsep(&cpy, "\n"))) {
		parse(ref, &move);
		do {
			append_cell_list(list, cell.x, cell.y);
		} while(update_cell(&cell, &move) > 0);
	}

	return --index;
}

int update_cell(struct cell *cell, struct move *move) {
	switch(move->direction) {
		case U: cell->y -= 1;
			break;
		case D: cell->y += 1;
			break;
		case L: cell->x -= 1;
			break;
		case R: cell->x += 1;
			break;
	}
	return --(move->count);
}
