#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


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
int remove_cell_list(struct list *, int);
void delete_cell_list(struct list *);

int update_cell(struct cell *, struct move *);

void parse(const char *, struct move *);
int parseinput(const char *, struct list *);

int main(void) {
	
	struct list list;
	
	parseinput("input.txt", &list);

	for(int i = 0; i < list.length; i++)
		printf("(%d, %d)\n", list.cells[i].x, list.cells[i].y);

	delete_cell_list(&list);

	return 0;
}


int create_cell_list(struct list *list, int n) {
	list->cells = malloc(sizeof(struct cell) * n);
	list->length = 0;
	list->capacity = n;
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

void parse(const char *line, struct move *move) {
	move->direction = (enum direction) line[0];
	move->count = atoi(line + (sizeof(char) * 2));
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


int parseinput(const char *path, struct list *list) {
	
	struct cell cell = { 0, 0 };
	struct move move;
	
	char *ref, *cpy, buffer[8192] = { 0 };
	int index = 0, fd = open(path, O_RDONLY);

	if (fd < 0)
		return fd;

	read(fd, buffer, 8192);
	close(fd);

	create_cell_list(list, 1024);

	cpy = buffer;

	while((ref = strsep(&cpy, "\n"))) {
		parse(ref, &move);
		do {
			append_cell_list(list, cell.x, cell.y);
		} while(update_cell(&cell, &move) > 0);
	}

	return --index;
}
