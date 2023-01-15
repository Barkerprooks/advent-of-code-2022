#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


enum direction { U = 85, D = 68, L = 76, R = 82};
struct cell { int x, y; };
struct move {
	enum direction direction;
	int count;
};


void parse(const char *, struct move *);

int update(struct cell *, struct move *);

int findmax(struct cell *);
int findmin(struct cell *);

int parseinput(const char *, struct cell *);

int main(void) {

	struct cell *cells = malloc(1);
	int count;
	
	count = parseinput("input.txt", cells);

	free(cells);

	return 0;
}


void parse(const char *line, struct move *move) {
	move->direction = (enum direction) line[0];
	move->count = atoi(line + (sizeof(char) * 2));
}


int update(struct cell *cell, struct move *move) {
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


int parseinput(const char *path, struct cell *cells) {
	
	struct cell cell = { 0, 0 };
	struct move move;
	
	char *ref, *cpy, buffer[8192] = { 0 };
	int index, fd;

	fd = open(path, O_RDONLY);

	if (fd < 0)
		return fd;

	read(fd, buffer, 8192);
	close(fd);

	index = 0;
	cpy = buffer;

	while((ref = strsep(&cpy, "\n"))) {
		parse(ref, &move);
		do {
			cells[index++] = cell;
			struct cell *p = realloc(cells, index);
			if (p == NULL) {
				return -1;
			} else {
				cells = p;
			}
		} while(update(&cell, &move) > 0);
	}

	return --index;
}
