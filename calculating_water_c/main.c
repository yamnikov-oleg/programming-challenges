#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 1
#define FALSE 0

// Structure of a water container.
typedef struct container_t {
  // Input array: list of height values.
  unsigned char* columns;
  // Number of values in the array above.
  size_t column_cnt;
} container_t;

// Allocates a new container with given number of columns.
container_t* container_create(size_t col_cnt) {
  container_t* ctr = malloc(sizeof(container_t));
  ctr->columns = malloc(sizeof(unsigned char) * col_cnt);
  ctr->column_cnt = col_cnt;
  return ctr;
}

// Reallocates a container to append a new column onto the right side.
void container_add_column(container_t* ctr) {
  ctr->column_cnt++;
  ctr->columns = realloc(ctr->columns, sizeof(unsigned char) * ctr->column_cnt);
}

// Free a container.
void container_destroy(container_t* ctr) {
  free(ctr->columns);
  free(ctr);
}

// Maximum length on the input.
#define INPUT_BUFFER_SIZE 255

// Error codes for input-parsing procedure.
#define PARSE_UNDEFINED -1
#define PARSE_SUCCESS 0
#define PARSE_EMPTY 1
#define PARSE_INVALID_NUMBER 2

// Parses user input and stores parsed values into the `ctr`.
// If `ctr` has not enough columns, they will allocated with container_add_column.
int parse_container(char *input, container_t* ctr) {
  size_t input_len = strlen(input);
  int ret_code = PARSE_UNDEFINED;

  char* element_buf = malloc(sizeof(char) * INPUT_BUFFER_SIZE);
  size_t element_index = 0;
  unsigned char element;

  // Empty element buffer string.
  element_buf[0] = '\0';

  // Walk through input chars.
  for (size_t i = 0; i < input_len; i++) {
    // If char is a space - parse element buffer and append the element to
    // the container columns.
    if (input[i] == ' ') {
      if (strlen(element_buf) == 0) continue;

      int result = sscanf(element_buf, "%hhu", &element);
      if (result < 1) {
        ret_code = PARSE_INVALID_NUMBER;
        goto free_and_return;
      }

      while(element_index >= ctr->column_cnt)
        container_add_column(ctr);
      ctr->columns[element_index] = element;
      element_index++;
      element_buf[0] = '\0';
    // If char is not space - append it to element buffer.
    } else {
      size_t buf_len = strlen(element_buf);
      element_buf[buf_len] = input[i];
      element_buf[buf_len+1] = '\0';
    }
  }

  // If after finishing the char walk the element buffer is not empty -
  // parse the last elemnt as usual.
  if (strlen(element_buf) != 0) {
    int result = sscanf(element_buf, "%hhu", &element);
    if (result < 1) {
      ret_code = PARSE_INVALID_NUMBER;
      goto free_and_return;
    }

    while(element_index >= ctr->column_cnt)
    container_add_column(ctr);
    ctr->columns[element_index] = element;
    element_index++;
  }

  if (element_index == 0) {
    ret_code = PARSE_EMPTY;
    goto free_and_return;
  }

  ret_code = PARSE_SUCCESS;

free_and_return:
  free(element_buf);
  return ret_code;
}

// Returns TRUE if the cell of the container does not contain "container block".
// It is safe to pass column index larger than columns count.
int container_is_cell_empty(container_t* ctr, unsigned char row, size_t col) {
  if (col >= ctr->column_cnt)
    return TRUE;

  if (row >= ctr->columns[col])
    return TRUE;

  return FALSE;
}

// Calculates the capacity of a given row of container.
int container_get_row_capacity(container_t* ctr, unsigned char row) {
  size_t left_border = -1;
  for (size_t i = 0; i < ctr->column_cnt; i++) {
    if (!container_is_cell_empty(ctr, row, i)) {
      left_border = i;
      break;
    }
  }

  if (left_border == -1) {
    // The row is completely empty, no borders at all.
    return 0;
  }

  size_t right_border = left_border;
  for (size_t i = ctr->column_cnt-1; i >= 0; i--) {
    if (!container_is_cell_empty(ctr, row, i)) {
      right_border = i;
      break;
    }
  }

  int capacity = 0;
  for (size_t i = left_border+1; i < right_border; i++) {
    if (container_is_cell_empty(ctr, row, i))
      capacity++;
  }

  return capacity;
}

// Finds the height of a container (maximum column height).
unsigned char container_get_height(container_t* ctr) {
  unsigned char height = 0;
  for (size_t i = 0; i < ctr->column_cnt; i++) {
    if (ctr->columns[i] > height) {
      height = ctr->columns[i];
    }
  }
  return height;
}

// Calculates full water capacity of a container.
int container_get_capacity(container_t* ctr) {
  int capacity = 0;
  unsigned char height = container_get_height(ctr);
  for (unsigned char row = 0; row < height; row++) {
    capacity += container_get_row_capacity(ctr, row);
  }
  return capacity;
}

// Prints the container contents as blocks of ' ' and 'X' into the stdout.
void container_print(container_t* ctr) {
  unsigned char height = container_get_height(ctr);
  for (unsigned char row = height-1; row >= 0; row--) {
    for (size_t col = 0; col < ctr->column_cnt; col++) {
      if (container_is_cell_empty(ctr, row, col))
        printf(" ");
      else
        printf("X");
    }
    printf("\n");

    if (row == 0) break;
  }
}

// Runs assertions to test the code.
void test() {
  container_t* ctr = container_create(0);

  // 4   X
  // 3   X  X
  // 2 X X  X
  // 1 X XXXX
  // 0 XXXXXX
  //   012345
  parse_container("  3  1 5  2 2 4 ", ctr);
  assert(ctr->column_cnt == 6);
  assert(ctr->columns[0] == 3);
  assert(ctr->columns[1] == 1);
  assert(ctr->columns[2] == 5);
  assert(ctr->columns[3] == 2);
  assert(ctr->columns[4] == 2);
  assert(ctr->columns[5] == 4);

  assert(container_is_cell_empty(ctr, 0, 0) == FALSE);
  assert(container_is_cell_empty(ctr, 1, 0) == FALSE);
  assert(container_is_cell_empty(ctr, 1, 1) == TRUE);
  assert(container_is_cell_empty(ctr, 1, 99) == TRUE);
  assert(container_is_cell_empty(ctr, 99, 1) == TRUE);

  assert(container_get_row_capacity(ctr, 0) == 0);
  assert(container_get_row_capacity(ctr, 1) == 1);
  assert(container_get_row_capacity(ctr, 2) == 3);
  assert(container_get_row_capacity(ctr, 3) == 2);
  assert(container_get_row_capacity(ctr, 4) == 0);
  assert(container_get_row_capacity(ctr, 5) == 0);

  assert(container_get_height(ctr) == 5);

  assert(container_get_capacity(ctr) == 6);

  container_destroy(ctr);
}

int main(int argc, char **argv) {
  test();

  char* input_buf = malloc(sizeof(char) * INPUT_BUFFER_SIZE);

  printf("Type input array as decimal integers, separated by space:\n");
  fgets(input_buf, INPUT_BUFFER_SIZE, stdin);

  container_t* ctr = container_create(0);

  switch (parse_container(input_buf, ctr)) {
  case PARSE_EMPTY:
    printf("ERROR: Container is empty\n");
    return 1;
  case PARSE_INVALID_NUMBER:
    printf("ERROR: Row values must be unsigned integers [0..255]\n");
    return 1;
  case PARSE_UNDEFINED:
    printf("ERROR: Undefined error\n");
    return 1;
  }

  container_print(ctr);
  printf("Capacity = %d\n", container_get_capacity(ctr));

  container_destroy(ctr);
  return 0;
}
