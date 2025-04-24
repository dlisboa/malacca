// binary search
// comment comment comment
#include <stdio.h>

/* block
 *  comment
 */
int bsearch(int arr[], int size, int target) {
  int low = 0, high = size - 1; // comment comment comment
  while (low <= high) {
    int mid = low + (high - low) / 2;
    if (arr[mid] == target)
      return mid;
    if (arr[mid] < target)
      low = mid + 1;
    else
      high = mid - 1;
  }
  return -1;
}

int main(void) {
  int arr[] = {1, 3, 5, 7, 9, 11, 13, 15};
  int size = 8;
  int elem = 12;

  int result = bsearch(arr, size, elem);
  if (result != -1) {
    printf("found %d at index: %d\n", elem, result);
  } else {
    printf("%d not found\n", elem);
  }

  return 0;
}
