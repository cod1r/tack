struct Buffer {
  float* contents;
  int size; // amount written in buffer.
  int capacity; // max amount that can be written into buffer
};
