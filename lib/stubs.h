struct Buffer {
  float* contents;
  int size; // amount written in buffer.
  int capacity; // max amount that can be written into buffer
};

// The purpose of this struct is to hold important information
// that we need from FT_Glyph_Metrics for a particular glyph
// and to wrap a Buffer struct. Buffer structs are used for bitmaps for performance reasons.
struct GlyphInfo {
    int horiBearingX;
    int horiBearingY;
    int x_advance;
    int y_advance;
    struct Buffer buffer;
};
