/*******************************************************************************
 * Size: 14 px
 * Bpp: 4
 * Opts: 
 ******************************************************************************/

#include "lvgl.h"

#ifndef SCP_REG_14
#define SCP_REG_14 1
#endif

#if SCP_REG_14

/*-----------------
 *    BITMAPS
 *----------------*/

/*Store the image of the glyphs*/
static LV_ATTRIBUTE_LARGE_CONST const uint8_t glyph_bitmap[] = {
    /* U+0020 " " */

    /* U+0021 "!" */
    0x5c, 0x5, 0xb0, 0x4b, 0x4, 0xa0, 0x3a, 0x3,
    0x90, 0x2, 0xa, 0xf1, 0x8d, 0x0,

    /* U+0022 "\"" */
    0x2b, 0x30, 0xa6, 0x2f, 0x50, 0xf8, 0x1f, 0x40,
    0xd7, 0xf, 0x20, 0xc5, 0xd, 0x0, 0xa4,

    /* U+0023 "#" */
    0x0, 0xb0, 0x55, 0x0, 0x29, 0x8, 0x30, 0x6d,
    0xec, 0xfd, 0x30, 0x65, 0xb, 0x0, 0x7, 0x40,
    0xb0, 0x9, 0xec, 0xcf, 0xc0, 0xb, 0x2, 0xa0,
    0x0, 0xc0, 0x38, 0x0, 0xc, 0x5, 0x60, 0x0,

    /* U+0024 "$" */
    0x0, 0x2b, 0x0, 0x0, 0x2, 0xb0, 0x0, 0x9,
    0xed, 0xe6, 0x5, 0xe0, 0x1, 0x30, 0x2f, 0x60,
    0x0, 0x0, 0x2b, 0xe7, 0x0, 0x0, 0x4, 0xdb,
    0x0, 0x0, 0x1, 0xf1, 0x88, 0x20, 0x7e, 0x1,
    0x9d, 0xf9, 0x20, 0x0, 0x2b, 0x0, 0x0, 0x2,
    0xb0, 0x0,

    /* U+0025 "%" */
    0x2a, 0xb8, 0x0, 0x6, 0x8, 0x60, 0xb3, 0x1c,
    0x50, 0x87, 0xc, 0x2a, 0x30, 0x1, 0xab, 0x60,
    0x0, 0x0, 0x0, 0x0, 0x1a, 0xb3, 0x0, 0x9,
    0x3a, 0x31, 0xc0, 0x8, 0x80, 0xd0, 0xd, 0x5,
    0xb0, 0xb, 0x31, 0xd0, 0x1, 0x0, 0x3c, 0xc4,
    0x0,

    /* U+0026 "&" */
    0x0, 0x8d, 0xb0, 0x0, 0x0, 0x3d, 0x8, 0x60,
    0x0, 0x3, 0xc0, 0xc3, 0x0, 0x0, 0xe, 0xb6,
    0x0, 0x0, 0x3, 0xec, 0x0, 0x3b, 0x1, 0xe2,
    0xaa, 0xb, 0x70, 0x5c, 0x0, 0xbb, 0xe0, 0x2,
    0xf4, 0x4, 0xfd, 0x40, 0x5, 0xde, 0xb3, 0x6c,
    0x0,

    /* U+0027 "'" */
    0x6a, 0x8f, 0x7d, 0x5c, 0x3a,

    /* U+0028 "(" */
    0x0, 0x0, 0x10, 0x0, 0xb6, 0x0, 0x98, 0x0,
    0x3d, 0x0, 0xa, 0x60, 0x0, 0xe1, 0x0, 0xf,
    0x0, 0x0, 0xf0, 0x0, 0xf, 0x10, 0x0, 0xb5,
    0x0, 0x5, 0xc0, 0x0, 0xb, 0x60, 0x0, 0x1d,
    0x40, 0x0, 0x12,

    /* U+0029 ")" */
    0x1, 0x0, 0x1, 0xd3, 0x0, 0x3, 0xe1, 0x0,
    0x7, 0xa0, 0x0, 0xe, 0x0, 0x0, 0xb4, 0x0,
    0x9, 0x60, 0x0, 0x96, 0x0, 0xb, 0x50, 0x0,
    0xe1, 0x0, 0x5b, 0x0, 0x1e, 0x20, 0x1d, 0x50,
    0x0, 0x30, 0x0,

    /* U+002A "*" */
    0x0, 0x28, 0x0, 0x0, 0x3, 0x90, 0x0, 0x8b,
    0x9c, 0x9c, 0x10, 0x1d, 0xf5, 0x0, 0x3, 0xc7,
    0x90, 0x0, 0xc2, 0xa, 0x30, 0x1, 0x0, 0x0,
    0x0,

    /* U+002B "+" */
    0x0, 0x13, 0x0, 0x0, 0x4, 0xa0, 0x0, 0x0,
    0x4a, 0x0, 0xb, 0xde, 0xfd, 0xd2, 0x0, 0x4a,
    0x0, 0x0, 0x4, 0xa0, 0x0, 0x0, 0x4a, 0x0,
    0x0,

    /* U+002C "," */
    0x0, 0x0, 0x9, 0xf3, 0x8, 0xf7, 0x0, 0xa6,
    0x3, 0xe1, 0x1d, 0x30, 0x0, 0x0,

    /* U+002D "-" */
    0xbd, 0xdd, 0xdd, 0x20,

    /* U+002E "." */
    0x13, 0xc, 0xf3, 0x9d, 0x10,

    /* U+002F "/" */
    0x0, 0x0, 0x3d, 0x0, 0x0, 0x9, 0x70, 0x0,
    0x0, 0xe1, 0x0, 0x0, 0x5a, 0x0, 0x0, 0xb,
    0x40, 0x0, 0x2, 0xe0, 0x0, 0x0, 0x88, 0x0,
    0x0, 0xe, 0x20, 0x0, 0x4, 0xc0, 0x0, 0x0,
    0xa6, 0x0, 0x0, 0x1e, 0x0, 0x0, 0x6, 0x90,
    0x0, 0x0,

    /* U+0030 "0" */
    0x6, 0xde, 0xa0, 0x4, 0xd2, 0x9, 0xb0, 0xb6,
    0x0, 0xe, 0x2e, 0x25, 0xa0, 0xc4, 0xf1, 0x7d,
    0xb, 0x6e, 0x20, 0x0, 0xc4, 0xb6, 0x0, 0xf,
    0x14, 0xe2, 0x9, 0xa0, 0x6, 0xde, 0xa0, 0x0,

    /* U+0031 "1" */
    0x7, 0xbf, 0x10, 0x0, 0x67, 0xf1, 0x0, 0x0,
    0xf, 0x10, 0x0, 0x0, 0xf1, 0x0, 0x0, 0xf,
    0x10, 0x0, 0x0, 0xf1, 0x0, 0x0, 0xf, 0x10,
    0x0, 0x0, 0xf1, 0x0, 0x9f, 0xff, 0xff, 0x60,

    /* U+0032 "2" */
    0x1, 0xae, 0xe9, 0x0, 0xa, 0x50, 0x2c, 0x80,
    0x0, 0x0, 0x6, 0xc0, 0x0, 0x0, 0x8, 0x90,
    0x0, 0x0, 0x2e, 0x20, 0x0, 0x1, 0xd5, 0x0,
    0x0, 0x1d, 0x60, 0x0, 0x2, 0xd5, 0x0, 0x0,
    0xe, 0xff, 0xff, 0xf4,

    /* U+0033 "3" */
    0x2, 0xae, 0xea, 0x10, 0x7, 0x60, 0x1b, 0xb0,
    0x0, 0x0, 0x5, 0xe0, 0x0, 0x0, 0x3c, 0x70,
    0x0, 0x2f, 0xf7, 0x0, 0x0, 0x0, 0x29, 0xc0,
    0x0, 0x0, 0x0, 0xf2, 0xb, 0x40, 0x17, 0xe0,
    0x3, 0xbe, 0xeb, 0x30,

    /* U+0034 "4" */
    0x0, 0x0, 0x5f, 0x30, 0x0, 0x2, 0xdd, 0x30,
    0x0, 0xc, 0x5d, 0x30, 0x0, 0x8a, 0xd, 0x30,
    0x3, 0xd0, 0xd, 0x30, 0x1d, 0x30, 0xd, 0x30,
    0x6f, 0xee, 0xef, 0xf9, 0x0, 0x0, 0xd, 0x30,
    0x0, 0x0, 0xd, 0x30,

    /* U+0035 "5" */
    0x3, 0xff, 0xff, 0xd0, 0x4, 0xc0, 0x0, 0x0,
    0x5, 0xa0, 0x0, 0x0, 0x7, 0xdc, 0xeb, 0x20,
    0x1, 0x50, 0x8, 0xe0, 0x0, 0x0, 0x0, 0xf3,
    0x0, 0x0, 0x0, 0xf2, 0xc, 0x40, 0x19, 0xc0,
    0x4, 0xbe, 0xea, 0x10,

    /* U+0036 "6" */
    0x2, 0xbe, 0xd8, 0x1, 0xe6, 0x1, 0x50, 0x88,
    0x0, 0x0, 0xb, 0x59, 0xdc, 0x40, 0xed, 0x30,
    0x3f, 0x2d, 0x40, 0x0, 0xc6, 0xa7, 0x0, 0xb,
    0x53, 0xe3, 0x4, 0xe1, 0x4, 0xce, 0xc3, 0x0,

    /* U+0037 "7" */
    0xf, 0xff, 0xff, 0xf6, 0x0, 0x0, 0x3, 0xd0,
    0x0, 0x0, 0xd, 0x20, 0x0, 0x0, 0x79, 0x0,
    0x0, 0x0, 0xe1, 0x0, 0x0, 0x4, 0xd0, 0x0,
    0x0, 0x8, 0x90, 0x0, 0x0, 0xb, 0x70, 0x0,
    0x0, 0xc, 0x60, 0x0,

    /* U+0038 "8" */
    0x0, 0x7e, 0xec, 0x20, 0x4, 0xe1, 0x7, 0xd0,
    0x6, 0xa0, 0x0, 0xf0, 0x1, 0xd6, 0x6, 0x80,
    0x0, 0x8c, 0xde, 0x10, 0xa, 0x70, 0x6, 0xe1,
    0xf, 0x10, 0x0, 0xb6, 0xc, 0x81, 0x3, 0xe3,
    0x1, 0xae, 0xec, 0x50,

    /* U+0039 "9" */
    0x1, 0xae, 0xd8, 0x0, 0xc, 0x81, 0x9, 0x90,
    0xf, 0x10, 0x0, 0xe1, 0xd, 0x81, 0x7, 0xf3,
    0x2, 0xbe, 0xc5, 0xd4, 0x0, 0x0, 0x0, 0xf2,
    0x0, 0x0, 0x5, 0xe0, 0x6, 0x40, 0x3e, 0x60,
    0x4, 0xcf, 0xd6, 0x0,

    /* U+003A ":" */
    0x9d, 0x1c, 0xf3, 0x14, 0x0, 0x0, 0x13, 0xc,
    0xf3, 0x9d, 0x10,

    /* U+003B ";" */
    0x9, 0xd1, 0xc, 0xf3, 0x1, 0x40, 0x0, 0x0,
    0x0, 0x0, 0x9, 0xf3, 0x8, 0xf7, 0x0, 0xa6,
    0x3, 0xe1, 0x1d, 0x30, 0x0, 0x0,

    /* U+003C "<" */
    0x0, 0x0, 0x5, 0x0, 0x1, 0xba, 0x0, 0x5d,
    0x60, 0x1a, 0xb2, 0x0, 0x4e, 0x20, 0x0, 0x5,
    0xd6, 0x0, 0x0, 0x1b, 0xb1, 0x0, 0x0, 0x5c,
    0x0, 0x0, 0x1,

    /* U+003D "=" */
    0xbd, 0xdd, 0xdd, 0x20, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0xb, 0xdd, 0xdd, 0xd2,

    /* U+003E ">" */
    0x31, 0x0, 0x0, 0x4d, 0x40, 0x0, 0x2, 0xc9,
    0x0, 0x0, 0x7, 0xd4, 0x0, 0x0, 0xaa, 0x0,
    0x2c, 0x90, 0x7, 0xd4, 0x0, 0x7a, 0x10, 0x0,
    0x10, 0x0, 0x0,

    /* U+003F "?" */
    0x8, 0xdf, 0xa1, 0x27, 0x1, 0xc9, 0x0, 0x0,
    0x98, 0x0, 0x6, 0xc1, 0x0, 0x4c, 0x0, 0x0,
    0x65, 0x0, 0x0, 0x0, 0x0, 0x0, 0xdd, 0x0,
    0x0, 0xbb, 0x0,

    /* U+0040 "@" */
    0x0, 0x2b, 0xcc, 0x50, 0x3, 0xd2, 0x0, 0xb2,
    0xb, 0x40, 0x0, 0x48, 0xd, 0x0, 0x3, 0x79,
    0x3a, 0x2, 0xc9, 0x8a, 0x49, 0xb, 0x30, 0x3a,
    0x2b, 0xc, 0x40, 0x9a, 0xd, 0x3, 0xcb, 0x49,
    0x9, 0x50, 0x0, 0x0, 0x1, 0xd4, 0x0, 0x40,
    0x0, 0x19, 0xcc, 0x70,

    /* U+0041 "A" */
    0x0, 0x9, 0xf0, 0x0, 0x0, 0xd, 0x95, 0x0,
    0x0, 0x4a, 0x4b, 0x0, 0x0, 0xa5, 0xe, 0x10,
    0x0, 0xe0, 0x9, 0x60, 0x5, 0xfe, 0xef, 0xb0,
    0xa, 0x50, 0x0, 0xf1, 0xf, 0x10, 0x0, 0xb6,
    0x5c, 0x0, 0x0, 0x6c,

    /* U+0042 "B" */
    0x9f, 0xff, 0xc3, 0x9, 0x80, 0x7, 0xe0, 0x98,
    0x0, 0x1f, 0x19, 0x80, 0x8, 0xa0, 0x9f, 0xff,
    0xd2, 0x9, 0x80, 0x3, 0xd5, 0x98, 0x0, 0x8,
    0x99, 0x80, 0x3, 0xe5, 0x9f, 0xff, 0xd6, 0x0,

    /* U+0043 "C" */
    0x0, 0x2a, 0xee, 0x90, 0x2, 0xf8, 0x11, 0x83,
    0xa, 0x90, 0x0, 0x0, 0xe, 0x30, 0x0, 0x0,
    0xf, 0x20, 0x0, 0x0, 0xe, 0x40, 0x0, 0x0,
    0xb, 0x90, 0x0, 0x0, 0x2, 0xf8, 0x22, 0x87,
    0x0, 0x2b, 0xed, 0x90,

    /* U+0044 "D" */
    0xcf, 0xfd, 0x80, 0xc, 0x50, 0x2b, 0xd0, 0xc5,
    0x0, 0xe, 0x6c, 0x50, 0x0, 0x99, 0xc5, 0x0,
    0x8, 0xbc, 0x50, 0x0, 0x99, 0xc5, 0x0, 0xe,
    0x5c, 0x50, 0x2b, 0xc0, 0xcf, 0xfd, 0x80, 0x0,

    /* U+0045 "E" */
    0x6f, 0xff, 0xff, 0x46, 0xc0, 0x0, 0x0, 0x6c,
    0x0, 0x0, 0x6, 0xc0, 0x0, 0x0, 0x6f, 0xff,
    0xf9, 0x6, 0xc0, 0x0, 0x0, 0x6c, 0x0, 0x0,
    0x6, 0xc0, 0x0, 0x0, 0x6f, 0xff, 0xff, 0x60,

    /* U+0046 "F" */
    0x2f, 0xff, 0xff, 0x72, 0xf0, 0x0, 0x0, 0x2f,
    0x0, 0x0, 0x2, 0xf0, 0x0, 0x0, 0x2f, 0xff,
    0xfc, 0x2, 0xf0, 0x0, 0x0, 0x2f, 0x0, 0x0,
    0x2, 0xf0, 0x0, 0x0, 0x2f, 0x0, 0x0, 0x0,

    /* U+0047 "G" */
    0x0, 0x3b, 0xed, 0x80, 0x4, 0xf6, 0x12, 0x81,
    0xd, 0x70, 0x0, 0x0, 0x1f, 0x10, 0x0, 0x0,
    0x3f, 0x0, 0x7f, 0xf7, 0x1f, 0x10, 0x0, 0x97,
    0xd, 0x60, 0x0, 0x97, 0x5, 0xf6, 0x12, 0xc7,
    0x0, 0x4c, 0xfd, 0x80,

    /* U+0048 "H" */
    0xe4, 0x0, 0xd, 0x4e, 0x40, 0x0, 0xd4, 0xe4,
    0x0, 0xd, 0x4e, 0x40, 0x0, 0xd4, 0xee, 0xee,
    0xef, 0x4e, 0x51, 0x11, 0xe4, 0xe4, 0x0, 0xd,
    0x4e, 0x40, 0x0, 0xd4, 0xe4, 0x0, 0xd, 0x40,

    /* U+0049 "I" */
    0xaf, 0xff, 0xff, 0x0, 0x6, 0xc0, 0x0, 0x0,
    0x6c, 0x0, 0x0, 0x6, 0xc0, 0x0, 0x0, 0x6c,
    0x0, 0x0, 0x6, 0xc0, 0x0, 0x0, 0x6c, 0x0,
    0x0, 0x6, 0xc0, 0x0, 0xaf, 0xff, 0xff, 0x0,

    /* U+004A "J" */
    0x1f, 0xff, 0xfe, 0x0, 0x0, 0x4e, 0x0, 0x0,
    0x4e, 0x0, 0x0, 0x4e, 0x0, 0x0, 0x4e, 0x0,
    0x0, 0x4e, 0x0, 0x0, 0x5d, 0x99, 0x22, 0xc9,
    0x9, 0xee, 0x90,

    /* U+004B "K" */
    0x98, 0x0, 0x1e, 0x50, 0x98, 0x0, 0xc8, 0x0,
    0x98, 0xa, 0xa0, 0x0, 0x98, 0x8f, 0x10, 0x0,
    0x9d, 0xdc, 0x80, 0x0, 0x9e, 0x23, 0xf1, 0x0,
    0x98, 0x0, 0x9a, 0x0, 0x98, 0x0, 0x1f, 0x30,
    0x98, 0x0, 0x7, 0xd0,

    /* U+004C "L" */
    0x1f, 0x0, 0x0, 0x1, 0xf0, 0x0, 0x0, 0x1f,
    0x0, 0x0, 0x1, 0xf0, 0x0, 0x0, 0x1f, 0x0,
    0x0, 0x1, 0xf0, 0x0, 0x0, 0x1f, 0x0, 0x0,
    0x1, 0xf0, 0x0, 0x0, 0x1f, 0xff, 0xff, 0x90,

    /* U+004D "M" */
    0xf9, 0x0, 0x3f, 0x6f, 0xe0, 0x8, 0xe6, 0xfb,
    0x40, 0xdb, 0x6f, 0x69, 0x3b, 0xa6, 0xf2, 0xd8,
    0x5b, 0x6f, 0x1a, 0xe0, 0xb6, 0xf1, 0x36, 0xb,
    0x6f, 0x10, 0x0, 0xb6, 0xf1, 0x0, 0xb, 0x60,

    /* U+004E "N" */
    0xdb, 0x0, 0xd, 0x3d, 0xe3, 0x0, 0xd3, 0xd8,
    0xb0, 0xd, 0x3d, 0x3d, 0x40, 0xd3, 0xd4, 0x5c,
    0xd, 0x3d, 0x40, 0xd4, 0xd3, 0xd4, 0x5, 0xcc,
    0x3d, 0x40, 0xc, 0xe3, 0xd4, 0x0, 0x4f, 0x30,

    /* U+004F "O" */
    0x0, 0x7d, 0xea, 0x10, 0x8, 0xd3, 0x19, 0xd0,
    0xf, 0x40, 0x0, 0xd6, 0x3f, 0x0, 0x0, 0x99,
    0x4e, 0x0, 0x0, 0x7b, 0x3f, 0x0, 0x0, 0x99,
    0xf, 0x40, 0x0, 0xe6, 0x7, 0xd3, 0x19, 0xd0,
    0x0, 0x7d, 0xea, 0x10,

    /* U+0050 "P" */
    0x9f, 0xef, 0xd7, 0x9, 0x90, 0x2, 0xe6, 0x99,
    0x0, 0xa, 0x89, 0x90, 0x3, 0xe4, 0x9f, 0xee,
    0xc5, 0x9, 0x90, 0x0, 0x0, 0x99, 0x0, 0x0,
    0x9, 0x90, 0x0, 0x0, 0x99, 0x0, 0x0, 0x0,

    /* U+0051 "Q" */
    0x0, 0x6d, 0xea, 0x10, 0x6, 0xe3, 0x1a, 0xc0,
    0xe, 0x50, 0x0, 0xe5, 0x2f, 0x0, 0x0, 0x98,
    0x4e, 0x0, 0x0, 0x8a, 0x4e, 0x0, 0x0, 0x8a,
    0x2f, 0x10, 0x0, 0xa8, 0xd, 0x60, 0x0, 0xf3,
    0x4, 0xe5, 0x4c, 0xa0, 0x0, 0x4b, 0xf7, 0x0,
    0x0, 0x0, 0xd8, 0x10, 0x0, 0x0, 0x2b, 0xf9,

    /* U+0052 "R" */
    0x9f, 0xef, 0xd6, 0x9, 0x80, 0x3, 0xe3, 0x98,
    0x0, 0xc, 0x69, 0x80, 0x4, 0xf3, 0x9f, 0xff,
    0xd3, 0x9, 0x80, 0x8c, 0x0, 0x98, 0x0, 0xe5,
    0x9, 0x80, 0x6, 0xd0, 0x98, 0x0, 0xd, 0x70,

    /* U+0053 "S" */
    0x0, 0x7d, 0xfc, 0x50, 0x6, 0xe3, 0x14, 0xa0,
    0x8, 0xa0, 0x0, 0x0, 0x2, 0xe9, 0x20, 0x0,
    0x0, 0x18, 0xeb, 0x20, 0x0, 0x0, 0x7, 0xf3,
    0x0, 0x0, 0x0, 0xc7, 0xc, 0x83, 0x15, 0xf3,
    0x1, 0x9d, 0xfc, 0x50,

    /* U+0054 "T" */
    0x6f, 0xff, 0xff, 0xfc, 0x0, 0x6, 0xc0, 0x0,
    0x0, 0x6, 0xc0, 0x0, 0x0, 0x6, 0xc0, 0x0,
    0x0, 0x6, 0xc0, 0x0, 0x0, 0x6, 0xc0, 0x0,
    0x0, 0x6, 0xc0, 0x0, 0x0, 0x6, 0xc0, 0x0,
    0x0, 0x6, 0xc0, 0x0,

    /* U+0055 "U" */
    0xe4, 0x0, 0xd, 0x4e, 0x40, 0x0, 0xd4, 0xe4,
    0x0, 0xd, 0x4e, 0x40, 0x0, 0xd4, 0xe4, 0x0,
    0xd, 0x4d, 0x40, 0x0, 0xd4, 0xb6, 0x0, 0xf,
    0x26, 0xd3, 0x19, 0xd0, 0x8, 0xef, 0xb2, 0x0,

    /* U+0056 "V" */
    0x3f, 0x0, 0x0, 0x8a, 0xe, 0x40, 0x0, 0xd4,
    0x9, 0x90, 0x2, 0xf0, 0x4, 0xe0, 0x7, 0xa0,
    0x0, 0xe3, 0xc, 0x50, 0x0, 0x98, 0x1f, 0x0,
    0x0, 0x4c, 0x5b, 0x0, 0x0, 0xe, 0xc5, 0x0,
    0x0, 0xa, 0xf1, 0x0,

    /* U+0057 "W" */
    0xc8, 0x0, 0x0, 0xf, 0x39, 0xa0, 0x0, 0x2,
    0xf0, 0x7c, 0x6, 0xe0, 0x4e, 0x4, 0xd0, 0xae,
    0x26, 0xb0, 0x2f, 0xe, 0x96, 0x89, 0x0, 0xf4,
    0xc5, 0xaa, 0x70, 0xd, 0xa8, 0x2e, 0xb4, 0x0,
    0xae, 0x50, 0xee, 0x20, 0x8, 0xf1, 0xa, 0xf0,
    0x0,

    /* U+0058 "X" */
    0xc, 0x70, 0x1, 0xf3, 0x3, 0xf1, 0x9, 0x90,
    0x0, 0x99, 0x2e, 0x10, 0x0, 0x1e, 0xc6, 0x0,
    0x0, 0xa, 0xf1, 0x0, 0x0, 0x3e, 0xa8, 0x0,
    0x0, 0xc6, 0x1f, 0x20, 0x5, 0xd0, 0x8, 0xb0,
    0xe, 0x40, 0x0, 0xe5,

    /* U+0059 "Y" */
    0x3f, 0x10, 0x0, 0x99, 0xa, 0x80, 0x1, 0xf1,
    0x2, 0xf1, 0x9, 0x80, 0x0, 0x99, 0x1e, 0x10,
    0x0, 0x1e, 0xb7, 0x0, 0x0, 0x8, 0xe0, 0x0,
    0x0, 0x6, 0xc0, 0x0, 0x0, 0x6, 0xc0, 0x0,
    0x0, 0x6, 0xc0, 0x0,

    /* U+005A "Z" */
    0xa, 0xff, 0xff, 0xf6, 0x0, 0x0, 0x5, 0xd0,
    0x0, 0x0, 0x1e, 0x30, 0x0, 0x0, 0xb8, 0x0,
    0x0, 0x6, 0xc0, 0x0, 0x0, 0x2e, 0x20, 0x0,
    0x0, 0xc6, 0x0, 0x0, 0x8, 0xb0, 0x0, 0x0,
    0x1f, 0xff, 0xff, 0xf8,

    /* U+005B "[" */
    0xda, 0xaa, 0xd, 0x10, 0x0, 0xd1, 0x0, 0xd,
    0x10, 0x0, 0xd1, 0x0, 0xd, 0x10, 0x0, 0xd1,
    0x0, 0xd, 0x10, 0x0, 0xd1, 0x0, 0xd, 0x10,
    0x0, 0xd1, 0x0, 0xb, 0xaa, 0xa0,

    /* U+005C "\\" */
    0x69, 0x0, 0x0, 0x1, 0xe0, 0x0, 0x0, 0xa,
    0x60, 0x0, 0x0, 0x4c, 0x0, 0x0, 0x0, 0xe2,
    0x0, 0x0, 0x8, 0x80, 0x0, 0x0, 0x2e, 0x0,
    0x0, 0x0, 0xb4, 0x0, 0x0, 0x5, 0xa0, 0x0,
    0x0, 0xe, 0x10, 0x0, 0x0, 0x97, 0x0, 0x0,
    0x3, 0xd0,

    /* U+005D "]" */
    0x6a, 0xae, 0x30, 0x0, 0xa3, 0x0, 0xa, 0x30,
    0x0, 0xa3, 0x0, 0xa, 0x30, 0x0, 0xa3, 0x0,
    0xa, 0x30, 0x0, 0xa3, 0x0, 0xa, 0x30, 0x0,
    0xa3, 0x0, 0xa, 0x36, 0xaa, 0xc3,

    /* U+005E "^" */
    0x0, 0x36, 0x0, 0x0, 0xbe, 0x10, 0x1, 0xd7,
    0x70, 0x7, 0x71, 0xd0, 0xd, 0x10, 0xb4, 0x4b,
    0x0, 0x5a,

    /* U+005F "_" */
    0x2f, 0xff, 0xff, 0xf8,

    /* U+0060 "`" */
    0x57, 0x0, 0x2e, 0x40, 0x2, 0xb0, 0x0, 0x0,

    /* U+0061 "a" */
    0x18, 0xdf, 0xc3, 0x4, 0x61, 0x7, 0xe0, 0x0,
    0x2, 0x5f, 0x21, 0xac, 0x86, 0xf3, 0xa7, 0x0,
    0xe, 0x3c, 0x80, 0x19, 0xf3, 0x3b, 0xec, 0x4c,
    0x30,

    /* U+0062 "b" */
    0xb7, 0x0, 0x0, 0xb, 0x70, 0x0, 0x0, 0xb7,
    0x0, 0x0, 0xb, 0x8a, 0xed, 0x50, 0xbd, 0x40,
    0x5f, 0x2b, 0x70, 0x0, 0xd7, 0xb7, 0x0, 0xa,
    0x8b, 0x70, 0x0, 0xd6, 0xbc, 0x20, 0x7e, 0xb,
    0x7b, 0xfc, 0x20,

    /* U+0063 "c" */
    0x3, 0xbe, 0xd9, 0x3, 0xf6, 0x1, 0x71, 0xa8,
    0x0, 0x0, 0xd, 0x50, 0x0, 0x0, 0xa8, 0x0,
    0x0, 0x3, 0xf5, 0x1, 0x73, 0x4, 0xce, 0xd9,
    0x10,

    /* U+0064 "d" */
    0x0, 0x0, 0x0, 0xf1, 0x0, 0x0, 0x0, 0xf1,
    0x0, 0x0, 0x0, 0xf1, 0x0, 0x8e, 0xd7, 0xf1,
    0x9, 0xc2, 0x7, 0xf1, 0xf, 0x30, 0x0, 0xf1,
    0x1f, 0x10, 0x0, 0xf1, 0xf, 0x30, 0x0, 0xf1,
    0xa, 0xb1, 0x19, 0xf1, 0x1, 0xae, 0xd5, 0xe1,

    /* U+0065 "e" */
    0x0, 0x5d, 0xec, 0x40, 0x6, 0xe2, 0x3, 0xe1,
    0xd, 0x60, 0x0, 0xa6, 0xf, 0xee, 0xee, 0xe7,
    0xd, 0x40, 0x0, 0x0, 0x6, 0xd3, 0x0, 0x50,
    0x0, 0x5c, 0xed, 0x80,

    /* U+0066 "f" */
    0x0, 0x9, 0xee, 0xb0, 0x0, 0x6d, 0x10, 0x20,
    0x0, 0x98, 0x0, 0x0, 0x8e, 0xff, 0xee, 0x50,
    0x0, 0x98, 0x0, 0x0, 0x0, 0x98, 0x0, 0x0,
    0x0, 0x98, 0x0, 0x0, 0x0, 0x98, 0x0, 0x0,
    0x0, 0x98, 0x0, 0x0, 0x0, 0x98, 0x0, 0x0,

    /* U+0067 "g" */
    0x7, 0xcd, 0xff, 0xd5, 0xd0, 0xb, 0x70, 0x89,
    0x0, 0x7a, 0x4, 0xe2, 0x1c, 0x60, 0x1d, 0xbc,
    0x70, 0x7, 0x90, 0x0, 0x0, 0x2f, 0xff, 0xed,
    0x5b, 0x40, 0x0, 0x7d, 0xe4, 0x0, 0x1a, 0x93,
    0xcd, 0xdc, 0x70,

    /* U+0068 "h" */
    0xb7, 0x0, 0x0, 0xb, 0x70, 0x0, 0x0, 0xb7,
    0x0, 0x0, 0xb, 0x68, 0xde, 0x70, 0xbe, 0x61,
    0x6f, 0x1b, 0x70, 0x0, 0xd4, 0xb7, 0x0, 0xd,
    0x5b, 0x70, 0x0, 0xd5, 0xb7, 0x0, 0xd, 0x5b,
    0x70, 0x0, 0xd5,

    /* U+0069 "i" */
    0x0, 0xc, 0x90, 0x0, 0x96, 0x0, 0x0, 0xb,
    0xee, 0xf9, 0x0, 0x9, 0x90, 0x0, 0x99, 0x0,
    0x9, 0x90, 0x0, 0x99, 0x0, 0x9, 0x90, 0x0,
    0x99,

    /* U+006A "j" */
    0x0, 0x0, 0xc9, 0x0, 0x0, 0x96, 0x0, 0x0,
    0x0, 0xb, 0xee, 0xf9, 0x0, 0x0, 0x99, 0x0,
    0x0, 0x99, 0x0, 0x0, 0x99, 0x0, 0x0, 0x99,
    0x0, 0x0, 0x99, 0x0, 0x0, 0x99, 0x0, 0x0,
    0xa8, 0x3, 0x1, 0xe4, 0x1b, 0xee, 0x80,

    /* U+006B "k" */
    0x89, 0x0, 0x0, 0x0, 0x89, 0x0, 0x0, 0x0,
    0x89, 0x0, 0x0, 0x0, 0x89, 0x0, 0x3e, 0x30,
    0x89, 0x3, 0xe3, 0x0, 0x89, 0x3e, 0x40, 0x0,
    0x8c, 0xec, 0x70, 0x0, 0x8f, 0x41, 0xe3, 0x0,
    0x8a, 0x0, 0x5e, 0x10, 0x89, 0x0, 0x9, 0xa0,

    /* U+006C "l" */
    0xee, 0xf9, 0x0, 0x0, 0x8, 0x90, 0x0, 0x0,
    0x89, 0x0, 0x0, 0x8, 0x90, 0x0, 0x0, 0x89,
    0x0, 0x0, 0x8, 0x90, 0x0, 0x0, 0x89, 0x0,
    0x0, 0x8, 0xa0, 0x0, 0x0, 0x6e, 0x11, 0x0,
    0x0, 0xaf, 0xd4,

    /* U+006D "m" */
    0x2c, 0x9e, 0x5a, 0xe4, 0x2f, 0x56, 0xf4, 0x8b,
    0x2f, 0x3, 0xd0, 0x5c, 0x2f, 0x3, 0xd0, 0x5c,
    0x2f, 0x3, 0xd0, 0x5c, 0x2f, 0x3, 0xd0, 0x5c,
    0x2f, 0x3, 0xd0, 0x5c,

    /* U+006E "n" */
    0xb4, 0x8d, 0xe7, 0xb, 0xd6, 0x16, 0xf1, 0xb7,
    0x0, 0xd, 0x4b, 0x70, 0x0, 0xd5, 0xb7, 0x0,
    0xd, 0x5b, 0x70, 0x0, 0xd5, 0xb7, 0x0, 0xd,
    0x50,

    /* U+006F "o" */
    0x0, 0x7d, 0xeb, 0x20, 0x8, 0xc2, 0x7, 0xe0,
    0xf, 0x30, 0x0, 0xd6, 0x1f, 0x10, 0x0, 0xa8,
    0xf, 0x30, 0x0, 0xd6, 0x8, 0xc1, 0x7, 0xe0,
    0x0, 0x7d, 0xeb, 0x20,

    /* U+0070 "p" */
    0xb6, 0xae, 0xd5, 0xb, 0xd4, 0x5, 0xf2, 0xb7,
    0x0, 0xd, 0x7b, 0x70, 0x0, 0xa8, 0xb7, 0x0,
    0xd, 0x6b, 0xc2, 0x7, 0xe0, 0xb9, 0xbf, 0xc2,
    0xb, 0x70, 0x0, 0x0, 0xb7, 0x0, 0x0, 0xb,
    0x70, 0x0, 0x0,

    /* U+0071 "q" */
    0x0, 0x8d, 0xc8, 0xe1, 0x9, 0xb0, 0x4, 0xf1,
    0xf, 0x30, 0x0, 0xf1, 0x1f, 0x10, 0x0, 0xf1,
    0xf, 0x30, 0x0, 0xf1, 0xa, 0xb1, 0x19, 0xf1,
    0x1, 0xae, 0xd5, 0xf1, 0x0, 0x0, 0x0, 0xf1,
    0x0, 0x0, 0x0, 0xf1, 0x0, 0x0, 0x0, 0xf1,

    /* U+0072 "r" */
    0xf0, 0x7c, 0xe5, 0xfa, 0x82, 0x21, 0xf8, 0x0,
    0x0, 0xf2, 0x0, 0x0, 0xf2, 0x0, 0x0, 0xf2,
    0x0, 0x0, 0xf2, 0x0, 0x0,

    /* U+0073 "s" */
    0x9, 0xde, 0xc5, 0x7, 0xc0, 0x2, 0x50, 0x6e,
    0x51, 0x0, 0x0, 0x4a, 0xec, 0x50, 0x0, 0x0,
    0x4f, 0x38, 0x50, 0x2, 0xf3, 0x3a, 0xde, 0xd6,
    0x0,

    /* U+0074 "t" */
    0x0, 0xf, 0x0, 0x0, 0x0, 0xf, 0x0, 0x0,
    0xe, 0xff, 0xfe, 0xe5, 0x0, 0x1f, 0x0, 0x0,
    0x0, 0x1f, 0x0, 0x0, 0x0, 0x1f, 0x0, 0x0,
    0x0, 0x1f, 0x10, 0x0, 0x0, 0xe, 0x70, 0x11,
    0x0, 0x3, 0xdf, 0xd5,

    /* U+0075 "u" */
    0xe3, 0x0, 0x1f, 0xe, 0x30, 0x1, 0xf0, 0xe3,
    0x0, 0x1f, 0xe, 0x30, 0x1, 0xf0, 0xe5, 0x0,
    0x1f, 0xb, 0xb2, 0x3b, 0xf0, 0x2c, 0xfb, 0x2e,
    0x0,

    /* U+0076 "v" */
    0x1f, 0x10, 0x0, 0x97, 0xa, 0x70, 0x0, 0xf1,
    0x4, 0xd0, 0x6, 0xa0, 0x0, 0xd4, 0xc, 0x40,
    0x0, 0x7a, 0x2d, 0x0, 0x0, 0x1e, 0x97, 0x0,
    0x0, 0xa, 0xf1, 0x0,

    /* U+0077 "w" */
    0xc5, 0x3, 0x70, 0xe, 0x29, 0x90, 0x9f, 0x1,
    0xf0, 0x5c, 0xc, 0xa4, 0x4c, 0x2, 0xf1, 0xd7,
    0x77, 0x80, 0xe, 0x6a, 0x3b, 0xa5, 0x0, 0xbc,
    0x60, 0xed, 0x20, 0x8, 0xf3, 0xc, 0xe0, 0x0,

    /* U+0078 "x" */
    0x8, 0xb0, 0x4, 0xe1, 0x0, 0xd6, 0xd, 0x40,
    0x0, 0x2e, 0xa9, 0x0, 0x0, 0xb, 0xf1, 0x0,
    0x0, 0x5d, 0x9a, 0x0, 0x1, 0xe3, 0xd, 0x60,
    0xb, 0x70, 0x2, 0xe2,

    /* U+0079 "y" */
    0x1f, 0x10, 0x0, 0x98, 0xa, 0x70, 0x0, 0xe2,
    0x3, 0xe0, 0x5, 0xb0, 0x0, 0xb5, 0xb, 0x50,
    0x0, 0x4c, 0x1e, 0x0, 0x0, 0xd, 0xa8, 0x0,
    0x0, 0x6, 0xf2, 0x0, 0x0, 0x5, 0xb0, 0x0,
    0x0, 0x3e, 0x20, 0x0, 0xe, 0xd5, 0x0, 0x0,

    /* U+007A "z" */
    0x7e, 0xee, 0xff, 0x30, 0x0, 0xc, 0x80, 0x0,
    0xa, 0xa0, 0x0, 0x8, 0xc0, 0x0, 0x6, 0xd1,
    0x0, 0x4, 0xe2, 0x0, 0x0, 0xef, 0xee, 0xee,
    0x60,

    /* U+007B "{" */
    0x0, 0xa, 0xba, 0x0, 0x5, 0xb0, 0x0, 0x0,
    0x69, 0x0, 0x0, 0x5, 0x90, 0x0, 0x0, 0x5a,
    0x0, 0x0, 0x8, 0x70, 0x0, 0x3e, 0xc1, 0x0,
    0x0, 0x7, 0x80, 0x0, 0x0, 0x59, 0x0, 0x0,
    0x6, 0x90, 0x0, 0x0, 0x5b, 0x0, 0x0, 0x0,
    0x9b, 0xa0,

    /* U+007C "|" */
    0x4b, 0x4b, 0x4b, 0x4b, 0x4b, 0x4b, 0x4b, 0x4b,
    0x4b, 0x4b, 0x4b, 0x4b, 0x4b, 0x4b,

    /* U+007D "}" */
    0x6a, 0xc3, 0x0, 0x0, 0x5b, 0x0, 0x0, 0x2c,
    0x0, 0x0, 0x3b, 0x0, 0x0, 0x3b, 0x0, 0x0,
    0x1d, 0x10, 0x0, 0x7, 0xf9, 0x0, 0x2d, 0x0,
    0x0, 0x3b, 0x0, 0x0, 0x2c, 0x0, 0x0, 0x5b,
    0x0, 0x6a, 0xa3, 0x0,

    /* U+007E "~" */
    0x3c, 0xb5, 0xa, 0x2a, 0x23, 0x9d, 0x70
};


/*---------------------
 *  GLYPH DESCRIPTION
 *--------------------*/

static const lv_font_fmt_txt_glyph_dsc_t glyph_dsc[] = {
    {.bitmap_index = 0, .adv_w = 0, .box_w = 0, .box_h = 0, .ofs_x = 0, .ofs_y = 0} /* id = 0 reserved */,
    {.bitmap_index = 0, .adv_w = 134, .box_w = 0, .box_h = 0, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 0, .adv_w = 134, .box_w = 3, .box_h = 9, .ofs_x = 3, .ofs_y = 0},
    {.bitmap_index = 14, .adv_w = 134, .box_w = 6, .box_h = 5, .ofs_x = 1, .ofs_y = 5},
    {.bitmap_index = 29, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 61, .adv_w = 134, .box_w = 7, .box_h = 12, .ofs_x = 1, .ofs_y = -2},
    {.bitmap_index = 103, .adv_w = 134, .box_w = 9, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 144, .adv_w = 134, .box_w = 9, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 185, .adv_w = 134, .box_w = 2, .box_h = 5, .ofs_x = 3, .ofs_y = 5},
    {.bitmap_index = 190, .adv_w = 134, .box_w = 5, .box_h = 14, .ofs_x = 2, .ofs_y = -3},
    {.bitmap_index = 225, .adv_w = 134, .box_w = 5, .box_h = 14, .ofs_x = 1, .ofs_y = -3},
    {.bitmap_index = 260, .adv_w = 134, .box_w = 7, .box_h = 7, .ofs_x = 1, .ofs_y = 1},
    {.bitmap_index = 285, .adv_w = 134, .box_w = 7, .box_h = 7, .ofs_x = 1, .ofs_y = 1},
    {.bitmap_index = 310, .adv_w = 134, .box_w = 4, .box_h = 7, .ofs_x = 2, .ofs_y = -4},
    {.bitmap_index = 324, .adv_w = 134, .box_w = 7, .box_h = 1, .ofs_x = 1, .ofs_y = 4},
    {.bitmap_index = 328, .adv_w = 134, .box_w = 3, .box_h = 3, .ofs_x = 3, .ofs_y = 0},
    {.bitmap_index = 333, .adv_w = 134, .box_w = 7, .box_h = 12, .ofs_x = 1, .ofs_y = -2},
    {.bitmap_index = 375, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 407, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 439, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 475, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 511, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 547, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 583, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 615, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 651, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 687, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 723, .adv_w = 134, .box_w = 3, .box_h = 7, .ofs_x = 3, .ofs_y = 0},
    {.bitmap_index = 734, .adv_w = 134, .box_w = 4, .box_h = 11, .ofs_x = 2, .ofs_y = -4},
    {.bitmap_index = 756, .adv_w = 134, .box_w = 6, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 783, .adv_w = 134, .box_w = 7, .box_h = 4, .ofs_x = 1, .ofs_y = 3},
    {.bitmap_index = 797, .adv_w = 134, .box_w = 6, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 824, .adv_w = 134, .box_w = 6, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 851, .adv_w = 134, .box_w = 8, .box_h = 11, .ofs_x = 0, .ofs_y = -2},
    {.bitmap_index = 895, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 931, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 963, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 999, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1031, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1063, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1095, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1131, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1163, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1195, .adv_w = 134, .box_w = 6, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1222, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1258, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1290, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1322, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1354, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1390, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1422, .adv_w = 134, .box_w = 8, .box_h = 12, .ofs_x = 0, .ofs_y = -3},
    {.bitmap_index = 1470, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1502, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1538, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1574, .adv_w = 134, .box_w = 7, .box_h = 9, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1606, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1642, .adv_w = 134, .box_w = 9, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1683, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1719, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1755, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 1791, .adv_w = 134, .box_w = 5, .box_h = 12, .ofs_x = 3, .ofs_y = -2},
    {.bitmap_index = 1821, .adv_w = 134, .box_w = 7, .box_h = 12, .ofs_x = 1, .ofs_y = -2},
    {.bitmap_index = 1863, .adv_w = 134, .box_w = 5, .box_h = 12, .ofs_x = 1, .ofs_y = -2},
    {.bitmap_index = 1893, .adv_w = 134, .box_w = 6, .box_h = 6, .ofs_x = 1, .ofs_y = 4},
    {.bitmap_index = 1911, .adv_w = 134, .box_w = 8, .box_h = 1, .ofs_x = 0, .ofs_y = -2},
    {.bitmap_index = 1915, .adv_w = 134, .box_w = 4, .box_h = 4, .ofs_x = 2, .ofs_y = 7},
    {.bitmap_index = 1923, .adv_w = 134, .box_w = 7, .box_h = 7, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1948, .adv_w = 134, .box_w = 7, .box_h = 10, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 1983, .adv_w = 134, .box_w = 7, .box_h = 7, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2008, .adv_w = 134, .box_w = 8, .box_h = 10, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 2048, .adv_w = 134, .box_w = 8, .box_h = 7, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 2076, .adv_w = 134, .box_w = 8, .box_h = 10, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2116, .adv_w = 134, .box_w = 7, .box_h = 10, .ofs_x = 1, .ofs_y = -3},
    {.bitmap_index = 2151, .adv_w = 134, .box_w = 7, .box_h = 10, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2186, .adv_w = 134, .box_w = 5, .box_h = 10, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2211, .adv_w = 134, .box_w = 6, .box_h = 13, .ofs_x = 0, .ofs_y = -3},
    {.bitmap_index = 2250, .adv_w = 134, .box_w = 8, .box_h = 10, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2290, .adv_w = 134, .box_w = 7, .box_h = 10, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2325, .adv_w = 134, .box_w = 8, .box_h = 7, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 2353, .adv_w = 134, .box_w = 7, .box_h = 7, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2378, .adv_w = 134, .box_w = 8, .box_h = 7, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 2406, .adv_w = 134, .box_w = 7, .box_h = 10, .ofs_x = 1, .ofs_y = -3},
    {.bitmap_index = 2441, .adv_w = 134, .box_w = 8, .box_h = 10, .ofs_x = 0, .ofs_y = -3},
    {.bitmap_index = 2481, .adv_w = 134, .box_w = 6, .box_h = 7, .ofs_x = 2, .ofs_y = 0},
    {.bitmap_index = 2502, .adv_w = 134, .box_w = 7, .box_h = 7, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2527, .adv_w = 134, .box_w = 8, .box_h = 9, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 2563, .adv_w = 134, .box_w = 7, .box_h = 7, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2588, .adv_w = 134, .box_w = 8, .box_h = 7, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 2616, .adv_w = 134, .box_w = 9, .box_h = 7, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 2648, .adv_w = 134, .box_w = 8, .box_h = 7, .ofs_x = 0, .ofs_y = 0},
    {.bitmap_index = 2676, .adv_w = 134, .box_w = 8, .box_h = 10, .ofs_x = 0, .ofs_y = -3},
    {.bitmap_index = 2716, .adv_w = 134, .box_w = 7, .box_h = 7, .ofs_x = 1, .ofs_y = 0},
    {.bitmap_index = 2741, .adv_w = 134, .box_w = 7, .box_h = 12, .ofs_x = 1, .ofs_y = -2},
    {.bitmap_index = 2783, .adv_w = 134, .box_w = 2, .box_h = 14, .ofs_x = 3, .ofs_y = -4},
    {.bitmap_index = 2797, .adv_w = 134, .box_w = 6, .box_h = 12, .ofs_x = 1, .ofs_y = -2},
    {.bitmap_index = 2833, .adv_w = 134, .box_w = 7, .box_h = 2, .ofs_x = 1, .ofs_y = 4}
};

/*---------------------
 *  CHARACTER MAPPING
 *--------------------*/



/*Collect the unicode lists and glyph_id offsets*/
static const lv_font_fmt_txt_cmap_t cmaps[] =
{
    {
        .range_start = 32, .range_length = 95, .glyph_id_start = 1,
        .unicode_list = NULL, .glyph_id_ofs_list = NULL, .list_length = 0, .type = LV_FONT_FMT_TXT_CMAP_FORMAT0_TINY
    }
};



/*--------------------
 *  ALL CUSTOM DATA
 *--------------------*/

#if LV_VERSION_CHECK(8, 0, 0)
/*Store all the custom data of the font*/
static  lv_font_fmt_txt_glyph_cache_t cache;
static const lv_font_fmt_txt_dsc_t font_dsc = {
#else
static lv_font_fmt_txt_dsc_t font_dsc = {
#endif
    .glyph_bitmap = glyph_bitmap,
    .glyph_dsc = glyph_dsc,
    .cmaps = cmaps,
    .kern_dsc = NULL,
    .kern_scale = 0,
    .cmap_num = 1,
    .bpp = 4,
    .kern_classes = 0,
    .bitmap_format = 0,
#if LV_VERSION_CHECK(8, 0, 0)
    .cache = &cache
#endif
};


/*-----------------
 *  PUBLIC FONT
 *----------------*/

/*Initialize a public general font descriptor*/
#if LV_VERSION_CHECK(8, 0, 0)
const lv_font_t scp_reg_14 = {
#else
lv_font_t scp_reg_14 = {
#endif
    .get_glyph_dsc = lv_font_get_glyph_dsc_fmt_txt,    /*Function pointer to get glyph's data*/
    .get_glyph_bitmap = lv_font_get_bitmap_fmt_txt,    /*Function pointer to get glyph's bitmap*/
    .line_height = 15,          /*The maximum line height required by the font*/
    .base_line = 4,             /*Baseline measured from the bottom of the line*/
#if !(LVGL_VERSION_MAJOR == 6 && LVGL_VERSION_MINOR == 0)
    .subpx = LV_FONT_SUBPX_NONE,
#endif
#if LV_VERSION_CHECK(7, 4, 0) || LVGL_VERSION_MAJOR >= 8
    .underline_position = -1,
    .underline_thickness = 1,
#endif
    .dsc = &font_dsc           /*The custom font data. Will be accessed by `get_glyph_bitmap/dsc` */
};



#endif /*#if SCP_REG_14*/
