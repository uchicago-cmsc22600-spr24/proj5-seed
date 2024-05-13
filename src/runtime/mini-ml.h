/*! \file mini-ml.h
 *
 * \author John Reppy
 *
 * Sample code
 * CMSC 22600
 * Autumn 2024
 * University of Chicago
 */

/*
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#ifndef _MINI_ML_H_
#define _MINI_ML_H_

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

// A machine word can hold either a primitive value (nil, int, or bool), or a pointer
typedef struct _mml_word_ *MML_word_t;

/***** MiniML Bool type *****/

#define MML_true       ((MML_word_t)3)
#define MML_false      ((MML_word_t)1)

/***** MiniML Int type *****/

static inline MML_word_t MML_fromInt (int64_t n)
{
    return (MML_word_t)((n << 1) | 1);
}

static inline int64_t MML_toInt (MML_word_t n)
{
    return ((int64_t)n) >> 1;
}

/***** MiniML tuple types *****/

typedef MML_word_t    *MML_tuple_t;

MML_word_t _mml_alloc (int64_t nw);

/***** MiniML String type *****/

typedef struct {
    MML_word_t  _len;           // number of characters as tagged int
    char        _data[8];       // character data
} MML_string_t;

MML_string_t *MML_AllocString (int len);

/***** MiniML List type *****/
#define MML_Nil       ((MML_word_t)1)

// LANGF cons (assuming that we have space)
//
static inline MML_word_t MML_Cons (MML_word_t hd, MML_word_t tl)
{
    MML_word_t *cons = (MML_word_t *)_mml_alloc(2);
    cons[0] = hd;
    cons[1] = tl;
    return (MML_word_t)cons;
}

/***** GC API *****/

void _mml_invoke_gc (MML_word_t *sp, size_t allocSz, uint32_t numRoots, MML_word_t *roots);
void _mml_init_heap ();

typedef struct {
    uint64_t    _baseAddr;              // base address of semispace
    uint64_t    _usedTop;               // top of used region of semispace
    uint64_t    _szB;                   // total size of semispace
} MML_semispace_t;

typedef struct {
    MML_word_t         *_allocPtr;     // next word to allocate in to-space
    MML_word_t         *_limitPtr;     // top of to-space (_toSp->baseAddr + _toSp->szB)
    MML_semispace_t    *_toSp;         // current to-space
    MML_semispace_t    *_fromSp;       // current from-space
} MML_heap_t;

extern MML_heap_t _mml_heap;

static inline bool isPtr (MML_word_t w)
{
    return (((uint64_t)w & 0x7) == 0);
}

static inline bool isInt (MML_word_t w)
{
    return (((uint64_t)w & 0x1) == 0x1);
}

/* we use the following tagging scheme in the low 3 bits of heap words:
 *
 *      000             -- 8-byte aligned pointer
 *      010             -- tuple
 *      100             -- forward pointer (in object-header slot)
 *      110             -- string object header
 *      xx1             -- tagged integer
 */
#define TAG_MASK        0x7             /* mask off low 3 bits */
#define TAG_FWDPTR      0x4             /* tag for forward pointers */
#define TAG_TUPLE       0x2
#define TAG_STRING      0x6

/* make a tuple-object header */
static inline MML_word_t MML_make_tuple_header (int n)
{
    return (MML_word_t)(((uint64_t)n << 3) | TAG_TUPLE);
}

/* make a string-object header, where `n` is the number of characters in the string */
static inline MML_word_t MML_make_string_header (int n)
{
  /* size of string object includes length field, data, zero terminator,
   * and padding (but does not include object header).
   */
    uint64_t nw = ((n + 8) >> 3) + 1;
    return (MML_word_t)(((uint64_t)nw << 3) | TAG_STRING);
}

/* extract and return the number of words in an object from its header */
static inline uint64_t MML_get_size_from_header (uint64_t hdr)
{
    return (hdr >> 3);
}

#endif // !_MINI_ML_H_
