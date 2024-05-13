/*! \file main.c
 *
 * \author John Reppy
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 */

/*
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#include "mini-ml.h"
#include <inttypes.h>
#include <string.h>

// the main program provided by the MiniML compiler
extern MML_word_t _mml_entry (MML_word_t args);

int main (int argc, const char **argv)
{
  /* initialize the heap */
    _mml_init_heap ();

  // build the list of command-line arguments; we assume that there
  // is adequate heap space
    MML_word_t args = MML_Nil;
    for (int i = argc - 1;  i > 0;  --i) {
        int argLen = strlen(argv[i]);
      // allocate the string
        MML_string_t *s = MML_AllocString (argLen);
        strncpy (s->_data, argv[i], argLen);
      // allocate the list cell
        args = MML_Cons((MML_word_t)s, args);
    }

  /* run the program */
    printf("## Running the program\n");
    _mml_entry (args);
    printf("## finished\n");

    return 0;

}
