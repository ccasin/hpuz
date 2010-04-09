/******************************************************************
 * libpuz - A .PUZ crossword library
 * Copyright(c) 2006 Josh Myer <josh@joshisanerd.com>
 * 
 * This code is released under the terms of the GNU General Public
 * License version 2 or later.  You should have receieved a copy as
 * along with this source as the file COPYING.
 ******************************************************************/

/*
 * puzzle.c -- Implements the puzzle accessor routines
 */



#include <puz.h>
#include <math.h>

/**
 * puz_init - initialize a puzzle
 *
 * @puz: pointer to the struct puzzle_t to init.  If NULL, one will be malloc'd for you.
 *
 * This function is used to initialize a new struct puzzle_t to sane defaults.
 *
 * Return Value: NULL on error, else a pointer to the filled-in struct
 * puzzle_t.  If puz was NULL, this is a pointer to the
 * newly-allocated structure.
 */
struct puzzle_t *puz_init(struct puzzle_t *puz) {
  int didmalloc;

  unsigned char file_magic[12] = FILE_MAGIC;
  unsigned char magic_18[4] = VER_MAGIC;

 if(NULL == puz) {
    puz = (struct puzzle_t *)malloc(sizeof(struct puzzle_t));
    if(NULL == puz) {
      perror("malloc");
      return NULL;
    }
    didmalloc = 1;
  }

  memset(puz, 0, sizeof(struct puzzle_t));

  memcpy(puz->header.magic, file_magic, 12);
  memcpy(puz->header.magic_18, magic_18, 4);
  puz->header.x_unk_30 = 0x0001;

  return puz;
}

/**
 * puz_size - calculate the size of a puzzle as a PUZ file
 *
 * @puz: a pointer to the struct puzzle_t to size
 *
 * This function is used to calculate the size of the PUZ file for a
 * given puzzle.
 *
 * Return value: -1 on error, a positive (non-zero) integer size on
 * success.
 */
int puz_size(struct puzzle_t *puz) {
  int i, sz;

  if(!puz)
    return -1;

  sz = 0x34; // header
  sz += puz_width_get(puz) * puz_height_get(puz); // solution
  sz += puz_width_get(puz) * puz_height_get(puz); // grid
  sz += Sstrlen(puz_title_get(puz)) + 1;  // title
  sz += Sstrlen(puz_author_get(puz)) + 1; // author
  sz += Sstrlen(puz_copyright_get(puz)) + 1; // copyright

  for(i = 0; i < puz_clue_count_get(puz); i++) {
    sz += Sstrlen(puz_clue_get(puz, i)) + 1;
  }

  if(puz->notes_sz)
    sz += puz->notes_sz;
  sz += 1; // NULL terminated

  if (puz_has_rebus(puz)) {
    sz += 4; // GRBS
    sz += 2; // board size
    sz += 2; // checksum
    sz += puz_width_get(puz) * puz_height_get(puz); // rebus grid
    sz += 1; // NULL

    sz += 4; // RTBL
    sz += 2; // size
    sz += 2; // checksum

    for (i = 0; i < puz_rebus_count_get(puz); i++) {
      sz += Sstrlen(puz_rtbl_get(puz, i)) + 1;
    }
    sz += 1; // NULL
  }

  if (puz_has_timer(puz)) {
    sz += 4; //LTIM
    sz += 4; // size and checksum
    
    sz += Sstrlen(puz->ltim); //timer string

    sz += 1; // NULL
  }


  if (puz_has_extras(puz)) {
    sz += 4; // GEXT
    sz += 2; // size
    sz += 2; // checksum
    
    sz += puz_width_get(puz) * puz_height_get(puz); // extras grid
    sz += 1; // NULL
  }

  return sz;
}


/**
 * puz_width_get - get the puzzle's width
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns -1 on error; else a non-negative value
 */
int puz_width_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return -1;

  return(puz->header.width);
}

/**
 * puz_width_set - set the puzzle's width
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: the (positive) value to set width to (required)
 * 
 * returns -1 on error, else the old value of width
 */
int puz_width_set(struct puzzle_t *puz, unsigned char val) {
  int i;

  if(NULL == puz)
    return -1;

  i = puz->header.width;

  puz->header.width = val;

  return(i);
}

/**
 * puz_height_get - get the puzzle's height
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns -1 on error; else a non-negative value
 */
int puz_height_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return -1;

  return(puz->header.height);
}

/**
 * puz_height_set - set the puzzle's height
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: the (positive) value to set height to (required)
 * 
 * returns -1 on error, else the old value of height
 */
int puz_height_set(struct puzzle_t *puz, unsigned char val) {
  int i;

  if(NULL == puz)
    return -1;

  i = puz->header.height;

  puz->header.height = val;

  return(i);
}

/**
 * puz_solution_get - get the puzzle's solution
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error or if field is unset.
 */
unsigned char * puz_solution_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return NULL;

  return(puz->solution);
}

/**
 * puz_solution_set - set the puzzle's solution
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: a pointer to the string to st the value to (required)
 * 
 * returns NULL on error, else a pointer to the struct's copy of the string
 */
unsigned char * puz_solution_set(struct puzzle_t *puz, unsigned char *val) {
  if(NULL == puz || NULL == val)
    return NULL;

  free(puz->solution);

  puz->solution = Sstrdup(val);

  return puz->solution;
}


/**
 * puz_grid_get - get the puzzle's grid
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error or if field is unset.
 */
unsigned char * puz_grid_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return NULL;

  return(puz->grid);
}

/**
 * puz_grid_set - set the puzzle's grid
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: a pointer to the string to st the value to (required)
 * 
 * returns NULL on error, else a pointer to the struct's copy of the string
 */
unsigned char * puz_grid_set(struct puzzle_t *puz, unsigned char *val) {
  if(NULL == puz || NULL == val)
    return NULL;

  free(puz->grid);

  puz->grid = Sstrdup(val);

  return puz->grid;
}


/**
 * puz_title_get - get the puzzle's title
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error or if field is unset.
 */
unsigned char * puz_title_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return NULL;

  return(puz->title);
}

/**
 * puz_title_set - set the puzzle's title
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: a pointer to the string to st the value to (required)
 * 
 * returns NULL on error, else a pointer to the struct's copy of the string
 */
unsigned char * puz_title_set(struct puzzle_t *puz, unsigned char *val) {
  if(NULL == puz || NULL == val)
    return NULL;

  free(puz->title);

  puz->title = Sstrdup(val);

  return puz->title;
}


/**
 * puz_author_get - get the puzzle's author
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error or if field is unset.
 */
unsigned char * puz_author_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return NULL;

  return(puz->author);
}

/**
 * puz_author_set - set the puzzle's author
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: a pointer to the string to st the value to (required)
 * 
 * returns NULL on error, else a pointer to the struct's copy of the string
 */
unsigned char * puz_author_set(struct puzzle_t *puz, unsigned char *val) {
  if(NULL == puz || NULL == val)
    return NULL;

  free(puz->author);

  puz->author = Sstrdup(val);

  return puz->author;
}


/**
 * puz_copyright_get - get the puzzle's copyright
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error or if field is unset.
 */
unsigned char * puz_copyright_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return NULL;

  return(puz->copyright);
}

/**
 * puz_copyright_set - set the puzzle's copyright
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: a pointer to the string to st the value to (required)
 * 
 * returns NULL on error, else a pointer to the struct's copy of the string
 */
unsigned char * puz_copyright_set(struct puzzle_t *puz, unsigned char *val) {
  if(NULL == puz || NULL == val)
    return NULL;

  free(puz->copyright);

  puz->copyright = Sstrdup(val);

  return puz->copyright;
}


/**
 * puz_notes_get - get the puzzle's notes
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error or if field is unset.
 */
unsigned char * puz_notes_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return NULL;

  return(puz->notes);
}

/**
 * puz_notes_set - set the puzzle's notes
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: a pointer to the string to st the value to (required)
 * 
 * returns NULL on error, else a pointer to the struct's copy of the string
 */
unsigned char * puz_notes_set(struct puzzle_t *puz, unsigned char *val) {
  if(NULL == puz || NULL == val)
    return NULL;

  free(puz->notes);

  puz->notes = Sstrdup(val);

  return puz->notes;
}





/**
 * puz_clue_count_get - get the puzzle's clue_count
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns -1 on error; else a non-negative value
 */
int puz_clue_count_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return -1;

  return(puz->header.clue_count);
}

/**
 * puz_clue_count_set - set the puzzle's clue_count
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: the (positive) value to set clue_count to (required)
 * 

 * This function can only be used to set the number of clues for a
 * blank puzzle.  If a puzzle has been filled in and you want to set
 * the number of clues to a different value, you'll need to first
 * clear the clues with puz_clear_clues(), then set the number of
 * clues, and finally fill the clues back in.
 *
 * returns -1 on error, 0 on success.
 */
int puz_clue_count_set(struct puzzle_t *puz, int val) {
  if(NULL == puz || 0 > val)
    return -1;

  if(puz->header.clue_count != 0)
    return -1;

  puz->clues = (unsigned char **)malloc(val * sizeof(unsigned char *));
  memset(puz->clues, 0, val * sizeof(unsigned char *));

  puz->header.clue_count = val;

  return(0);
}

/**
 * puz_clear_clues - clear out a puzzle's clues
 *
 * @puz: a pointer to the struct puzzle_t to clear clues from (required)
 *
 * This function clears out the clues.  Specifically, it frees all
 * clues, the clues storage, and sets the number of clues to zero.
 *
 * Note that any pointers you have to clues will become invalid after
 * calling this function.
 *
 * Returns -1 on error, 0 on success.
 */
int puz_clear_clues(struct puzzle_t *puz) {
  int i;

  if(NULL == puz)
    return -1;

  for(i = 0; i < puz->header.clue_count; i++)
    free(puz->clues[i]);
  
  free(puz->clues);

  puz->clues = NULL;
  puz->header.clue_count = 0;

  return 0;
}

/**
 * puz_clue_get -- get the nth clue of a puzzle
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 * @n: the index (between zero and n_clues) to get
 *
 * Returns NULL on error, pointer to the nth clue on success.
 */
unsigned char * puz_clue_get(struct puzzle_t *puz, int n) {
  if(NULL == puz || n < 0 || n > puz->header.clue_count)
    return NULL;

  return puz->clues[n];
}


/**
 * puz_clue_set -- set the nth clue of a puzzle
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 * @n: the index (between zero and n_clues) to get
 * @val: the value to set it to
 *
 * Returns NULL on error, pointer to the puzzle's new copy on success.
 */
unsigned char * puz_clue_set(struct puzzle_t *puz, int n, unsigned char * val) {
  if(NULL == puz || n < 0 || n > puz->header.clue_count || NULL == val)
    return NULL;

  free(puz->clues[n]);
  puz->clues[n] = Sstrdup(val);

  return puz->clues[n];
}

/**
 * puz_has_rebus -- checks if a puzzle has rebuses
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns 1 if the puzzle has a rebus, 0 if not or if the puzzle is NULL
 */
int puz_has_rebus(struct puzzle_t *puz) {
  if(NULL == puz)
    return 0;

  return NULL != puz->grbs;
}

/**
 * puz_rebus_get - get the puzzle's rebus grid
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error or if field is unset.
 */
unsigned char * puz_rebus_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return NULL;

  return puz->grbs;
}

/**
 * puz_rebus_set - set the puzzle's rebus grid
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: a pointer to the string to st the value to (required)
 * 
 * returns NULL on error, else a pointer to the struct's copy of the string
 */
unsigned char * puz_rebus_set(struct puzzle_t *puz, unsigned char *val) {
  if(NULL == puz || NULL == val)
    return NULL;

  free(puz->grbs);

  int size = puz_width_get(puz) * puz_height_get(puz);
  puz->grbs = calloc(size+1, sizeof (unsigned char));
  memcpy(puz->grbs, val, size);
  puz->grbs[size] = 0;

  return puz->grbs;
}

/**
 * puz_rebus_count_get - get the puzzle's rebus count
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns -1 on error; else a non-negative value
 */
int puz_rebus_count_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return -1;

  if (!puz_has_rebus(puz))
    return 0;

  return puz->rtbl_sz;
}

/**
 * puz_rebus_count_set - set the puzzle's clue_count
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: the (positive) value to set rtbl_sz to (required)
 * 
 * returns -1 on error, 0 on success.
 */
int puz_rebus_count_set(struct puzzle_t *puz, int val) {
  if(NULL == puz || 0 > val)
    return -1;

  puz->rtbl = (unsigned char **)malloc(val * sizeof(unsigned char *));
  // TODO should check for malloc failure
  memset(puz->rtbl, 0, val * sizeof(unsigned char *));

  puz->rtbl_sz = val;

  return(0);
}

/**
 * puz_rtbl_get -- get the nth entry of the rebus table
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 * @n: the index (between zero and rtbl_sz) to get
 *
 * Note that the numbering used here is for the list of rebus table
 * entries, and has NOTHING to do with the numeric keys in the table.
 *
 * Returns NULL on error, pointer to the nth clue on success.
 */
unsigned char * puz_rtbl_get(struct puzzle_t *puz, int n) {
  if(NULL == puz || n < 0 || !puz_has_rebus(puz) || n > puz->rtbl_sz)
    return NULL;

  return puz->rtbl[n];
}


/**
 * puz_rtbl_set -- set the nth rtbl of a puzzle
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 * @n: the index (between zero and rtbl_sz) to get
 * @val: the value to set it to
 *
 * Returns NULL on error, pointer to the puzzle's new copy on success.
 */
unsigned char * puz_rtbl_set(struct puzzle_t *puz, int n, unsigned char * val) {
  if(NULL == puz || n < 0 || n > puz->header.clue_count || NULL == val)
    return NULL;

  free(puz->rtbl[n]);
  puz->rtbl[n] = Sstrdup(val);

  return puz->rtbl[n];
}

/**
 * puz_rtblstr_get -- get the rebus table as a single string
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error, newly allocated string on success
 */
unsigned char * puz_rtblstr_get(struct puzzle_t *puz) {
  int rtbl_strsz, sz, i;
  unsigned char *rtbl_str;
  unsigned char *c;

  if (NULL == puz)
    return NULL;

  rtbl_strsz = 1; // account for NULL
  for (i = 0; i < puz->rtbl_sz; i++) {
    rtbl_strsz += Sstrlen(puz->rtbl[i]) + 1; // account for semi-colon
  }

  rtbl_str = (unsigned char *)malloc(rtbl_strsz * sizeof(unsigned char));
  if (NULL == rtbl_str) {
    perror("malloc");
    return NULL;
  }

  c = rtbl_str;
  for (i = 0; i < puz->rtbl_sz; i++) {
    sz = Sstrlen(puz->rtbl[i]);

    memcpy(c, puz->rtbl[i], sz);
    c[sz] = ';';
    c += sz+1;
  }

  rtbl_str[rtbl_strsz - 1] = '\0';

  return rtbl_str;
}

/**
 * puz_rtblstr_set -- set the rebus table as a single string
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 * @val: the rebus table as a string (required)
 *
 * Returns NULL on error, well-indexed rebus table on success
 */
unsigned char ** puz_rtblstr_set(struct puzzle_t *puz, unsigned char * val) {
  int i, sz;
  unsigned char *start;
  unsigned char *end;

  if (NULL == puz)
    return NULL;

  sz = 0;
  for (i = 0; i < Sstrlen(val); i++) {
    if (';' == val[i])
      sz += 1;
  }
  puz_rebus_count_set(puz, sz); // allocates RTBL, sets rtbl_sz

  start = val;
  for (i = 0; i < puz->rtbl_sz; i++) {
    end = Sstrchr(start, ';'); // XXX this is fragile...bad input will hose this

    if (NULL == end) {
      printf("Appear to have run out of rebus table entries: rebuses: %d, i: %d\n",
             puz->rtbl_sz, i);

      return NULL; /* XXX cleanup */
    }

    puz->rtbl[i] = (unsigned char*) malloc((end - start) + 1);
    Sstrncpy(puz->rtbl[i],start,end-start);
    (puz->rtbl[i])[end-start] = 0;
    start = end+1;
  }

  return puz->rtbl;
}

/**
 * puz_has_timer -- checks if a puzzle has time data
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns 1 if the puzzle has a timer, 0 if not or if the puzzle is NULL
 */
int puz_has_timer(struct puzzle_t *puz) {
  if (NULL == puz)
    return 0;

  return NULL != puz->ltim;
}

/**
 * puz_timer_elapsed_get - get the puzzle's elapsed time
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns -1 on error or if field is unset.
 */
int puz_timer_elapsed_get(struct puzzle_t *puz) {
  int checked, elapsed;

  if(NULL == puz)
    return -1;
  if(NULL == puz->ltim)
    return -1;

  checked = sscanf((char*)puz->ltim,"%d,",&elapsed);
  if (checked == 1) {
    return elapsed;
  } else {
    printf("Puzzle has ill-formed timer section, setting time to 0\n");
    return 0;
  }
}

/**
 * puz_timer_stopped_get - get the puzzle timer's stopped value.
 * This is usually 0 if the timer if running and 1 if it is stopped.
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns -1 on error or if field is unset.
 */
int puz_timer_stopped_get(struct puzzle_t *puz) {
  int checked, elapsed, stopped;

  if(NULL == puz)
    return -1;
  if(NULL == puz->ltim)
    return -1;

  checked = sscanf((char*)puz->ltim,"%d,%d",&elapsed,&stopped);
  if (checked == 2) {
    return stopped;
  } else {
    printf("Warning: puzzle has ill-formed timer section\n");
    return 1;
  }
}


/**
 * puz_timer_set - set the puzzle's extras time data
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @elapsed: the number of seconds elapsed (should be non-negative) (required)
 * @stopped: 1 if the timer is stopped, 0 if it is running
 * returns NULL on error, else a pointer to the puz's copy of the timer_data
 */
unsigned char * puz_timer_set(struct puzzle_t *puz, int elapsed, 
                              int stopped) 
{ int size;
  
  if(NULL == puz)
    return NULL;

  free(puz->ltim);

  //we need to calculate the ltim's size as a string.  This is a little
  //tricky.  The int is represented as a string of ASCII digits, so we
  //use log10, but add an extra byte in case we're a little too small
  //because of floating point errors.
  
  size = 5+(int)(floor(log10(elapsed)));
  puz->ltim = (unsigned char*)calloc(size,sizeof(unsigned char));
  if (puz->ltim) {
    snprintf((char*)puz->ltim, size, "%d,%d", elapsed, stopped);
  }
  puz->ltim[size-1] = 0;

  return puz->ltim;
}

/**
 * puz_has_extras -- checks if a puzzle has extras
 * 
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns 1 if the puzzle has a extras, 0 if not or if the puzzle is NULL
 */
int puz_has_extras(struct puzzle_t *puz) {
  if(NULL == puz)
    return 0;

  return NULL != puz->gext;
}

/**
 * puz_extras_get - get the puzzle's extras grid
 *
 * @puz: a pointer to the struct puzzle_t to read from (required)
 *
 * Returns NULL on error or if field is unset.
 */
unsigned char * puz_extras_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return NULL;

  return puz->gext;
}

/**
 * puz_extras_set - set the puzzle's extras grid
 *
 * @puz: a pointer to the struct puzzle_t to write to (required)
 * @val: a pointer to the string to st the value to (required)
 * 
 * returns NULL on error, else a pointer to the struct's copy of the string
 */
unsigned char * puz_extras_set(struct puzzle_t *puz, unsigned char *val) {
  if(NULL == puz || NULL == val)
    return NULL;

  free(puz->gext);

  int size = puz_width_get(puz) * puz_height_get(puz);
  puz->gext = calloc(size+1, sizeof (unsigned char));
  memcpy(puz->gext, val, size);
  puz->gext[size] = 0;

  return puz->gext;
}


/**
 * puz_is_locked_get - check if the puzzle's solution is scrambled
 *
 * @puz: a pointer to the struct puzzle_t to check (required)
 * 
 * returns the value of the puzzle's locked flag (0 if unscrambled,
 *   nonzero if scrambled).
 */
int puz_is_locked_get (struct puzzle_t *puz) {
  if(NULL == puz)
    return 0;

  return puz->header.scrambled_tag;
}

/**
 * puz_locked_cksum_get - for puzzles that are locked, this returns
 *   the checksum of the real solution.
 *
 * @puz: a pointer to the struct puzzle_t to check against (required)
 * 
 * If the puzzle is locked, this returns a short, which is the result 
 * of calling:
 *     puz_cksum_region(sol, len, 0x0000);
 * where sol is the correct solution in column major order with the black
 * squares removed, and len is its length.
 * 
 */
unsigned short puz_locked_cksum_get(struct puzzle_t *puz) {
  if(NULL == puz)
    return 0;
  
  return puz->header.scrambled_cksum;
}

/**
 * puz_lock_set - set the flags indicating a puzzle's solution is locked
 *
 * @puz: a pointer to the struct puzzle_t to check (required)
 * @cksum: a unsigned short, the result of calling:
 *     puz_cksum_region(sol, len, 0x0000);
 *   when sol is the correct solution in column-major order with the
 *   black squares removed and len is its length.
 * 
 * returns the input upon success.  Otherwise, -1.
 */
unsigned short puz_lock_set (struct puzzle_t *puz, unsigned short cksum) {
  if(NULL == puz)
    return -1;

  puz->header.scrambled_tag = 4;
  puz->header.scrambled_cksum = cksum;

  return cksum;
}


/**
 * unscramble_string - an internal function used by puz_unscramble.
 * 
 * Let s be a string and len be its length.  A "scrambled" version
 * of s looks like:
 *
 * [ s[len/2], s[0], s[1 + (len/2)], s[1], s[2 + (len/2)], s[2],  ...]
 *
 * This function takes such a scrambled string (inp) and unscrambles
 * it into another string (out).  inp and out had better have the same
 * length.
 * 
 * return -1 on error and 0 otherwise.
 * 
 */
unsigned int unscramble_string (unsigned char* inp, unsigned char* out) {
  if(NULL == inp || NULL == out)
    return -1;

  int len = Sstrlen(inp);
  int strbreak = len/2;
  
  int i;
  for(i = 0; i < len; i++) {
    int index;
    if (0 == i%2) {
      index = strbreak + i/2;
    } else {
      index = i/2;
    }
    
    out[index] = inp[i];
  }
  out[len] = 0;

  return 0;
}

/**
 * unshift_string - an internal function used by puz_unscramble.
 * 
 * Given a string s and an int k, the "shifted" verison of s is created
 * by moving everything before index k to the end of the string.
 * 
 * This takes such a shifted string (inp) and undoes the shifting into
 * (out).
 *
 * return -1 on error and 0 otherwise.
 */
unsigned int unshift_string(unsigned char* inp, unsigned int shift,
                            unsigned char* out) {
  if(NULL == inp || NULL == out)
    return -1;

  int len = Sstrlen(inp);

  if (len < shift)
    return -1;

  Sstrncpy(out + shift, inp, len - shift);
  Sstrncpy(out, inp + (len - shift), shift);

  out[len] = 0;

  return 0;
}

