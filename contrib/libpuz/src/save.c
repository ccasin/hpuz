/******************************************************************
 * libpuz - A .PUZ crossword library
 * Copyright(c) 2006 Josh Myer <josh@joshisanerd.com>
 * 
 * This code is released under the terms of the GNU General Public
 * License version 2 or later.  You should have receieved a copy as
 * along with this source as the file COPYING.
 ******************************************************************/

/*
 * save.c -- Puzzle save (write-out) routines
 */

#include <puz.h>

static int puz_save_bin(struct puzzle_t *puz, unsigned char *base, int sz);

/**
 * puz_save_bin - save a puzzle in the binary PUZ format
 *
 * @puz: the puzzle to write out
 * @base: the buffer to write to
 * @sz: the size of the buffer
 *
 * This is an internal function.
 *
 * This function is called by puz_save to save binary files.  You will
 * probably need to call puz_size() first to make sure your buffer is
 * the right size/big enough to contain the puzzle.
 *
 * Return value: -1 if the buffer is too small, positive integer of
 * the number of bytes used by the file on success.
 */
static int puz_save_bin(struct puzzle_t *puz, unsigned char *base, int sz) {
  int i, k, s;
  unsigned char * rtbl_str;

  int puz_a = 0;

  s = puz_size(puz);

  //printf("Saving puzzle of size %d\n", s);

  if(s > sz) {
    printf("Save failed: buffer not big enough.  Got %d bytes, needed %d\n", sz, s);
    return -1;
  }

  memset(base, 0, s);

  i = 0;

  w_le_16(base+i, puz->header.cksum_puz);
  i += 2;

  memcpy(base+i, puz->header.magic, 12);
  i += 12;

  w_le_16(base+i, puz->header.cksum_cib);
  i += 2;

  memcpy(base+i, puz->header.magic_10, 4);
  i += 4;

  memcpy(base+i, puz->header.magic_14, 4);
  i += 4;

  memcpy(base+i, puz->header.magic_18, 4);
  i += 4;

  w_le_16(base+i, puz->header.noise_1c);
  i += 2;

  w_le_16(base+i, puz->header.scrambled_cksum);
  i += 2;

  w_le_16(base+i, puz->header.noise_20);
  i += 2;
  w_le_16(base+i, puz->header.noise_22);
  i += 2;
  w_le_16(base+i, puz->header.noise_24);
  i += 2;
  w_le_16(base+i, puz->header.noise_26);
  i += 2;
  w_le_16(base+i, puz->header.noise_28);
  i += 2;
  w_le_16(base+i, puz->header.noise_2a);
  i += 2;

  w_le_8(base+i, puz->header.width);
  i++;
  w_le_8(base+i, puz->header.height);
  i++;
  w_le_16(base+i, puz->header.clue_count);
  i += 2;
  w_le_16(base+i, puz->header.x_unk_30);
  i += 2;
  w_le_16(base+i, puz->header.scrambled_tag);
  i += 2;

  puz_a = puz->header.width * puz->header.height;

  memcpy(base+i, puz->solution, puz_a);
  i += puz_a;

  memcpy(base+i, puz->grid, puz_a);
  i += puz_a;

  memcpy(base+i, puz->title, Sstrlen(puz->title)+1);
  i += Sstrlen(puz->title)+1;
  memcpy(base+i, puz->author, Sstrlen(puz->author)+1);
  i += Sstrlen(puz->author)+1;
  memcpy(base+i, puz->copyright, Sstrlen(puz->copyright)+1);
  i += Sstrlen(puz->copyright)+1;

  for(k = 0; i < s && k < puz->header.clue_count; k++) {
    memcpy(base+i, puz->clues[k], Sstrlen(puz->clues[k])+1);
    i += Sstrlen(puz->clues[k])+1;
  }

  memcpy(base+i, puz->notes, puz->notes_sz);
  i += puz->notes_sz;

  base[i] = '\0';
  i++;

  if (puz_has_rebus(puz)) {
    memcpy(base+i, "GRBS", 4);
    i += 4;

    w_le_16(base+i, puz_a);
    i += 2;

    w_le_16(base+i, puz->grbs_cksum);
    i += 2;

    memcpy(base+i, puz->grbs, puz_a);
    i += puz_a;

    base[i] = '\0';
    i++;

    memcpy(base+i, "RTBL", 4);
    i += 4;

    // load up rebus table as a string...
    rtbl_str = puz_rtblstr_get(puz);  
    unsigned int rtbl_sz = Sstrlen(rtbl_str);
    
    w_le_16(base+i, rtbl_sz);
    i += 2;

    w_le_16(base+i, puz->rtbl_cksum);
    i += 2;

    memcpy(base+i, rtbl_str, rtbl_sz);
    free (rtbl_str);
    i += rtbl_sz;

    base[i] = 0;
    i++;
  }

  if (puz_has_timer(puz)) {
    // the string must be null-terminated.  We're careful to maintain that.
    int size = Sstrlen(puz->ltim);

    memcpy(base+i, "LTIM", 4);
    i += 4;

    w_le_16(base+i, size);
    i += 2;

    w_le_16(base+i, puz->ltim_cksum);
    i += 2;

    Sstrncpy(base+i,puz->ltim,size+1);
    i += 1+size;
  }

  if (puz_has_extras(puz)) {
    memcpy(base+i, "GEXT", 4);
    i += 4;

    w_le_16(base+i, puz_a);
    i += 2;

    w_le_16(base+i, puz->gext_cksum);
    i += 2;

    memcpy(base+i, puz->gext, puz_a);
    i += puz_a;

    base[i] = '\0';
    i++;
  }

  if (puz_has_rusr(puz)) {
    memcpy(base+i, "RUSR", 4);
    i += 4;

    w_le_16(base+i, puz->rusr_sz);
    i += 2;

    w_le_16(base+i, puz->rusr_cksum);
    i += 2;

    unsigned char* rusrstr = puz_rusrstr_get(puz);  // XXX NULL
    memcpy(base+i, rusrstr, puz->rusr_sz);
    free(rusrstr);

    i += puz->rusr_sz;
    base[i] = 0;
    i++;
  }

  if(s != i)
    printf("Writing to memory was short: expected to write %d bytes, wrote %d\n", s, i);

  return s;
}

/**
 * puz_save - Save a puzzle
 *
 * @puz: pointer to the struct puzzle_t containing the puzzle to save (required)
 * @type: type of the file to save as (currently only PUZ_FILE_BINARY supported)
 * @base: pointer to the buffer to write the PUZ to (required)
 * @sz: size of the buffer (required)
 *
 * This function is used to save a file 
 *
 * Return Value: -1 on error, size of the output puz on success. 
 */
int puz_save(struct puzzle_t *puz, int type, unsigned char *base, int sz) {
  if(type != PUZ_FILE_BINARY)
    return -1;

  return puz_save_bin(puz, base, sz);
}

