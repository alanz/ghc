/* -----------------------------------------------------------------------------
 * $Id: Proftimer.c,v 1.4 1999/08/04 17:03:41 panne Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

/* Only have cost centres etc if PROFILING defined */

#if defined (PROFILING)

#include "Rts.h"
#include "ProfRts.h"
#include "Itimer.h"
#include "Proftimer.h"

lnat total_ticks = 0;

nat current_interval = 1;               /* Current interval number -- 
					   stored in AGE */

nat interval_ticks = DEFAULT_INTERVAL;  /* No of ticks in an interval */

nat previous_ticks = 0;                 /* ticks in previous intervals */
nat current_ticks = 0;                  /* ticks in current interval */

void
initProfTimer(nat ms)
{
  if (initialize_virtual_timer(ms)) {
    fflush(stdout);
    fprintf(stderr, "Can't initialize virtual timer.\n");
    stg_exit(EXIT_FAILURE);
  }
};

void
stopProfTimer(void)
{				/* Stops time profile */
  if (time_profiling) {
    initProfTimer(0);
  }
};

void
startProfTimer(void)
{				/* Starts time profile */
  if (time_profiling) {
    initProfTimer(TICK_MILLISECS);
  }
};

/* For a small collection of signal handler prototypes, see
   http://web2.airmail.net/sjbaker1/software/signal_collection.html */

void
handleProfTick(int unused)
{
  (void)unused;   /* no warnings, please */
  CCS_TICK(CCCS);
  total_ticks++;
};

#endif /* PROFILING */
