#include <stdlib.h> 
#include <unistd.h> 
#include <stdio.h> 
#include <libpq-bindings.h>
/* internals for PGconn struct: */
#include <internal/libpq-int.h>

#define ONE_MEGABYTE (1024*1024)

/* 
 * libpq grows the buffers it uses for input and output, as needed,
 * exponentially, but it never shrinks them. This means that if we try to keep
 * connections open in a thread pool for as long as possible we should expect
 * RES to grow and grow as e.g. a new biggest result query comes in.
 *
 * Our solution is to simply shrink the buffers back down to some max
 * acceptable size after each transaction. User Ilya confirms they've been
 * using essentially this approach successfully in production since reporting
 * the first ticket below.
 *
 * Later we might improve this by e.g. 
 *   - doing a shrink by half each time
 *   - only shrink when the last Result would have fit in the shrunken buffer
 *   - do above but only according to some random probability
 *
 * See e.g. pqCheckInBufferSpace which is the source of the leak that we noticed:
 *
 *   https://docs.huihoo.com/doxygen/postgresql/fe-misc_8c.html#ab9525b356ad08b90e9beb497203848f8 
 *
 * References to others who have encountered this issue
 *
 *   https://www.postgresql.org/message-id/15693-0df90da151425ff5@postgresql.org 
 *   https://groups.google.com/forum/#!topic/pgsql.general/DIA4g8mMzNM 
 *
 */ 
void PQclampInOutBufferSpace(PGconn *conn) {
    /* Max size we'll clamp buffers to. Setting this to below 8192 may break
     * assumptions in libpq.  Leaving it to well above that value should be
     * future-proof.
     *
     * Note also at least for glibc this region of memory is well above the
     * default M_MMAP_THRESHOLD of 128K; if this were not the case a free() or
     * realloc() would not necessarily free memory to the OS.
     */ 
    const int maxSize = ONE_MEGABYTE;
    
    /* RESPONSE BUFFER: 
     *
     * Shrink back to maxSize when the buffer has grown to double that (due to
     * a large response).
     *
     * The first condition here should always be true when we call this after
     * committing a transaction, but is included for sanity. It's not clear if
     * this function is safe to call in other situation.
     */
    if (conn->inStart == conn->inEnd && conn->inBufSize > maxSize * 2) {
        /* NOTE: Operationally we think we want to shrink this buffer and free
         * pages from the tail (of what is, at this size, likely mmapped memory)
         * and so realloc() is tempting. But realloc might actually copy, even when
         * shrinking (perhaps to avoid fragmentation).  free()+malloc() captures
         * our requirements better (we don't need to preserve data in the buffer),
         * so trust our malloc implementation to handle this case in the most
         * efficient way.
         */
        free(conn->inBuffer);
        conn->inBuffer = malloc(maxSize);
        conn->inBufSize = maxSize;
        if (conn->inBuffer == NULL) {
            /* out of memory somehow. just die. */
            abort();
        }
    }

    /* REQUEST BUFFER: 
     *
     * Just as above, but for the request buffer.
     */
    if (conn->outCount == 0 && conn->outBufSize > maxSize * 2) {
        free(conn->outBuffer);
        conn->outBuffer = malloc(maxSize);
        conn->outBufSize = maxSize;
        if (conn->outBuffer == NULL) {
            abort();
        }
    }
}
