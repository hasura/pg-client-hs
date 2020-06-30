#include <internal/postgres_fe.h>
#include <stdio.h>
#include "libpq-fe.h"

/* Shrink input and output buffers. See libpq.c */
extern void PQclampInOutBufferSpace(PGconn *conn);
