/*
 * LWSDK Library Source File
 *
 * Default 'Startup' function returns any non-zero value for success.
 *
 *$copyright$
 */
#include <lwserver.h>

void *Startup( void); // prototype

void *Startup (void)
{
    return (void *) 4;
}
