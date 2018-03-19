// Include type definitions for SQL Server's driver-specific types.
//
// These types aren't part of ODBC proper, but they have more
// information that regular ODBC.

#ifndef __msodbcsql_h__
#define __msodbcsql_h__

#pragma pack(8)


/* New Structure for TIME2 */
typedef struct tagSS_TIME2_STRUCT
{
    SQLUSMALLINT   hour;
    SQLUSMALLINT   minute;
    SQLUSMALLINT   second;
    SQLUINTEGER    fraction;
} SQL_SS_TIME2_STRUCT;

/* New Structure for TIMESTAMPOFFSET */
typedef struct tagSS_TIMESTAMPOFFSET_STRUCT
{
    SQLSMALLINT    year;
    SQLUSMALLINT   month;
    SQLUSMALLINT   day;
    SQLUSMALLINT   hour;
    SQLUSMALLINT   minute;
    SQLUSMALLINT   second;
    SQLUINTEGER    fraction;
    SQLSMALLINT    timezone_hour;
    SQLSMALLINT    timezone_minute;
} SQL_SS_TIMESTAMPOFFSET_STRUCT;

#pragma pack()

#endif
