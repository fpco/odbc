#ifdef _WIN32
#include <windows.h>
#endif
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#define FALSE 0
#define TRUE 1
#define MAXBUFLEN 1024

// Just a way of grouping together these two dependent resources. It's
// probably not a good idea to free up an environment before freeing a
// database. So, we put them together so that they can be allocated
// and freed atomically.
typedef struct EnvAndDbc {
  SQLHENV *env;
  SQLHDBC *dbc;
  char *error;
  char *sqlState;
  // Allocated once in odbc_AllocEnvAndDbc and freed once in odbc_FreeEnvAndDbc.
} EnvAndDbc;

char *odbc_error(EnvAndDbc *envAndDbc){
  return envAndDbc->error;
}

char *odbc_sqlState(EnvAndDbc *envAndDbc){
  return envAndDbc->sqlState;
}

void odbc_ProcessLogMessages(EnvAndDbc *envAndDbc, SQLSMALLINT plm_handle_type, SQLHANDLE plm_handle, char *logstring, int ConnInd);

////////////////////////////////////////////////////////////////////////////////
// Alloc/dealloc env

SQLHENV *odbc_SQLAllocEnv(){
  SQLHENV *henv = malloc(sizeof *henv);
  *henv = SQL_NULL_HENV;
  RETCODE retcode = SQLAllocHandle (SQL_HANDLE_ENV, NULL, henv);
  if ((retcode != SQL_SUCCESS_WITH_INFO) && (retcode != SQL_SUCCESS)) {
    free(henv);
    return NULL;
  } else {
    return henv;
  }
}

void odbc_SQLFreeEnv(SQLHENV *henv){
  if(henv != NULL && *henv != SQL_NULL_HENV) {
    SQLFreeHandle(SQL_HANDLE_ENV, *henv);
    free(henv);
  }
}

RETCODE odbc_SetEnvAttr(SQLHENV *henv){
  return
    SQLSetEnvAttr(*henv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC3, SQL_IS_INTEGER);
}

////////////////////////////////////////////////////////////////////////////////
// Alloc/dealloc dbc

SQLHDBC *odbc_SQLAllocDbc(SQLHENV *henv){
  SQLHDBC *hdbc = malloc(sizeof *hdbc);
  *hdbc = SQL_NULL_HDBC;
  RETCODE retcode = SQLAllocHandle (SQL_HANDLE_DBC, *henv, hdbc);
  if ((retcode != SQL_SUCCESS_WITH_INFO) && (retcode != SQL_SUCCESS)) {
    free(hdbc);
    return NULL;
  } else {
    return hdbc;
  }
}

void odbc_SQLFreeDbc(SQLHDBC *hdbc){
  if(hdbc != NULL && *hdbc != SQL_NULL_HDBC) {
    SQLFreeHandle(SQL_HANDLE_DBC, *hdbc);
    free(hdbc);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Allocate/dealloc env-and-database

EnvAndDbc *odbc_AllocEnvAndDbc(){
  SQLHENV *env = odbc_SQLAllocEnv();
  if (env == NULL) {
    return NULL;
  } else {
    int retcode = odbc_SetEnvAttr(env);
    if ((retcode != SQL_SUCCESS_WITH_INFO) && (retcode != SQL_SUCCESS)) {
      free(env);
      return NULL;
    } else {
      SQLHDBC *dbc = odbc_SQLAllocDbc(env);
      if (dbc == NULL) {
        return NULL;
      } else {
        EnvAndDbc *envAndDbc = malloc(sizeof *envAndDbc);
        envAndDbc->env = env;
        envAndDbc->dbc = dbc;
        envAndDbc->error = malloc(SQL_MAX_MESSAGE_LENGTH);
        // SQLSTATE is a five-character code, see https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/appendix-a-odbc-error-codes?view=sql-server-ver15
        envAndDbc->sqlState = malloc(6);
        return envAndDbc;
      }
    }
  }
}

void odbc_FreeEnvAndDbc(EnvAndDbc *envAndDbc){
  free(envAndDbc->error);
  free(envAndDbc->sqlState);
  odbc_SQLFreeDbc(envAndDbc->dbc);
  odbc_SQLFreeEnv(envAndDbc->env);
  free(envAndDbc);
}

////////////////////////////////////////////////////////////////////////////////
// Connect/disconnect

RETCODE odbc_SQLDriverConnect(EnvAndDbc *envAndDbc, SQLCHAR *connString, SQLSMALLINT len){
  SQLSMALLINT ignored = 0;
  RETCODE r = SQLDriverConnect(
    *(envAndDbc->dbc),
    NULL,
    connString,
    len,
    NULL,
    0,
    &ignored,
    SQL_DRIVER_NOPROMPT);
  if (r == SQL_ERROR)
    odbc_ProcessLogMessages(envAndDbc, SQL_HANDLE_DBC, *(envAndDbc->dbc), "odbc_SQLDriverConnect", FALSE);
  return r;
}

void odbc_SQLDisconnect(EnvAndDbc *envAndDbc){
  SQLDisconnect(*(envAndDbc->dbc));
}

////////////////////////////////////////////////////////////////////////////////
// Alloc/dealloc statement

SQLHSTMT *odbc_SQLAllocStmt(EnvAndDbc *envAndDbc){
  SQLHSTMT *hstmt = malloc(sizeof *hstmt);
  *hstmt = SQL_NULL_HSTMT;
  RETCODE retcode = SQLAllocHandle (SQL_HANDLE_STMT, *(envAndDbc->dbc), hstmt);
  if ((retcode != SQL_SUCCESS_WITH_INFO) && (retcode != SQL_SUCCESS)) {
    free(hstmt);
    return NULL;
  } else {
    return hstmt;
  }
}

void odbc_SQLFreeStmt(SQLHSTMT *hstmt){
  if(hstmt != NULL && *hstmt != SQL_NULL_HSTMT) {
    SQLFreeHandle(SQL_HANDLE_STMT, *hstmt);
    free(hstmt);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Params

SQLRETURN odbc_SQLBindParameter(
  EnvAndDbc*    envAndDbc,
  SQLHSTMT*     statement_handle,
  SQLUSMALLINT  parameter_number,
  SQLSMALLINT   value_type,
  SQLSMALLINT   parameter_type,
  SQLULEN       column_size,
  SQLPOINTER    parameter_value_ptr,
  SQLLEN        buffer_length,
  SQLLEN*       buffer_length_ptr
  ) {
  RETCODE r = SQLBindParameter(
    *statement_handle,
    parameter_number,
    SQL_PARAM_INPUT,
    value_type,
    parameter_type,
    column_size,
    0,
    parameter_value_ptr,
    buffer_length,
    buffer_length_ptr
    );
  if (r == SQL_ERROR)
    odbc_ProcessLogMessages(envAndDbc, SQL_HANDLE_STMT, *statement_handle, "odbc_SQLBindParameter", FALSE);
  return r;
}

////////////////////////////////////////////////////////////////////////////////
// Execute

RETCODE odbc_SQLExecDirectW(EnvAndDbc *envAndDbc, SQLHSTMT *hstmt, SQLWCHAR *stmt, SQLINTEGER len){
  RETCODE r = SQLExecDirectW(*hstmt, stmt, len);
  if (r == SQL_ERROR) {
    odbc_ProcessLogMessages(envAndDbc, SQL_HANDLE_STMT, *hstmt, "odbc_SQLExecDirectW", FALSE);
  }
  return r;
}

////////////////////////////////////////////////////////////////////////////////
// Fetch row

RETCODE odbc_SQLFetch(EnvAndDbc *envAndDbc, SQLHSTMT *hstmt){
  RETCODE r = SQLFetch(*hstmt);
  if (r == SQL_ERROR) odbc_ProcessLogMessages(envAndDbc, SQL_HANDLE_STMT, *hstmt, "odbc_SQLFetch", FALSE);
  return r;
}

RETCODE odbc_SQLMoreResults(EnvAndDbc *envAndDbc, SQLHSTMT *hstmt){
  RETCODE r = SQLMoreResults(*hstmt);
  if (r == SQL_ERROR) odbc_ProcessLogMessages(envAndDbc, SQL_HANDLE_STMT, *hstmt, "odbc_SQLMoreResults", FALSE);
  return r;
}

////////////////////////////////////////////////////////////////////////////////
// Get fields

RETCODE odbc_SQLGetData(EnvAndDbc *envAndDbc,
                        SQLHSTMT *hstmt,
                        SQLUSMALLINT   col,
                        SQLSMALLINT    targetType,
                        SQLPOINTER     buffer,
                        SQLLEN         bufferLen,
                        SQLLEN *       resultLen){
  RETCODE r = SQLGetData(*hstmt, col, targetType, buffer, bufferLen, resultLen);
  if (r == SQL_ERROR) odbc_ProcessLogMessages(envAndDbc, SQL_HANDLE_STMT, *hstmt, "odbc_SQLGetData", FALSE);
  return r;
}

SQLRETURN odbc_SQLDescribeColW(
  SQLHSTMT      *StatementHandle,
  SQLUSMALLINT   ColumnNumber,
  SQLWCHAR *      ColumnName,
  SQLSMALLINT    BufferLength,
  SQLSMALLINT *  NameLengthPtr,
  SQLSMALLINT *  DataTypePtr,
  SQLULEN *      ColumnSizePtr,
  SQLSMALLINT *  DecimalDigitsPtr,
  SQLSMALLINT *  NullablePtr){
  return SQLDescribeColW(
    *StatementHandle,
    ColumnNumber,
    ColumnName,
    BufferLength,
    NameLengthPtr,
    DataTypePtr,
    ColumnSizePtr,
    DecimalDigitsPtr,
    NullablePtr);
}

////////////////////////////////////////////////////////////////////////////////
// Get columns

RETCODE odbc_SQLNumResultCols(SQLHSTMT *hstmt, SQLSMALLINT *cols){
  return SQLNumResultCols(*hstmt, cols);
}

////////////////////////////////////////////////////////////////////////////////
// Get rows

RETCODE odbc_SQLRowCount(SQLHSTMT *hstmt, SQLLEN *rows){
  return SQLRowCount(*hstmt, rows);
}

////////////////////////////////////////////////////////////////////////////////
// Logs

void odbc_ProcessLogMessages(EnvAndDbc *envAndDbc, SQLSMALLINT plm_handle_type, SQLHANDLE plm_handle, char *logstring, int ConnInd) {
  // It has been fully tested that trying anything beyond 1 produces
  // the same string.
  SQLSMALLINT plg_record_number = 1;
  // The subtract-1 leaves space for zero-termination.
  SQLSMALLINT copy_this_many_bytes = MAXBUFLEN - 1;

  // These are not interesting for our use-case, but needed by
  // SQLGetDiagRec:
  SDWORD plm_pfNativeError = 0L;
  SWORD plm_pcbErrorMsg = 0;

  // Copy the error into: envAndDbc->error
  //
  // This may fail, but in that case there's no useful error message.
  SQLGetDiagRec(plm_handle_type,
                plm_handle,
                plg_record_number,
                (SQLCHAR *)envAndDbc->sqlState,
                &plm_pfNativeError,
                (SQLCHAR *)envAndDbc->error,
                copy_this_many_bytes,
                &plm_pcbErrorMsg);
}

////////////////////////////////////////////////////////////////////////////////
// Accessors for DATE_STRUCT

SQLSMALLINT DATE_STRUCT_year(DATE_STRUCT *d){
  return d->year;
}
SQLUSMALLINT DATE_STRUCT_month(DATE_STRUCT *d){
  return d->month;
}
SQLUSMALLINT DATE_STRUCT_day(DATE_STRUCT *d){
  return d->day;
}

////////////////////////////////////////////////////////////////////////////////
// Accessors for TIME_STRUCT

SQLUSMALLINT TIME_STRUCT_hour(TIME_STRUCT *t){
  return t->hour;
}
SQLUSMALLINT TIME_STRUCT_minute(TIME_STRUCT *t){
  return t->minute;
}
SQLUSMALLINT TIME_STRUCT_second(TIME_STRUCT *t){
  return t->second;
}

////////////////////////////////////////////////////////////////////////////////
// Accessors for TIMESTAMP_STRUCT

SQLSMALLINT TIMESTAMP_STRUCT_year(TIMESTAMP_STRUCT *t){
  return t->year;
}
SQLUSMALLINT TIMESTAMP_STRUCT_month(TIMESTAMP_STRUCT *t){
  return t->month;
}
SQLUSMALLINT TIMESTAMP_STRUCT_day(TIMESTAMP_STRUCT *t){
  return t->day;
}
SQLUSMALLINT TIMESTAMP_STRUCT_hour(TIMESTAMP_STRUCT *t){
  return t->hour;
}
SQLUSMALLINT TIMESTAMP_STRUCT_minute(TIMESTAMP_STRUCT *t){
  return t->minute;
}
SQLUSMALLINT TIMESTAMP_STRUCT_second(TIMESTAMP_STRUCT *t){
  return t->second;
}
SQLUINTEGER TIMESTAMP_STRUCT_fraction(TIMESTAMP_STRUCT *t){
  return t->fraction;
}

////////////////////////////////////////////////////////////////////////////////
// Definition and accessors for SQL_SS_TIMESTAMPOFFSET_STRUCT
// The strcut definition is from
// https://docs.microsoft.com/en-us/sql/relational-databases/native-client-odbc-date-time/data-type-support-for-odbc-date-and-time-improvements
typedef struct tagTIMESTAMPOFFSET_STRUCT {
  SQLSMALLINT year;
  SQLUSMALLINT month;
  SQLUSMALLINT day;
  SQLUSMALLINT hour;
  SQLUSMALLINT minute;
  SQLUSMALLINT second;
  SQLUINTEGER fraction;
  SQLSMALLINT timezone_hour;
  SQLSMALLINT timezone_minute;
} TIMESTAMPOFFSET_STRUCT;

#if (ODBCVER >= 0x0300)
  typedef TIMESTAMPOFFSET_STRUCT SQL_SS_TIMESTAMPOFFSET_STRUCT;
#endif

SQLSMALLINT TIMESTAMPOFFSET_STRUCT_year(TIMESTAMPOFFSET_STRUCT *t){
  return t->year;
}

SQLUSMALLINT TIMESTAMPOFFSET_STRUCT_month(TIMESTAMPOFFSET_STRUCT *t){
  return t->month;
}

SQLUSMALLINT TIMESTAMPOFFSET_STRUCT_day(TIMESTAMPOFFSET_STRUCT *t){
  return t->day;
}

SQLUSMALLINT TIMESTAMPOFFSET_STRUCT_hour(TIMESTAMPOFFSET_STRUCT *t){
  return t->hour;
}

SQLUSMALLINT TIMESTAMPOFFSET_STRUCT_minute(TIMESTAMPOFFSET_STRUCT *t){
  return t->minute;
}

SQLUSMALLINT TIMESTAMPOFFSET_STRUCT_second(TIMESTAMPOFFSET_STRUCT *t){
  return t->second;
}

SQLUINTEGER TIMESTAMPOFFSET_STRUCT_fraction(TIMESTAMPOFFSET_STRUCT *t){
  return t->fraction;
}

SQLSMALLINT TIMESTAMPOFFSET_STRUCT_timezone_hour(TIMESTAMPOFFSET_STRUCT *t){
  return t->timezone_hour;
}

SQLSMALLINT TIMESTAMPOFFSET_STRUCT_timezone_minute(TIMESTAMPOFFSET_STRUCT *t){
  return t->timezone_minute;
}
