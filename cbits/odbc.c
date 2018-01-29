#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>
#include <odbcss.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>
#include <odbcss.h>

#define FALSE 0
#define TRUE 1
#define MAXBUFLEN 256
#define MAXNAME 256

void odbc_ProcessLogMessages(SQLSMALLINT plm_handle_type, SQLHANDLE plm_handle, char *logstring, int ConnInd);

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
// Connect/disconnect

RETCODE odbc_SQLDriverConnectW(SQLHDBC *hdbc, SQLWCHAR *connString, SQLSMALLINT len){
  SQLSMALLINT ignored = 0;
  RETCODE r = SQLDriverConnectW(
      *hdbc,
      NULL,
      connString,
      len,
      NULL,
      0,
      &ignored,
      SQL_DRIVER_NOPROMPT);
  if (r == SQL_ERROR)
    odbc_ProcessLogMessages(SQL_HANDLE_DBC, *hdbc, "odbc_SQLDriverConnectW", FALSE);
  return r;
}

void odbc_SQLDisconnect(SQLHDBC *hdbc){
  SQLDisconnect(*hdbc);
}

////////////////////////////////////////////////////////////////////////////////
// Alloc/dealloc statement

SQLHSTMT *odbc_SQLAllocStmt(SQLHDBC *hdbc){
  SQLHSTMT *hstmt = malloc(sizeof *hstmt);
  *hstmt = SQL_NULL_HSTMT;
  RETCODE retcode = SQLAllocHandle (SQL_HANDLE_STMT, *hdbc, hstmt);
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
// Prepare

SQLRETURN odbc_SQLPrepareW(
  SQLHSTMT  *   hstmt,
  SQLWCHAR  *   text,
  SQLINTEGER len
  ){
  RETCODE r = SQLPrepareW(*hstmt, text, len);
  if (r == SQL_ERROR)
    odbc_ProcessLogMessages(SQL_HANDLE_STMT, *hstmt, "odbc_SQLPrepareW", FALSE);
  return r;
}

////////////////////////////////////////////////////////////////////////////////
// Execute

RETCODE odbc_SQLExecDirectW(SQLHSTMT *hstmt, SQLWCHAR *stmt, SQLINTEGER len){
  RETCODE r = SQLExecDirectW(*hstmt, stmt, len);
  if (r == SQL_ERROR) odbc_ProcessLogMessages(SQL_HANDLE_STMT, *hstmt, "odbc_SQLExecDirectW", FALSE);
  return r;
}

RETCODE odbc_SQLExec(SQLHSTMT *hstmt){
  RETCODE r = SQLExecute(*hstmt);
  if (r == SQL_ERROR) odbc_ProcessLogMessages(SQL_HANDLE_STMT, *hstmt, "odbc_SQLExec", FALSE);
  return r;
}

////////////////////////////////////////////////////////////////////////////////
// Fetch row

RETCODE odbc_SQLFetch(SQLHSTMT *hstmt){
  RETCODE r = SQLFetch(*hstmt);
  if (r == SQL_ERROR) odbc_ProcessLogMessages(SQL_HANDLE_STMT, *hstmt, "odbc_SQLFetch", FALSE);
  return r;
}

RETCODE odbc_SQLMoreResults(SQLHSTMT *hstmt){
  RETCODE r = SQLMoreResults(*hstmt);
  if (r == SQL_ERROR) odbc_ProcessLogMessages(SQL_HANDLE_STMT, *hstmt, "odbc_SQLMoreResults", FALSE);
  return r;
}

////////////////////////////////////////////////////////////////////////////////
// Get fields

RETCODE odbc_SQLGetData(SQLHSTMT *hstmt,
                        SQLUSMALLINT   col,
                        SQLSMALLINT    targetType,
                        SQLPOINTER     buffer,
                        SQLLEN         bufferLen,
                        SQLLEN *       resultLen){
  RETCODE r = SQLGetData(*hstmt, col, targetType, buffer, bufferLen, resultLen);
  if (r == SQL_ERROR) odbc_ProcessLogMessages(SQL_HANDLE_STMT, *hstmt, "odbc_SQLGetData", FALSE);
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
// Logs

void odbc_ProcessLogMessages(SQLSMALLINT plm_handle_type, SQLHANDLE plm_handle, char *logstring, int ConnInd) {
  RETCODE plm_retcode = SQL_SUCCESS;
  UCHAR plm_szSqlState[MAXBUFLEN] = "", plm_szErrorMsg[MAXBUFLEN] = "";
  SDWORD plm_pfNativeError = 0L;
  SWORD plm_pcbErrorMsg = 0;
  SQLSMALLINT plm_cRecNmbr = 1;
  SDWORD plm_SS_MsgState = 0/* , plm_SS_Severity = 0 */;
  SQLINTEGER plm_Rownumber = 0;
  USHORT plm_SS_Line;
  /* SQLSMALLINT plm_cbSS_Procname, plm_cbSS_Srvname; */
  /* SQLCHAR plm_SS_Procname[MAXNAME], plm_SS_Srvname[MAXNAME]; */

  if (logstring)
    puts(logstring);

  while (plm_retcode != SQL_NO_DATA_FOUND) {
    plm_retcode = SQLGetDiagRec(plm_handle_type, plm_handle, plm_cRecNmbr,
                                plm_szSqlState, &plm_pfNativeError, plm_szErrorMsg,
                                MAXBUFLEN - 1, &plm_pcbErrorMsg);

    // Note that if the application has not yet made a successful connection,
    // the SQLGetDiagField information has not yet been cached by ODBC Driver Manager and
    // these calls to SQLGetDiagField will fail.
    if (plm_retcode != SQL_NO_DATA_FOUND) {
      if (ConnInd) {
        plm_retcode = SQLGetDiagField( plm_handle_type, plm_handle, plm_cRecNmbr,
                                       SQL_DIAG_ROW_NUMBER, &plm_Rownumber,
                                       SQL_IS_INTEGER, NULL);

        plm_retcode = SQLGetDiagField( plm_handle_type, plm_handle, plm_cRecNmbr,
                                       SQL_DIAG_SS_LINE, &plm_SS_Line, SQL_IS_INTEGER, NULL);

        plm_retcode = SQLGetDiagField( plm_handle_type, plm_handle, plm_cRecNmbr,
                                       SQL_DIAG_SS_MSGSTATE, &plm_SS_MsgState,
                                       SQL_IS_INTEGER, NULL);

        /* plm_retcode = SQLGetDiagField( plm_handle_type, plm_handle, plm_cRecNmbr, */
        /*                                SQL_DIAG_SS_SEVERITY, &plm_SS_Severity, */
        /*                                SQL_IS_INTEGER, NULL); */

        /* plm_retcode = SQLGetDiagField( plm_handle_type, plm_handle, plm_cRecNmbr, */
        /*                                SQL_DIAG_SS_PROCNAME, &plm_SS_Procname, */
        /*                                sizeof(plm_SS_Procname), &plm_cbSS_Procname); */

        /* plm_retcode = SQLGetDiagField( plm_handle_type, plm_handle, plm_cRecNmbr, */
        /*                                SQL_DIAG_SS_SRVNAME, &plm_SS_Srvname, */
        /*                                sizeof(plm_SS_Srvname), &plm_cbSS_Srvname); */
      }

      printf("szSqlState = %s\n", plm_szSqlState);
      printf("pfNativeError = %d\n", plm_pfNativeError);
      printf("szErrorMsg = %s\n", plm_szErrorMsg);
      printf("pcbErrorMsg = %d\n\n", plm_pcbErrorMsg);

      if (ConnInd) {
        printf("ODBCRowNumber = %d\n", plm_Rownumber);
        printf("SSrvrLine = %d\n", plm_Rownumber);
        printf("SSrvrMsgState = %d\n", plm_SS_MsgState);
        /* printf("SSrvrSeverity = %d\n", plm_SS_Severity); */
        /* printf("SSrvrProcname = %s\n", plm_SS_Procname); */
        /* printf("SSrvrSrvname = %s\n\n", plm_SS_Srvname); */
      }
    }

    plm_cRecNmbr++;   // Increment to next diagnostic record.
  }
}
