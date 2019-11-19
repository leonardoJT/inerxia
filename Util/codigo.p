/* on start-search del browser*/

    DEF VAR datasource AS HANDLE.
    DEF VAR csort AS CHAR NO-UNDO.
    DEF VAR tmpcol AS HANDLE NO-UNDO.
    
    tmpcol =  br_table:CURRENT-COLUMN.
    IF lastcol = tmpcol THEN DO:
        IF tmpdesc = "" THEN
            tmpdesc = " DESC ".
        ELSE tmpdesc = "".
    END. /*lastcol = tmpcol*/
    ELSE tmpdesc = "".

    lastcol = tmpcol.
    csort = tmpcol:NAME + tmpdesc .
    datasource = DYNAMIC-FUNCTION('getDataSource':U).
    {set querySort csort dataSource}.
    {fn openQuery dataSource}.

/*en definition del window*/
&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER dataSource AS HANDLE NO-UNDO.
&ELSE
DEF VARIABLE dataSource AS HANDLE NO-UNDO.
&ENDIF

/*create-object del window*/
  IF VALID-HANDLE(dataSource) THEN DO:
       
     RUN addLink
     (INPUT dataSource,
      INPUT 'DATA',
      INPUT h_bcust).

  END.

/*leave de fill busqueda*/
  DEF VAR cRowIdent AS CHARACTER NO-UNDO.
  DEF VAR cSearch   AS CHARACTER NO-UNDO.

  cSearch = "custnum = '" + fillcustnum:SCREEN-VALUE + "'".

  cRowIdent = dynamic-function('rowidWhere':U In datasource, cSearch).
  IF cRowIdent NE ? THEN
    dynamic-function('fetchRowIdent' IN datasource, cRowIdent, '':U).
