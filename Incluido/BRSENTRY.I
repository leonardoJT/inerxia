/* brsentry.i - trigger code for ROW-ENTRY of SmartBrowse - 8/16/95 */  
&IF NUM-ENTRIES ("{&ENABLED-TABLES-IN-QUERY-{&BROWSE-NAME}}":U," ":U) = 1 &THEN
&SCOP FP1 adm-initial-table.
&SCOP FP2 @ {&ENABLED-TABLES-IN-QUERY-{&BROWSE-NAME}}.
&SCOP FP3
   IF {&BROWSE-NAME}:NEW-ROW THEN 
   DO:
      IF adm-adding-record THEN 
              DISPLAY {&FIELD-PAIRS-IN-QUERY-{&BROWSE-NAME}}
                  WITH BROWSE {&BROWSE-NAME}.
      ELSE                       /* Start with old record for a copy */
      DO:
         FIND {&ENABLED-TABLES-IN-QUERY-{&BROWSE-NAME}} WHERE 
           ROWID({&ENABLED-TABLES-IN-QUERY-{&BROWSE-NAME}}) = 
              adm-prev-rowid NO-LOCK. 
          DISPLAY {&FIELDS-IN-QUERY-{&BROWSE-NAME}} WITH BROWSE {&BROWSE-NAME}.
      END.
   END. 
&ENDIF
