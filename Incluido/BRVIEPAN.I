  DEFINE VARIABLE hdl    AS HANDLE NO-UNDO.                                
  DEFINE VARIABLE ch     AS CHAR   NO-UNDO.                                
  DEFINE VARIABLE ids    AS CHAR   NO-UNDO.                               
  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,                
                                         "RECORD-SOURCE":U,             
                                         OUTPUT ch).                    
  ASSIGN hdl = WIDGET-HANDLE(ch).                                              
  RUN send-records IN hdl ("{&FIRST-EXTERNAL-TABLE}", OUTPUT ids).                  
  IF ids = "?" THEN                                                     
     DO:                                                               
        CLEAR FRAME {&FRAME-NAME}.                                    
        RUN new-state ('no-record-available, SELF':U).                
     END.
