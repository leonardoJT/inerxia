/* brsleave.i - trigger code for ROW-LEAVE trigger of SmartBrowse*/
/* If the object selected is not a button (which could be a SmartPanel
   button such as Cancel or Reset), then save any changes to the row. 
   Otherwise let the button take the appropriate action. */
DEFINE VARIABLE widget-enter AS HANDLE NO-UNDO.
  widget-enter = last-event:widget-enter.
  IF ((VALID-HANDLE(widget-enter) AND widget-enter:type NE "BUTTON":U) OR
      (NOT VALID-HANDLE(widget-enter))) AND
       {&BROWSE-NAME}:CURRENT-ROW-MODIFIED THEN 
  DO:
           RUN dispatch('update-record':U).
           IF RETURN-VALUE = "ADM-ERROR":U THEN
               RETURN NO-APPLY.
  END. 
