DEFINE BUTTON bprintset LABEL "Printer Setup".
DEFINE BUTTON bprintnames LABEL "Print Customer Names".
DEFINE BUTTON bcancel LABEL "Cancel".

DEFINE VAR vTemp AS HANDLE.

DEFINE FRAME PrintFrame
    bprintset
    bprintnames
    bcancel
    WITH TITLE "Quick Printer" VIEW-AS DIALOG-BOX.
    
ON CHOOSE OF bprintset DO:
    SYSTEM-DIALOG PRINTER-SETUP.
END.
    
ON CHOOSE OF bprintnames DO:
    /*DISPLAY session:GET-PRINTERS() FORMAT "X(50)".*/

    DISPLAY SESSION:PRINTER-NAME FORMAT "X(50)".

    LEAVE.

    OUTPUT TO PRINTER.
    FOR EACH clientes WHERE nit = "134679" BY nit:
        DISPLAY nit WITH STREAM-IO.
    END.
    OUTPUT CLOSE.
END.
    
ENABLE ALL WITH FRAME PrintFrame.
    
WAIT-FOR CHOOSE OF bcancel IN FRAME PrintFrame.
