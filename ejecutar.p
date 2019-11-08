
    DEFINE INPUT PARAMETER cFullPathName AS CHARACTER FORMAT "x(250)" NO-UNDO.
    DEFINE VARIABLE hInstance AS INTEGER   NO-UNDO.

    {windows.i}
    {winfunc.i}

    PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
       DEFINE INPUT PARAMETER HWND AS LONG.
       DEFINE INPUT PARAMETER lpOperation AS CHARACTER.
       DEFINE INPUT PARAMETER lpFile AS CHARACTER.
       DEFINE INPUT PARAMETER lpParameters AS CHARACTER.
       DEFINE INPUT PARAMETER lpDirectory AS CHARACTER.
       DEFINE INPUT PARAMETER nShowCmd AS LONG.
       DEFINE RETURN PARAMETER hInstance AS LONG.
    END.

    RUN ShellExecute{&A} IN hpApi 
                   (0,
                    "open",
                    cFullPathName,
                    "",
                    "",
                    1,
                    OUTPUT hInstance).
