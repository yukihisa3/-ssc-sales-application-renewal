/. ***********************************************************  ./
/. *     サカタのタネ　特販システム                          *  ./
/. *   SYSTEM-NAME :                                         *  ./
/. *   JOB-ID      :    PKY11010                             *  ./
/. *   JOB-NAME    :    送り状印刷　　　　                   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PKY11010'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.   CANWTR    PRINT2   ./
/.  送り状印刷                                              ./
SKY1101B:

    ?STEP :=   'SKY1101B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL



    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
/. *************** テスト用 **************************** ./
    OVRPRTF   FILE-PRTF,TOFILE-NAVF.XUCL
/. ***************************************************** ./
/.  OVRPRTF   FILE-PRTF,TOFILE-PRTF.TOKELIB              ./
    OVRF      FILE-TOKURIF,TOFILE-TOKURIF.TOKFLIB
    CALL      PGM-SKY1101B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:


/.  STRWTR    PRINT2,XOUTQ2,@NO      ./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:


    STRWTR    PRINT2,XOUTQ2,@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'


    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
