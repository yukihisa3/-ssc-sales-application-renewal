/. ***********************************************************  ./
/. *     サカタのタネ　新基幹システム                        *  ./
/. *   SYSTEM-NAME :    発注管理                             *  ./
/. *   JOB-ID      :    PHA00500                             *  ./
/. *   JOB-NAME    :    発注書                               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PHA00500'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


/.  発注書                                                      ./
SHA0050L:
    DEFLIBL    TOKFLIB/TOKELIB
    ?STEP :=   'SHA0050L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB

    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL,SPOOL-@YES,
            OUTQ-XOUTQ2,MEDLIB-TOKELIB
    OVRF      FILE-HACHEDL1,TOFILE-HACHEDL1.TOKFLIB
    OVRF      FILE-HACMEIL1,TOFILE-HACMEIL1.TOKFLIB
    OVRF      FILE-ZSOMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-HTOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-HTENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-HMEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SHA0050L.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

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