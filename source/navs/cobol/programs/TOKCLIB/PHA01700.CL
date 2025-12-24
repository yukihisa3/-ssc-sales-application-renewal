/. ***********************************************************  ./
/. *     サカタのタネ　新基幹システム                        *  ./
/. *   SYSTEM-NAME :    自動発注管理                         *  ./
/. *   JOB-ID      :    PHA01700                             *  ./
/. *   JOB-NAME    :    自動発注確定更新                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PHA01700'
    VAR       ?STEP     ,STRING*8
    VAR       ?SOKO     ,STRING*2
    VAR       ?DSOKO    ,STRING*2
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN   :=        @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL    TOKFLIB/TOKELIB

/.  自動発注確定更新                                            ./
SHA0170B:
    ?STEP :=   'SHA0170B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-AUTHEDL1,TOFILE-AUTHEDL1.TOKFLIB
    OVRF      FILE-AUTMEIL1,TOFILE-AUTMEIL1.TOKFLIB
    OVRF      FILE-HACHEDL1,TOFILE-HACHEDL1.TOKFLIB
    OVRF      FILE-HACMEIL1,TOFILE-HACMEIL1.TOKFLIB
    OVRF      FILE-AUTHACL2,TOFILE-AUTHACL2.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-SHA0170B.TOKELIB
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