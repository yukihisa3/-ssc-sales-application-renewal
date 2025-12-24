/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    コンバート                           *  ./
/. *   JOB-ID      :    PNVPG30                              *  ./
/. *   JOB-NAME    :    入出庫ファイルコンバート             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PNVPG30'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##入出庫ファイルコンバート##./
CNVPG3:

    ?STEP :=   'CNVPG3'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    ?MSGX :=  '***  入出庫Ｆコンバート  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB

    OVRF FILE-ZNYUSDT1,TOFILE-ZNYUSDT1.TOKFLIB
    OVRF FILE-NYSFILL1,TOFILE-NYSFILL1.TOKFLIB
    CALL PGM-CNVPG3.TOKELIB
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