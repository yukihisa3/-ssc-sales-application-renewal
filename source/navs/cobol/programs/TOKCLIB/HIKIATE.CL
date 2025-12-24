/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理                             *  ./
/. *   JOB-ID      :                                         *  ./
/. *   JOB-NAME    :   リカバリ  　　　                     *  ./
/. ***********************************************************  ./
    PGM       (PARA-?PARA)
    PARA      ?PARA     ,STRING*8,IN,VALUE-'00000000'
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'HIKIATE '
    VAR       ?STEP     ,STRING*8

 REC0001:
/.  未出庫数／引き当て済数をゼロにする                           ./

    OVRF FILE-ZZAIMS,TOFILE-ZZAIMS.TOKFLIB
    CALL REC0001.TOKELIB
    IF   @PGMEC      ^=  0
       THEN GOTO     ABEND
    END


REC0015:

    ?PGMID := 'REC0015 '
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-DENJNL6,TOFILE-SHTDENL6.TOKFLIB
    OVRF FILE-ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB
    OVRF FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL REC0015.TOKELIB
    IF   @PGMEC      ^=  0
       THEN GOTO     ABEND
    END

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