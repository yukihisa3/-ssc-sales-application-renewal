/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＷＳログ制御処理                     *  ./
/. *   JOB-ID      :    PLG00300                              *  ./
/. *   JOB-NAME    :    ＷＳログ制御処理                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PLG00300'
    VAR       ?STEP     ,STRING*8
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKKLIB/TOKFLIB/TOKELIB/TOKELIBO/TOKCLIBO

/.##ｽﾌﾟｰﾙﾌｧｲﾙ情報取得##./
SKYSPLQB:

    ?STEP :=   'SKYSPLQB'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL SKYSPLQB.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'##　SPLF情報取得 ERR   ##',TO-XCTL
              GOTO ABEND END

/.##ｽﾌﾟｰﾙﾌｧｲﾙ→ﾗｲﾌﾞﾗﾘ出力##./
SLG0030B:

    ?STEP :=   'SLG0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL SLG0030B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'##　SPLF出力 ERROR     ##',TO-XCTL
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