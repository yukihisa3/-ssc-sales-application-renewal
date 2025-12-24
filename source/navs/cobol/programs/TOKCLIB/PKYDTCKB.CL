/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    サブルーチンチェック      　　　     *  ./
/. *   JOB-ID      :    PKYDTCKB                             *  ./
/. *   JOB-NAME    :    日付チェックサブルーチン             *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PKYDTCKB'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?INKBN    ,STRING*1,VALUE-'5'
    VAR ?INYMD1   ,STRING*6,VALUE-'000010'
    VAR ?INYMD2   ,STRING*8,VALUE-'19990915'
    VAR ?OUTRET   ,STRING*1,VALUE-' '
    VAR ?OUTYMD   ,STRING*8,VALUE-'        '
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    DEFLIBL TOKELIB/TOKFLIB
/.                              ./
PKYDTCKB:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    /.基本ｽｹｼﾞｭｰﾙﾏｽﾀ定義./
    CALL      PGM-SKYDTCKB.TOKELIB,
              PARA-(?INKBN,?INYMD1,?INYMD2,?OUTRET,?OUTYMD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    SNDMSG ?OUTRET,TO-XCTL
    SNDMSG ?OUTYMD,TO-XCTL
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

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