/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    PKY33011                             *  ./
/. *   JOB-NAME    :    ＡＣＯＳ計上Ｆクリア                 * ./
/. *               :    バックＪＯＢにて処理                 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PKY33011'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?CHK      ,STRING*1,VALUE-'0'           /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    DEFLIBL TOKELIB/TOKFLIB

/.##待ち受け処理（３０分間待ち合わせ）##./
TWAIT:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    MSG-'##計上Ｆ初期化監視中##',TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    CALL      PGM-WAITPG.TOKELIB,PARA-('000005')
    IF        @PGMEC  ^=  0  THEN
              GOTO ABEND
    END

/.##ＡＣＯＳ監視Ｆチェック##./
SKY3301B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    OVRF      FILE-ACOSCHK1,TOFILE-ACOSCHK1.TOKWLIB
    CALL      PGM-SKY3301B.TOKELIB,PARA-(?CHK)
    ?MSGX :=  '?CHK = ' && ?CHK
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    IF        @PGMEC  ^=  0  THEN
              GOTO ABEND
    ELSE
              SNDMSG ?CHK,TO-XCTL.@ORGPROF
              IF   ?CHK ^= '1'  THEN
                   GOTO  TWAIT
              END
    END

/.##ＡＣＯＳ監視Ｆ初期化##./
PCLRFILE:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE FILE-ACOSWK.TOKWLIB
    IF        @PGMEC  ^=  0  THEN
              GOTO ABEND
    END

    CLRFILE FILE-ACOSFILE.TOKWLIB
    IF        @PGMEC  ^=  0  THEN
              GOTO ABEND
    END

RTN:    /.ﾌﾟﾛｸﾞﾗﾑ正常終了時処理./

    CALL SKY3701L.TOKELIB,PARA-('1')
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    CALL SKY3701L.TOKELIB,PARA-('2')
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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    DSPLOG CLR-@YES,EDT-@JEF

    RETURN    PGMEC-@PGMEC
