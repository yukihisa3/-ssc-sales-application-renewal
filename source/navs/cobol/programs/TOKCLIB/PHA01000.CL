/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    発注管理サブシステム      　　　     *  ./
/. *   JOB-ID      :    PHA01000                             *  ./
/. *   JOB-NAME    :    自動発注数量計算処理                 * ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PHA01000'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?WKSTN    ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ./
    VAR ?WKSTNN   ,NAME                         /.ﾜｰｸｽﾃｰｼｮﾝ名前型./
/.###PG使用ﾊﾟﾗﾒﾀ###./
    VAR ?TEIBAN   ,STRING*1,VALUE-' '           /.ﾌﾟﾛｸﾞﾗﾑﾊﾟﾗﾒﾀ./
    VAR ?KISETU1  ,STRING*1,VALUE-' '           /.ﾌﾟﾛｸﾞﾗﾑﾊﾟﾗﾒﾀ./
    VAR ?KISETU2  ,STRING*1,VALUE-' '           /.ﾌﾟﾛｸﾞﾗﾑﾊﾟﾗﾒﾀ./
    VAR ?KISETU3  ,STRING*1,VALUE-' '           /.ﾌﾟﾛｸﾞﾗﾑﾊﾟﾗﾒﾀ./
    VAR ?JIDO     ,STRING*1,VALUE-' '           /.ﾌﾟﾛｸﾞﾗﾑﾊﾟﾗﾒﾀ./

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.###ﾗｲﾌﾞﾗﾘﾘｽﾄ登録###./
    DEFLIBL TOKELIB/TOKFLIB

/.##自動発注入力##./
SHA0100I:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SHA0100I.TOKELIB,PARA-(?TEIBAN,?KISETU1,?KISETU2,
                                         ?KISETU3,?JIDO)
    IF        @PGMEC    ^=   0    THEN
            IF @PGMEC = 4010  THEN
               SNDMSG MSG-'## 取消終了 ##',TO-XCTL.@ORGPROF,JLOG-@YES
               RETURN
            ELSE
              GOTO ABEND
            END
    ELSE
              /.## 自動発注済の場合、メッセージ出力後終了##./
              IF   ?JIDO  =  '1'  THEN
                   MENU MJIDOERR.TOKMLIB
                   RETURN
              END
    END

/.##在庫集計##./
SHA0110B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-ZAMZAIF,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-ZAMJISF,TOFILE-ZAMJISL1.TOKFLIB
    OVRF      FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-HMEIMS,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZAIKOF,TOFILE-ZAIKOF.TOKFLIB
    CALL      PGM-SHA0110B.TOKELIB,PARA-(?TEIBAN,?KISETU1,?KISETU2,
                                         ?KISETU3)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##自動発注ワーク作成##./
SHA0120B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-INF,TOFILE-ZAIKOF.TOKFLIB
    OVRF      FILE-HSHOTBL,TOFILE-SHOTBL5.TOKFLIB
    OVRF      FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-HMEIMS,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-AUTHACF,TOFILE-AUTHACL1.TOKFLIB
    CALL      PGM-SHA0120B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:    /.##ﾌﾟﾛｸﾞﾗﾑ正常終了時##./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.##ﾌﾟﾛｸﾞﾗﾑ異常終了時##./

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