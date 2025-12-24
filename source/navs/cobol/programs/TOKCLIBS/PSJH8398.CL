/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ホーマックリカバリ処理    　　　     *  ./
/. *   JOB-ID      :    PSJH8398                             *  ./
/. *   JOB-NAME    :    ホーマック情報データ倉庫チェック     * ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSJH8398'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?HIDUKE   ,STRING*8,VALUE-'        '    /.ﾊﾞｯﾁNO.:ﾋﾂﾞｹ./
    VAR ?JIKAN    ,STRING*4,VALUE-'    '        /.ﾊﾞｯﾁNO.:ｼﾞｶﾝ./
    VAR ?TORCD    ,STRING*8,VALUE-'        '    /.ﾊﾞｯﾁNO.:ﾄﾘﾋｷｻｷ./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.ｿｳｺCD         ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ホーマック情報データ倉庫チェック'

/.##ﾊﾞｯﾁNO.入力画面##./
SOS0640I:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL      PGM-SOS0670I.SOSLIBB,PARA-(?HIDUKE,?JIKAN,?TORCD
                                        ,?SOKCD)
    IF        @PGMEC    ^=   0    THEN
              IF  @PGMEC = 4010   THEN
                  ?MSGX := '##取消終了##'
                  SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                  RETURN
              ELSE
                  ?KEKA4 :=  '【バッチ番号入力】'
                  GOTO ABEND
              END
    END

/.##ホーマック情報倉庫チェック##./
SJH8398B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-HCJOHOL4,TOFILE-HCJOHOL4.TOKKLIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-CHKFILE,TOFILE-CHKFILE.TOKKLIB
    CALL      PGM-SJH8398B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TORCD)
    IF        @PGMEC    ^=   0    THEN
               ?KEKA4 :=  '【情報ＤＴ倉庫チェック】'
               GOTO ABEND
    END

/.##リカバリデータコピー##./
PSETPF:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SETPF FILE-CHKFILE.TOKKLIB,TOFILE-HCJOHOF.TOKKLIB,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
               ?KEKA4 :=  '【チェック時エラーＤＴ　コピー】'
               GOTO ABEND
    END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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

    RETURN    PGMEC-@PGMEC
