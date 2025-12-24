/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＳＯＳ処理　　　　        　　　     *  ./
/. *   JOB-ID      :    POS06400                             *  ./
/. *   JOB-NAME    :    ナフコリカバリ処理                   * ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'POS06400'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?HIDUKE   ,STRING*8,VALUE-'        '    /.ﾊﾞｯﾁNO.:ﾋﾂﾞｹ./
    VAR ?JIKAN    ,STRING*4,VALUE-'    '        /.ﾊﾞｯﾁNO.:ｼﾞｶﾝ./
    VAR ?TORCD    ,STRING*8,VALUE-'        '    /.ﾊﾞｯﾁNO.:ﾄﾘﾋｷｻｷ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ナフコリカバリ－伝票番号重複リカバリ'

/.##ﾊﾞｯﾁNO.入力画面##./
SOS0640I:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SOS0640I.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TORCD)
    IF        @PGMEC    ^=   0    THEN
              IF  @PGMEC = 4010   THEN
                  ?MSGX := '##取消終了##'
                  SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ELSE
                  ?KEKA4 :=  '【バッチ番号入力】'
                  GOTO ABEND
              END
    END

/.##売上中間Ｆ重複チェック##./
SOS0630B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRF      FILE-JHSHENWK,TOFILE-JHSHENWK.TOKFLIB
    CALL      PGM-SOS0630B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TORCD)
    IF        @PGMEC    ^=   0    THEN
               ?KEKA4 :=  '【重複チェック処理】'
               GOTO ABEND
    END

/.##売上中間Ｆ重複チェック##./
PSETUP:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SETPF FILE-JHSHENWK.TOKFLIB,TOFILE-JHSHENF.TOKFLIB,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
               ?KEKA4 :=  '【重複データセット】'
               GOTO ABEND
    END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '再伝票更新処理を実行して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

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
    DSPLOG CLR-@YES,EDT-@JEF

    RETURN    PGMEC-@PGMEC
