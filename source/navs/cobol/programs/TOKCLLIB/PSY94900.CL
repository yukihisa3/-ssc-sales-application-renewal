/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ヨドバシカメラＥＤＩ　　  　　　     *  ./
/. *   JOB-ID      :    PSY94900                             *  ./
/. *   JOB-NAME    :    納期数量一括変更データ取込　　　　　 *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾊﾟﾗﾒﾀ 定義####./
    PGM
/.###ﾜｰｸｴﾘｱ定義###./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY94900'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1./
    VAR ?KEKA1    ,STRING*40                    /.      2./
    VAR ?KEKA2    ,STRING*40                    /.      3./
    VAR ?KEKA3    ,STRING*40                    /.      4./
    VAR ?KEKA4    ,STRING*40                    /.      5./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.倉庫CD./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫CD./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?ERRKBN   ,STRING*1,VALUE-' '           /.エラー区分./
    VAR ?TDATE    ,STRING*8,VALUE-'        '    /.実行日付./
    VAR ?TTIME    ,STRING*6,VALUE-'      '      /.実行時刻./
    VAR ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                  /.      2    ./
    VAR ?OPR3     ,STRING*50                  /.      3    ./
    VAR ?OPR4     ,STRING*50                  /.      4    ./
    VAR ?OPR5     ,STRING*50                  /.      5    ./

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKDTLIB/TOKELIBO

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '納期数量一括変更データ取込'

/.##ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ＥＸＣＥＬ納期数量一括変更データ取込】'
    ?OPR2  :=  'ＥＸＣＥＬより出力された納期数量一括データの'
    ?OPR3  :=  '取込を行ないます。　　　　　　　　　　　　　'
    ?OPR4  :=  '実行ＯＫか確認してください。　　　　　　　　'
    ?OPR5  :=  '（ＥＸＣＥＬ側処理は完了済ですか？）'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)


/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##ﾌｧｲﾙ排他##./
PASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN FILE-RCVKKTEI.TOKDTLIB!@XCL/YODHECKF.TOKDTLIB!
           @XCL/YODHECCV.TOKDTLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で取込処理が行われています',TOWS-@ORGWS
              SNDMSG 'ヨドバシ取込　ファイルロック',TOWS-@ORGWS
              ?KEKA4 :=  '【排他：納期数量変更関係Ｆ】'
              GOTO ABEND
    END

/.##ファイル取込##./
PFEXPORT:

    ?STEP :=   'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-RCVKKTEI.TOKDTLIB,MODE-@REP,PARA-YODOBASI,
            UNIT-7
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '納期数量一括変更ＤＴ取込'
              GOTO ABEND
    END

/.##納期数量一括変更ＤＴ取込＆チェック##./
SSY9490B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSY9490B.TOKSOLIB,PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                                     ?ERRKBN,?TDATE,?TTIME)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '納期数量一括変更ＤＴ取込＆チェック'
              GOTO ABEND
    END

/.##出力ＭＳＧ確認##./
    ?MSGX :=  '# エラー区分　 = '  && ?ERRKBN   &&   '      #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 実行日付　　 = '  && ?TDATE    &&   ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '# 実行時刻　　 = '  && ?TTIME    &&   '   #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##納期数量一括変更ＤＴエラーチェックＣＳＶ出力##./
SSY9491V:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSY9491V.TOKSOLIB,PARA-(?TDATE,?TTIME,?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '納期数量一括変更ＤＴエラーチェック'
              GOTO ABEND
    END

/.##ファイル取込##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-YODHECCV.TOKDTLIB,PARA-YODOBASI,UNIT-8
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '納期数量一括変更ＤＴエラー出力'
              GOTO ABEND
    END

/.##エラー判定##./
PERRCHK:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    IF  ?ERRKBN  =  '1'  THEN
        ?MSGX :=  '# 取込ＤＴにエラーあり #'
        SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
        GOTO   ABEND1
    END

/.##納期数量一括変更ＤＴ更新##./
SSY9492B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSY9492B.TOKSOLIB,PARA-(?TDATE,?TTIME,?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '納期数量一括変更ＤＴ更新'
              GOTO ABEND
    END

RTN:

              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '納期数量一括変更データ取込が正常終了しま'
    ?KEKA2 :=  'した。取込結果を確認して下さい。'
    ?KEKA3 :=  ''
    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '納期数量一括変更データ取込が異常終了しま'
    ?KEKA2 :=  'した。ログリストを出力しＮＡＶへ連絡してZ'
    ?KEKA3 :=  '下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO ?MSGX :=   ?MSG(?I)
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC

ABEND1: /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '納期数量一括変更データ取込が異常終了しま'
    ?KEKA2 :=  'した。ログリストを出力しＮＡＶへ連絡してZ'
    ?KEKA3 :=  '下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO ?MSGX :=   ?MSG(?I)
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC
