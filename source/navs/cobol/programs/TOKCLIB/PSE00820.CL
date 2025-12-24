/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSE00820                             *  ./
/. *   JOB-NAME    :    更新                                 *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. *   UPDATE      :    2020/02/14 TAKAHASHI LTO=>多階層へ   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE00820'
    VAR       ?STEP     ,STRING*8
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR   ?PGNM    ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?KEKA1   ,STRING*40                  /.      2    ./
    VAR   ?KEKA2   ,STRING*40                  /.      3    ./
    VAR   ?KEKA3   ,STRING*40                  /.      4    ./
    VAR   ?KEKA4   ,STRING*40                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '出荷検品－検品済ＤＴホスト更新'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##請求締処理開始メッセージ##./
STEP00:

    ?OPR1  :=  '【処理名称】ケーヨー請求繰越処理'
    ?OPR2  :=  '【ＭＳＧ　】ケーヨー請求繰越処理を行います。'
    ?OPR3  :=  ''
    ?OPR4  :=  ''
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.## 請求ＤＴ退避 ##./
PSAVFILE:

    ?STEP :=   'PSAVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##SAVFILE FILE-SETGKFA.TOKFLIB/SETGKFB.TOKFLIB,TODEV-LTO,
            MODE-@USED##./
    SAVFILE FILE-SETGKFA.TOKFLIB/SETGKFB.TOKFLIB,
            TODEV-@NONE,MODE-@USED,TOPATH-'/HGNAS/PSE00820',
            REP-@YES,COMPRESS-@YES
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【請求ＤＴ退避処理】'
              GOTO ABEND
    END

/.## ケーヨー請求ＤＴ初期化 ##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SETGKFB.TOKFLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【ケーヨー請求ＤＴ削除処理】'
              GOTO ABEND
    END

/.##ケーヨー請求データ抽出##./
SSE0035B:

    ?STEP :=   'SSE0035B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SEIGKFA1,TOFILE-SETGKFA2.TOKFLIB
    OVRF      FILE-SEIGKFB1,TOFILE-SETGKFB2.TOKFLIB
    CALL      PGM-KURIKOSB.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ケーヨー請求ＤＴ抽出処理】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

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
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
