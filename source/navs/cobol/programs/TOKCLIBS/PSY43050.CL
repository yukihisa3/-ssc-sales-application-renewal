/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    欠品案内データ送信（アークランド）   *  ./
/. *   JOB-ID      :    PSY43050                             *  ./
/. *   JOB-NAME    :    欠品案内データ配信Ｆ作成             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY43050'
    VAR       ?STEP     ,STRING*8
    VAR       ?KENFLG   ,STRING*1,VALUE-' '         /.件数ＦＬＧ ./
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
/.-----------------------------------------------------------./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '欠品案内データ配信Ｆ作成'
/.アークランドサカモト　欠品案内データ配信Ｆ作成処理./
STEP00:
    ?JBID    :=   'PSY43050'
    ?CLID    :=   'PSY43050'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '　アークランドサカモト殿、欠品案内データ配信Ｆの'
    ?OPR2  :=  '　作成を行ないます。確認して下さい。'
    ?OPR3  :=  '　また，作成完了後，Ｋ６５００側へデータを転送し，'
    ?OPR4  :=  '　最終配信データ作成処理を行います。その後，Ｋ６５'
    ?OPR5  :=  '　００側より配信処理を行って下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##配信データ抽出##./
SSY4305B:

    ?STEP :=   'SSY4305B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ARKKEPL2,TOFILE-ARKKEPL2.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-CVCSF,TOFILE-ARKWK.ONLBLIB
    CALL      PGM-SSY4305B.TOKELIB,PARA-(?KENFLG)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '（配信ＤＴ作成）'
              GOTO ABEND END

/.##配信データなし画面表示##./
DATACHK:

    ?STEP :=   'DATACHK '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG ?KENFLG,TO-XCTL
    IF ?KENFLG  ^=  '1'    THEN
      ?OPR1 := '今回は、取引先への欠品案内データはありません。　'
      ?OPR2 := '件数を確認して下さい。　　　　　　　　　　　　　'
      ?OPR3 := '確認後、アークランド殿の欠品が存在するかチェック'
      ?OPR4 := 'して下さい。　　　　　　　　　　　　　　　　　　'
      ?OPR5 := '　　　　　　　　　　　　　　　　　　　　　　　　'
       CALL      OHOM0900.TOKELIB,PARA-
                 (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
       RETURN    PGMEC-@PGMEC
    END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '手動配信処理より、アークランドへ送信'
    ?KEKA3 :=  '処理を行なって下さい！！'
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