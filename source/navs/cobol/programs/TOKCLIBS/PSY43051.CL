/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    欠品案内データ送信（アークランド）   *  ./
/. *   JOB-ID      :    PSY43051                             *  ./
/. *   JOB-NAME    :    欠品案内データ配信Ｆ作成（再）       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY43051'
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
    ?JBID    :=   'PSY43051'
    ?CLID    :=   'PSY43051'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '　アークランドサカモト殿、欠品案内データ配信Ｆの'
    ?OPR2  :=  '　再送信を行います。確認して下さい。'
    ?OPR3  :=  '　また，作成完了後，Ｋ６５００側へデータを転送し，'
    ?OPR4  :=  '　最終配信データ作成処理を行います。その後，Ｋ６５'
    ?OPR5  :=  '　００側より配信処理を行って下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##配信データ抽出##./
SSY4305B:

/.  ?STEP :=   'SSY4305B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ARKKEPL2,TOFILE-ARKKEPL2.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-CVCSF,TOFILE-ARKWK.ONLBLIB
    CALL      PGM-SSY4305B.TOKELIB,PARA-(?KENFLG)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '（配信ＤＴ作成）'
              GOTO ABEND END   ./

/.##配信データなし画面表示##./
DATACHK:

/.  ?STEP :=   'DATACHK '
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
 ./
/.##欠品データ－ＧＰ６０００送信処理＃./
K6500SND:
/.
    ?STEP :=   'K6500SND'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃欠品ＤＴ転送　開始＃',TO-XCTL
    SNDFILE COM-KGPATH01.KGNODE01,SFILE-ARKWK.TOKFLIB,
            DFILE-'ARKWK',RL-256,BF-1
    SNDMSG MSG-'＃＃欠品ＤＴ　　転送中＃',TO-XCTL
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'＃＃欠品ＤＴ転送　異常＃',TO-XCTL
              ?KEKA4 :=  '（欠品ＤＴ送信）'
              GOTO ABEND END
    SNDMSG MSG-'＃＃欠品ＤＴ転送　終了＃',TO-XCTL
 ./
/.##ＧＰ６０００側ＪＯＢ起動##./
K6500JOB:
/.
    ?STEP :=   'K6500JOB'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃Ｋジョブ起動　開始＃',TO-XCTL
    SNDRSJJ COM-KGPATH01.KGNODE01,DFILE-'HSARKLD'
    SNDMSG MSG-'＃＃Ｋジョブ　　起動中＃',TO-XCTL
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'＃＃Ｋジョブ起動　異常＃',TO-XCTL
              ?KEKA4 :=  '（最終処理起動）'
              GOTO ABEND END
    SNDMSG MSG-'＃＃Ｋジョブ　起動終了＃',TO-XCTL
./
RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '配信処理メニューより、欠品案内データを'
    ?KEKA3 :=  '送信して下さい。'
    ?KEKA4 :=  '（再配信用データ作成：欠品案内）'
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