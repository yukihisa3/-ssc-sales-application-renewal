/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ダイユーエイトＷｅｂＥＤＩ対応　　   *  ./
/. *   JOB-ID      :    PSY73170                             *  ./
/. *   JOB-NAME    :    受領受信データ変換　　　　　　　　   *  ./
/. *   UPDATE      :    2013/09/19 日敷追加                  *  ./
/. *   UPDATE      :    2017.10.06 S11270540                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSY73170'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?HIDUKE ,STRING*8,VALUE-'        '  /.バッチ日付./
    VAR       ?JIKAN  ,STRING*4,VALUE-'    '      /.バッチ時間./
    VAR       ?TOKCD  ,STRING*8,VALUE-'00000000'  /.バッチ得意先./
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
    VAR       ?PGCHK1 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK2 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK3 ,STRING*1,VALUE-' '         /.      5    ./

    DEFLIBL TOKELIBO/TOKELIB/TOKKLIB/TOKFLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ダイユーエイト受領データ変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ダイユーエイトＷｅｂデータ取込変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  'ダイユーエイト、エイトファーム、日敷。　　　'
    ?OPR4  :=  'この処理で、受領データが取り込まれます。　　'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIBO/TOKELIB/TOKKLIB/TOKFLIB/TOKJLIB/BMSFLIB/
            TOKDTLIB/TOKWLIB/TOKSOLIB/ONLBLIB/TOKDLIB

/.##受信ＤＴ初期化##./
STEP030:

    ?STEP :=  'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DYJFILE.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
            GOTO ABEND
    END

/.##ダイユーエイト受信ＤＴ取込処理##./
STEP040:

    ?STEP :=  'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYJFILE.ONLBLIB,MODE-@REP,PARA-DAIYU8,UNIT-5
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ダイユーエイト受信データ取込処理】'
            GOTO ABEND
    END

/.##エイトファーム受信ＤＴ取込処理##./
STEP050:

    ?STEP :=  'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYJFILE.ONLBLIB,MODE-@ADD,PARA-DAIYU8,UNIT-6
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【エイトファーム受信データ取込処理】'
                    GOTO ABEND
    END

/.##ダイユーＭＡＸ受信ＤＴ取込処理##./
STEP055:

    ?STEP :=  'STEP055 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYJFILE.ONLBLIB,MODE-@ADD,PARA-DAIYUMAX,UNIT-4
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ダイユーＭＡＸ受信データ取込処理】'
            GOTO ABEND
    END

/.##日敷き受信ＤＴ取込処理## 2013/09/19↓追加 ./
STEP056:

    ?STEP :=  'STEP056 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYJFILE.ONLBLIB,MODE-@ADD,PARA-DAIYU8N,UNIT-4
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【日敷受信データ取込処理】'
            GOTO ABEND
    END

/.##バッチ_採番処理##./
STEP060:

    ?STEP :=  'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SJH8999B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ_採番処理】'
            GOTO ABEND
    END

/.##受領データ取込##./
STEP070:

    ?STEP :=  'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## ダイユーエイト受領データ変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    OVRF FILE-DYJFILE,TOFILE-DYJFILE.ONLBLIB
    OVRF FILE-DYJRYOF1,TOFILE-DYJRYOF1.TOKKLIB
    OVRF FILE-DYJRYOF2,TOFILE-DYJRYOF2.TOKKLIB
    OVRF FILE-DYJRYOF3,TOFILE-DYJRYOF3.TOKKLIB
    CALL SSY7314B.TOKELIBO
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受領データ変換処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理　ダイユーエイト##./
STEP090:

    ?STEP :=  'STEP090 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8,UNIT-15
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'ダイユーエイト【受信データクリア処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理　エイトファーム##./
STEP091:

    ?STEP :=  'STEP091 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8,UNIT-16
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'エイトファーム【受信データクリア処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理　ダイユーＭＡＸ##./
STEP092:

    ?STEP :=  'STEP092 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYUMAX,UNIT-7
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'ダイユーＭＡＸ【受信データクリア処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理　日敷## 2013/09/19↓追加 ./
STEP093:

    ?STEP :=  'STEP093 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8N,UNIT-7
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '日敷【受信データクリア処理】'
            GOTO ABEND
    END

/.##受領データ共通変換##./
SJR0090B:

    ?STEP :=   'SJR0090B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0090B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信エラーリスト（受領・返品）##./
SJR0220L:

    ?STEP :=   'SJR0220L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0220L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信状況リスト（受領・返品）##./
SJR0210L:

    ?STEP :=   'SJR0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0210L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品データ抽出##./
SJR0280B:

    ?STEP :=   'SJR0280B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0280B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ダイユーエイト変換処理が正常終了しました'
    ?KEKA2 :=  '件数リスト等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ダイユーエイト変換処理が異常終了しました'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
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