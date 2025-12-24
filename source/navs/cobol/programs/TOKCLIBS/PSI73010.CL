/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ダイユーエイトＷｅｂＥＤＩ対応　　   *  ./
/. *   JOB-ID      :    PSI73010                             *  ./
/. *   JOB-NAME    :    支払データ変換処理　　　　　　　　   *  ./
/. *               :    2013/09/19  日敷追加　　　　　　　   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSI73010'
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

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ダイユーエイト支払データ取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ダイユーエイト支払データ取込変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  'この処理で、支払データの取込が行われます。'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##受信ＤＴ初期化##./
STEP030:

    ?STEP :=  'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DYSIHASF.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
            GOTO ABEND
    END

/.##受信ＤＴ取込処理　ダイユーエイト##./
STEP040:

    ?STEP :=  'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYSIHASF.ONLBLIB,MODE-@ADD,PARA-DAIYU8,UNIT-8
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'ダイユーエイト【受信データ取込処理】'
            GOTO ABEND
    END

/.##受信ＤＴ取込処理　エイトファーム##./
STEP050:

    ?STEP :=  'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYSIHASF.ONLBLIB,MODE-@ADD,PARA-DAIYU8,UNIT-9
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'エイトファーム【受信データ取込処理】'
            GOTO ABEND
    END

/.##受信ＤＴ取込処理　ダイユーＭＡＸ##./
STEP055:

    ?STEP :=  'STEP055 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYSIHASF.ONLBLIB,MODE-@ADD,PARA-DAIYUMAX,UNIT-5
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'ダイユーＭＡＸ【受信データ取込処理】'
            GOTO ABEND
    END

/.##受信ＤＴ取込処理　日敷## 2013/09/19追加 ./
STEP056:

    ?STEP :=  'STEP056 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYSIHASF.ONLBLIB,MODE-@ADD,PARA-DAIYU8N,UNIT-5
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '日敷【受信データ取込処理】'
            GOTO ABEND
    END

/.##受信ＤＴ取込処理　リック資材## 2018/01/26追加 ./
STEP057:

    ?STEP :=  'STEP057 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYSIHASF.ONLBLIB,MODE-@ADD,PARA-DAIYLIC,UNIT-11
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '日敷【受信データ取込処理】'
            GOTO ABEND
    END

/.##受信ＤＴ取込処理　リック植物## 2018/01/26追加 ./
STEP058:

    ?STEP :=  'STEP058 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DYSIHASF.ONLBLIB,MODE-@ADD,PARA-DAIYLIC,UNIT-12
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '日敷【受信データ取込処理】'
            GOTO ABEND
    END

/.##支払データ変換チェック##./
STEP060:

    ?STEP :=  'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG '######################################',TO-XCTL
    SNDMSG '## ダイユーエイト支払データ変換開始 ##',TO-XCTL
    SNDMSG '######################################',TO-XCTL
    OVRF FILE-DYSIHASF,TOFILE-DYSIHASF.ONLBLIB
    OVRF FILE-DYSIHL3,TOFILE-DYSIHL3.TOKKLIB
    CALL SSI7301B.TOKELIBO
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【支払データ変換処理】'
         GOTO ABEND
    END

/.##受信ＤＴクリア処理 ダイユーエイト##./
STEP070:

    ?STEP :=  'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8,UNIT-17
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'ダイユーエイト【受信データクリア処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理 エイトファーム##./
STEP080:

    ?STEP :=  'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8,UNIT-18
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'エイトファーム【受信データクリア処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理 ダイユーＭＡＸ##./
STEP090:

    ?STEP :=  'STEP090 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYUMAX,UNIT-8
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := 'ダイユーＭＡＸ【受信データクリア処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理 日敷## 2013/09/19 追加 ./
STEP100:

    ?STEP :=  'STEP100 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYU8N,UNIT-7
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '日敷【受信データクリア処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理 リック資材## 2018/01/26 追加 ./
STEP110:

    ?STEP :=  'STEP110 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYLIC,UNIT-13
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '日敷【受信データクリア処理】'
            GOTO ABEND
    END

/.##受信ＤＴクリア処理 リック植物## 2018/01/26 追加 ./
STEP120:

    ?STEP :=  'STEP120 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3141.TOKKLIB,PARA-DAIYLIC,UNIT-14
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '日敷【受信データクリア処理】'
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