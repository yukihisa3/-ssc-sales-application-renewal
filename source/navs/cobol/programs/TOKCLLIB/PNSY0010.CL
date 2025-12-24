/. ***********************************************************  ./
/. *   ■サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＤＣＭ仕入先統合　　　　　　　　　   *  ./
/. *   JOB-ID      :    PNSY0010                             *  ./
/. *   JOB-NAME    :    ＤＣＭ発注データ取込＆変換　　　　   *  ./
/. *                                                         *  ./
/. *<修正履歴>************************************************  ./
/. *   2021/02/19 NAV TAKAHASHI 新規作成　　　　　　         *  ./
/. *   XXXX/XX/XX XXXXXXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮ *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PNSY0010'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK3   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK4   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK5   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK6   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK7   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK8   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK9   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK10  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK11  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK12  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK13  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK14  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
/.2018/02/15 NAV ST./
    VAR       ?CHK15  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK16  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK17  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK18  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
/.2018/02/15 NAV ED./
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
    VAR       ?PGCHK4 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK5 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK6 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK7 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK8 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK9 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK10,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK11,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK12,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK13,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK14,STRING*1,VALUE-' '         /.      5    ./
    /.2018/02/15 NAV ST./
    VAR       ?PGCHK15,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK16,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK17,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK18,STRING*1,VALUE-' '         /.      5    ./
    /.2018/02/15 NAV ED./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB/TOKCOLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '新ＤＣＭ仕入先統合　発注取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【新ＤＣＭ仕入先統合　発注変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  'この処理で、各事業所のデータが作成されます。'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
/.  DEFLIBL TOKELIB/TOKFLIB/TOKKLIB
./
/.##受信ＤＴ初期化##./
STEP025:

    ?STEP :=   'STEP025 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-DCMEDIAL.ONLBLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理（資材）##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DCMEDIAL.ONLBLIB,MODE-@ADD,PARA-NEWDCM,
            UNIT-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理（資材）】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理（植物）##./
STEP035:

    ?STEP :=   'STEP035 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-DCMEDIAL.ONLBLIB,MODE-@ADD,PARA-NEWDCM,
            UNIT-2
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理（植物）】'
                    GOTO ABEND
    END

/.##バッチ番号採番処理##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  NSY0010B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ番号採番処理】'
                    GOTO ABEND
    END

/.##受信発注データ取込（基本情報Ｆ作成）##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-DCMEDIAL,TOFILE-DCMEDIAL.ONLBLIB
    CALL NSY0020B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信発注データ取込（基本情報Ｆ作成）】'
                    GOTO ABEND
    END


/.##ＤＣＭ受信件数リスト##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL NSY0030L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【ＤＣＭ受信件数リスト】'
                    GOTO ABEND
    END

/.##発注受信データ変換連続処理##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL NSY0050B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【発注受信データ変換連続処理】'
                    GOTO ABEND
    END


/.##受信ＤＴクリア処理##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3149.TOKKLIB,PARA-NEWDCM,UNIT-3
    FIMPORT FILE-CLR3149.TOKKLIB,PARA-NEWDCM,UNIT-4
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データクリア処理】'
                    GOTO ABEND
    END


/.##プログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '新ＤＣＭ発注取込の連続変換処理が正常終了'
    ?KEKA2 :=  'しました。件数リスト等を確認して下さい。'
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
    ?KEKA1 :=  '新ＤＣＭ発注取込の連続変換処理が異常終了'
    ?KEKA2 :=  'しました。ログ採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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