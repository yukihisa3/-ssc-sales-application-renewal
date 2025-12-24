/. ***********************************************************  ./
/. *   サカタのタネ　営業第２部　　　　　　　　　　　 　     *  ./
/. *   SYSTEM-NAME :    ＤＣＭ仕入先統合　　　　　　　　　   *  ./
/. *   JOB-ID      :    PDCM0010                             *  ./
/. *   JOB-NAME    :    （自動用）発注受信データ変換連続処理 *  ./
/. *                              　　　　　　　　　　　　　 *  ./
/. *   CREATE      :    2022/05/23 NAV INOUE                 *  ./
/. *   流用元      :    PNSY0010.TOKCLLIB                    *  ./
/. *<修正履歴>************************************************  ./
/. *   XXXX/XX/XX XXXXXXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮ *  ./
/. *   XXXX/XX/XX XXXXXXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮ *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECN ,STRING*8
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PDCM0010'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,NAME
    VAR       ?WS     ,STRING*8,VALUE-'        '
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
    VAR       ?CHK15  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK16  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK17  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK18  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
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
    VAR       ?PGCHK15,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK16,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK17,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK18,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?SZICHK ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?SKBCHK ,STRING*1,VALUE-' '         /.      5    ./

    /.システム日付./
    VAR ?SDATE    ,STRING*08,VALUE-'00000000'
    /.システム時刻 6桁./
    VAR ?STIME6   ,STRING*06,VALUE-'000000'
    /.システム時刻 4桁./
    VAR ?STIME4   ,STRING*04,VALUE-'0000'

    /.更新区分./
    VAR ?KBN      ,STRING*01,VALUE-' '
    /.起動日付./
    VAR ?KDATE    ,STRING*08,VALUE-'00000000'
    /.起動時刻./
    VAR ?KTIME    ,STRING*06,VALUE-'000000'
    /.WKSTN名./
    VAR ?WSID     ,STRING*08,VALUE-'        '
    /.担当者CD./
    VAR ?TANTO    ,STRING*02,VALUE-'  '
    /.処理ＮＯ./
    VAR ?SYORI    ,STRING*10,VALUE-'0000000000'
    /.実行ＮＯ./
    VAR ?JIKKO    ,STRING*10,VALUE-'0000000000'

    /.子実行ＮＯ./
    VAR ?KJIKKO   ,STRING*10,VALUE-'0000000000'
    /.処理名称./
    VAR ?SYORINM  ,STRING*42,
        VALUE-'　　　　　　　　　　　　　　　　　　　　'
    /.実行名称./
    VAR ?JIKKONM  ,STRING*42,
        VALUE-'　　　　　　　　　　　　　　　　　　　　'
    /.処理結果区分./
    VAR ?KEKKA    ,STRING*01,VALUE-' '
    /.終了ステータス./
    VAR ?STATUS   ,STRING*04,VALUE-'    '
    /.区分./
    VAR ?ERKBN    ,STRING*03,VALUE-'   '
    /.バッチ日付./
    VAR ?ERDATE   ,STRING*08,VALUE-'00000000'
    /.バッチ時刻./
    VAR ?ERTIME   ,STRING*04,VALUE-'0000'
    /.バッチ取引先./
    VAR ?ERTKCD   ,STRING*08,VALUE-'00000000'


  /.DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB/TOKCOLIB./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/TOKDTLIB/TOKSOLIB/
            TOKCOLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '（自動用）発注受信データ変換連続処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
/.  SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES ./

/.##確認画面##./
STEP010:
/.
    ?OPR1  :=  '【新ＤＣＭ仕入先統合　発注変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  'この処理で、各事業所のデータが作成されます。'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
 ./

/.##実行履歴用日付時刻・バッチ取得#./
STEP020:

    ?STEP :=   'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  DCM0030B.TOKSOLIB,PARA-(?SDATE,?STIME6,?STIME4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【実行履歴用日付時刻取得】'
                    GOTO ABEND
    END

    ?MSGX :=  '## PARA_システム日付  = ' && ?SDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## PARA_システム時刻6 = ' && ?STIME6
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## PARA_システム時刻4 = ' && ?STIME4
    SNDMSG    ?MSGX,TO-XCTL

    ?HIDUKE  :=  ?SDATE
    ?JIKAN   :=  ?STIME4

/.##実行開始##./
STEP021:

    ?STEP :=   'STEP021 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.##パラメタ初期セット##./
    ?KBN     :=  '1'
    ?KDATE   :=  ?SDATE
    ?KTIME   :=  ?STIME6
    ?WSID    :=  ?WS
    ?TANTO   :=  '98'
    ?SYORI   :=  '0000000001'
    ?JIKKO   :=  '0000000000'
    ?KJIKKO  :=  '0000000000'
    ?SYORINM :=  '新ＤＣＭ発注データ取込＆変換　　　　　　'
    ?JIKKONM :=  '　　　　　　　　　　　　　　　　　　　　'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

/.##受信ＤＴ初期化##./
STEP025:

    ?STEP :=   'STEP025 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?JIKKO   :=  '0000000001'
    ?JIKKONM :=  '新ＤＣＭ発注取込ファイル初期化　　　　　'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    CLRFILE FILE-DCMEDIAL.ONLBLIB

    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM

    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN
              GOTO ABEND
    END

/.##受信ＤＴ取込処理（資材）##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?JIKKO   :=  '0000000002'
    ?JIKKONM :=  'ＤＣＭ資材発注データ取込（ＦＥＸＰＯＲＴ'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    FEXPORT FILE-DCMEDIAL.ONLBLIB,MODE-@ADD,PARA-NEWDCM,
            UNIT-1

    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理（資材）】'
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN
              ?ERKBN   :=  '020'
              ?ERDATE  :=  ?HIDUKE
              ?ERTIME  :=  ?JIKAN
              ?ERTKCD  :=  '00000000'
              CALL ERR0010B.TOKSOLIB,PARA-(?ERKBN,?ERDATE,?ERTIME,
                                           ?ERTKCD)
              GOTO ABEND
    END

/.##受信ＤＴ取込処理（植物）##./
STEP035:

    ?STEP :=   'STEP035 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?JIKKO   :=  '0000000003'
    ?JIKKONM :=  'ＤＣＭ植物発注データ取込（ＦＥＸＰＯＲＴ'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    FEXPORT FILE-DCMEDIAL.ONLBLIB,MODE-@ADD,PARA-NEWDCM,
            UNIT-2

    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理（植物）】'
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN
              GOTO ABEND
    END

/.##受信状態確認##./
STEP036:

    ?STEP :=   'STEP036 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-CTLDCMSZ.TOKDTLIB,MODE-@REP,PARA-NEWDCM,
            UNIT-6
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '受信制御Ｆ取込　資材'
              GOTO ABEND
    END

    FEXPORT FILE-CTLDCMSB.TOKDTLIB,MODE-@REP,PARA-NEWDCM,
            UNIT-7
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '受信制御Ｆ取込　植物'
              GOTO ABEND
    END
    CALL DCM0050L.TOKSOLIB,PARA-(?SZICHK,?SKBCHK)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '受信状況リスト発行'
              GOTO ABEND
    END

/.##受信発注データ取込（基本情報Ｆ作成）##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?JIKKO   :=  '0000000004'
    ?JIKKONM :=  'ＤＣＭ発注基本情報作成　　　　　　　　　'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    OVRF FILE-DCMEDIAL,TOFILE-DCMEDIAL.ONLBLIB
    CALL NSY0020B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信発注データ取込（基本情報Ｆ作成）】'
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN
              GOTO ABEND
    END

/.##ＤＣＭ受信件数リスト##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?JIKKO   :=  '0000000005'
    ?JIKKONM :=  'ＤＣＭ発注取込件数リスト　　　　　　　　'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    CALL NSY0030L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【ＤＣＭ受信件数リスト】'
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN
              GOTO ABEND
    END

/.##発注受信データ変換連続処理##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?JIKKO   :=  '0000000006'
    ?JIKKONM :=  '発注受信データ変換連続処理　　　　　　　'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

  /.CALL NSY0050B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN) ./
    OVRF DCMKENL1,DCMKENL1.TOKDTLIB
    CALL NSY0051B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,
                                 ?JIKKO,?KDATE,?KTIME,?TANTO,?SYORI)
    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【発注受信データ変換連続処理】'
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KDATE,?KTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              ?KEKA4 :=  '実行履歴ファイル更新'
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN
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


/.##プログラム正常終了##./
RTN:

    SNDMSG    '（自動用）発注受信データ変換連続処理　正常終了',
              ,TO-XCTL
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    ?ERKBN   :=  '021'
    ?ERDATE  :=  ?HIDUKE
    ?ERTIME  :=  ?JIKAN
    ?ERTKCD  :=  '00000000'
    CALL ERR0010B.TOKSOLIB,PARA-(?ERKBN,?ERDATE,?ERTIME,
                                 ?ERTKCD)
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '新ＤＣＭ発注取込の連続変換処理が異常終了'
    ?KEKA2 :=  'しました。ログ採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
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

    RETURN    PGMEC-?PGMEC
