/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    CNI10000                             *  ./
/. *   JOB-NAME    :    自動日次・月次更新処理               *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'CNI10000'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?SYORI1 ,STRING*2,VALUE-'  '        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?SYORI2 ,STRING*2,VALUE-'  '        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?SYORI3 ,STRING*2,VALUE-'  '        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?SYORI4 ,STRING*2,VALUE-'  '        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?SYORI5 ,STRING*2,VALUE-'  '        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?SYORI6 ,STRING*2,VALUE-'  '        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
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
    VAR       ?SYSDATE,STRING*13                  /.ｼｽﾃﾑ日付   ./
    VAR       ?YOUBI  ,STRING*1                   /.曜日       ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '自動日次月次処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKWLIB
              /.リモートワークステーションの終了./
              DACTWS WS-WKSTN501,SIGNOFF-@YES
              DACTWS WS-WKSTN502,SIGNOFF-@YES
              DACTWS WS-WKSTN503,SIGNOFF-@YES
              DACTWS WS-WKSTN504,SIGNOFF-@YES
              DACTWS WS-WKSTN505,SIGNOFF-@YES
              DACTWS WS-WKSTN506,SIGNOFF-@YES
              DACTWS WS-WKSTN507,SIGNOFF-@YES
              DACTWS WS-WKSTN508,SIGNOFF-@YES
              DACTWS WS-WKSTN509,SIGNOFF-@YES
              DACTWS WS-WKSTN510,SIGNOFF-@YES
              DACTWS WS-WKSTN511,SIGNOFF-@YES
              DACTWS WS-WKSTN512,SIGNOFF-@YES
              DACTWS WS-WKSTN513,SIGNOFF-@YES
              DACTWS WS-WKSTN514,SIGNOFF-@YES
              DACTWS WS-WKSTN515,SIGNOFF-@YES
              DACTWS WS-WKSTN516,SIGNOFF-@YES
              DACTWS WS-WKSTN517,SIGNOFF-@YES
              DACTWS WS-WKSTN518,SIGNOFF-@YES
              DACTWS WS-WKSTN519,SIGNOFF-@YES
              DACTWS WS-WKSTN520,SIGNOFF-@YES
              DACTWS WS-WKSTN521,SIGNOFF-@YES
              DACTWS WS-WKSTN522,SIGNOFF-@YES
              DACTWS WS-WKSTN523,SIGNOFF-@YES
              DACTWS WS-WKSTN524,SIGNOFF-@YES
              DACTWS WS-WKSTN525,SIGNOFF-@YES
              DACTWS WS-WKSTN526,SIGNOFF-@YES
              DACTWS WS-WKSTN527,SIGNOFF-@YES
              DACTWS WS-WKSTN528,SIGNOFF-@YES
              DACTWS WS-WKSTN529,SIGNOFF-@YES
              DACTWS WS-SAKATAPC,SIGNOFF-@YES
              /.## 新ﾈｯﾄﾜｰｸ端末 ﾜｰｸｽﾃｰｼｮﾝ切り離し ##./
              DACTWS WS-WKSTNH6A,SIGNOFF-@YES
              DACTWS WS-WKSTNH63,SIGNOFF-@YES
              DACTWS WS-WKSTNH60,SIGNOFF-@YES  /.## 大和倉庫ｾﾝﾀｰ ##./
              DACTWS WS-WKSTNH83,SIGNOFF-@YES  /.## 富岡配送ｾﾝﾀｰ ##./
              DACTWS WS-WKSTNH84,SIGNOFF-@YES  /.## 鴻巣配送ｾﾝﾀｰ ##./
              DACTWS WS-WKSTNH90,SIGNOFF-@YES  /.## 手綱園芸     ##./
              DACTWS WS-WKSTNH86,SIGNOFF-@YES  /.## 西尾植物１   ##./
              DACTWS WS-WKSTNH8A,SIGNOFF-@YES  /.## 西尾植物２   ##./
              DACTWS WS-WKSTNHE2,SIGNOFF-@YES  /.## 日立物流     ##./
              DACTWS WS-WKSTNH42,SIGNOFF-@YES  /.## 仙台営業所   ##./
              DACTWS WS-WKSTNH45,SIGNOFF-@YES  /.## 北海道支店   ##./
              DACTWS WS-WKSTNHT9,SIGNOFF-@YES  /.## 蔦井倉庫　   ##./
              DACTWS WS-WKSTNHT5,SIGNOFF-@YES  /.## KATOLEC　   ##./
              DACTWS WS-WKSTNH49,SIGNOFF-@YES  /.## NISINIHON   ##./

/.##日次更新処理##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**自動日次更新処理',TO-XCTL

    CALL CNI10200.TOKCLIBO,PARA-(?SYORI1)
    IF  ?SYORI1  =  'NG'  THEN
        ?PGNM := '【自動日次更新】'
        GOTO   ABEND      END

/.##月次ＤＴ退避##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**自動月次ＤＴ退避',TO-XCTL

    CALL CNI10300.TOKCLIBO,PARA-(?SYORI2)
    IF  ?SYORI2  =  'NG'  THEN
        ?PGNM := '【自動月次ＤＴ退避】'
        GOTO   ABEND      END

/.##自動月次更新##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**自動月次更新処理',TO-XCTL

    CALL CNI10400.TOKCLIBO,PARA-(?SYORI3)
    IF  ?SYORI3  =  'NG'  THEN
        ?PGNM := '【自動月次更新】'
        GOTO   ABEND      END

/.##自動月次更新_##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**自動月次更新_',TO-XCTL

    CALL CNI10500.TOKCLIBO,PARA-(?SYORI4)
    IF  ?SYORI4  =  'NG'  THEN
        ?PGNM := '【自動月次更新_】'
        GOTO   ABEND      END

/.##自動月次更新_##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**自動月次更新_',TO-XCTL

    CALL CNI10600.TOKCLIBO,PARA-(?SYORI5)
    IF  ?SYORI5  =  'NG'  THEN
        ?PGNM := '【自動日次更新】'
        GOTO   ABEND      END

/.##自動月次更新_##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**自動月次更新_',TO-XCTL

    CALL CNI10700.TOKCLIBO,PARA-(?SYORI6)
    IF  ?SYORI6  =  'NG'  THEN
        ?PGNM := '【自動日次更新】'
        GOTO   ABEND      END

/.##プログラム正常終了##./
RTN:

    ?KEKA1 :=  '自動日次更新処理が正常終了しました。'
    ?KEKA2 :=  '正常終了を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
              ACTWS WS-WKSTN501
              ACTWS WS-WKSTN502
              ACTWS WS-SAKATAPC
              ACTWS WS-WKSTN504
              ACTWS WS-WKSTN505
              ACTWS WS-WKSTN506
              ACTWS WS-WKSTN507
              ACTWS WS-WKSTN508
              ACTWS WS-WKSTN509
              ACTWS WS-WKSTN510
              ACTWS WS-WKSTN511
              ACTWS WS-WKSTN512
              ACTWS WS-WKSTN513
              ACTWS WS-WKSTN514
              ACTWS WS-WKSTN515
              ACTWS WS-WKSTN516
              ACTWS WS-WKSTN517
              ACTWS WS-WKSTN518
              ACTWS WS-WKSTN519
              ACTWS WS-WKSTN520
              ACTWS WS-WKSTN521
              ACTWS WS-WKSTN522
              ACTWS WS-WKSTN523
              ACTWS WS-WKSTN524
              ACTWS WS-WKSTN525
              ACTWS WS-WKSTN526
              ACTWS WS-WKSTN527
              ACTWS WS-WKSTN528
              ACTWS WS-WKSTN529
              ACTWS WS-WKSTNH6A      /.## 片岡配送ｾﾝﾀｰ       ##./
              ACTWS WS-WKSTNH63      /.## ﾌﾊﾞｻﾐ配送ｾﾝﾀｰ      ##./
              ACTWS WS-WKSTNH60      /.## 大和倉庫配送ｾﾝﾀｰ   ##./
              ACTWS WS-WKSTNH83      /.## 富岡配送ｾﾝﾀｰ       ##./
              ACTWS WS-WKSTNH84      /.## 鴻巣配送ｾﾝﾀｰ       ##./
              ACTWS WS-WKSTNH90      /.## 手綱園芸           ##./
              ACTWS WS-WKSTNH86      /.## 西尾植物１         ##./
              ACTWS WS-WKSTNH8A      /.## 西尾植物２         ##./
              ACTWS WS-WKSTNHE2      /.## 北上端末           ##./
              ACTWS WS-WKSTNH42      /.## 仙台営端末         ##./
              ACTWS WS-WKSTNH45      /.## 北海道支店端末     ##./
              ACTWS WS-WKSTNHT9      /.## 蔦井倉庫端末       ##./
              ACTWS WS-WKSTNH49      /.## 西日本支店端末     ##./
              ACTWS WS-WKSTNHT5      /.## ｶﾄｰﾚｯｸ岡山端末     ##./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了##./
ABEND:

    ?KEKA1 :=  '自動日次更新処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
              ACTWS WS-WKSTN501
              ACTWS WS-WKSTN502
              ACTWS WS-SAKATAPC
              ACTWS WS-WKSTN504
              ACTWS WS-WKSTN505
              ACTWS WS-WKSTN506
              ACTWS WS-WKSTN507
              ACTWS WS-WKSTN508
              ACTWS WS-WKSTN509
              ACTWS WS-WKSTN510
              ACTWS WS-WKSTN511
              ACTWS WS-WKSTN512
              ACTWS WS-WKSTN513
              ACTWS WS-WKSTN514
              ACTWS WS-WKSTN515
              ACTWS WS-WKSTN516
              ACTWS WS-WKSTN517
              ACTWS WS-WKSTN518
              ACTWS WS-WKSTN519
              ACTWS WS-WKSTN520
              ACTWS WS-WKSTN521
              ACTWS WS-WKSTN522
              ACTWS WS-WKSTN523
              ACTWS WS-WKSTN524
              ACTWS WS-WKSTN525
              ACTWS WS-WKSTN526
              ACTWS WS-WKSTN527
              ACTWS WS-WKSTN528
              ACTWS WS-WKSTN529
              ACTWS WS-WKSTNH6A      /.## 片岡配送ｾﾝﾀｰ       ##./
              ACTWS WS-WKSTNH63      /.## ﾌﾊﾞｻﾐ配送ｾﾝﾀｰ      ##./
              ACTWS WS-WKSTNH60      /.## 大和倉庫配送ｾﾝﾀｰ   ##./
              ACTWS WS-WKSTNH83      /.## 富岡配送ｾﾝﾀｰ       ##./
              ACTWS WS-WKSTNH84      /.## 鴻巣配送ｾﾝﾀｰ       ##./
              ACTWS WS-WKSTNH90      /.## 手綱園芸           ##./
              ACTWS WS-WKSTNH86      /.## 西尾植物１         ##./
              ACTWS WS-WKSTNH8A      /.## 西尾植物２         ##./
              ACTWS WS-WKSTNHE2      /.## 北上端末           ##./
              ACTWS WS-WKSTNH42      /.## 仙台営端末         ##./
              ACTWS WS-WKSTNH45      /.## 北海道支店端末     ##./
              ACTWS WS-WKSTNHT9      /.## 蔦井倉庫端末       ##./
              ACTWS WS-WKSTNH49      /.## 西日本支店端末     ##./
              ACTWS WS-WKSTNHT5      /.## ｶﾄｰﾚｯｸ岡山端末     ##./

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