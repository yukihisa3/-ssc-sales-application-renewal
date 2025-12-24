# CNI1021B

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/CNI1021B.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   ■サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    CNI10200                             *  ./
/. *   JOB-NAME    :    日次更新                             *  ./
/. *                    日次更新自動更新                     *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'CNI10200'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?MSGOK  ,STRING*2,VALUE-'OK'        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?MSGNG  ,STRING*2,VALUE-'NG'        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
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
    VAR       ?JMSG   ,STRING*50

    DEFLIBL TOKELIB/TOKFLIB
/.##ｼｽﾃﾑ日付取得##./
    ?SYSDATE  :=    @SCDATED
/.##曜日取得##./
    ?YOUBI    :=    %SBSTR(?SYSDATE,13,1)

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '自動日次更新処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP040:
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
              DACTWS WS-WKSTN530,SIGNOFF-@YES
              DACTWS WS-WKSTN531,SIGNOFF-@YES
              DACTWS WS-WKSTN532,SIGNOFF-@YES
              DACTWS WS-WKSTN533,SIGNOFF-@YES
              DACTWS WS-WKSTN534,SIGNOFF-@YES
              DACTWS WS-WKSTN535,SIGNOFF-@YES
              DACTWS WS-WKSTN536,SIGNOFF-@YES
              DACTWS WS-WKSTN550,SIGNOFF-@YES
              DACTWS WS-WKSTN551,SIGNOFF-@YES
              DACTWS WS-WKSTN552,SIGNOFF-@YES
              DACTWS WS-SAKATAPC,SIGNOFF-@YES
              /.## 新ﾈｯﾄﾜｰｸ端末 ﾜｰｸｽﾃｰｼｮﾝ切り離し ##./
              DACTWS WS-WKSTNH6A,SIGNOFF-@YES  /.## 片岡配送ｾﾝﾀｰ ##./
              DACTWS WS-WKSTNH63,SIGNOFF-@YES  /.## ﾌﾊﾞｻﾐ配送ｾﾝﾀｰ##./
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
              DACTWS WS-WKSTNHT5,SIGNOFF-@YES  /.## ｶﾄｰﾚｯｸ       ##./
              DACTWS WS-WKSTNH49,SIGNOFF-@YES  /.## 西日本１     ##./
              DACTWS WS-WKSTNH4A,SIGNOFF-@YES  /.## 西日本２     ##./
              DACTWS WS-WKSTNH6S,SIGNOFF-@YES  /.## 富岡新       ##./
              DACTWS WS-WKSTNH22,SIGNOFF-@YES  /.## 長野ｾﾙﾄｯﾌﾟ   ##./
              DACTWS WS-WKSTNH53,SIGNOFF-@YES  /.## 岡山総合花卉 ##./
              DACTWS WS-WKSTNHT4,SIGNOFF-@YES  /.## 花の海       ##./
              DACTWS WS-WKSTNH6B,SIGNOFF-@YES  /.## 片岡増山さん ##./


/.##データバックアップ##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**ﾗｲﾌﾞﾗﾘﾊﾞｯｸｱｯﾌﾟ',TO-XCTL
/.  SAVLIB LIB-TOKFLIB/ONLBLIB,TODEV-DAT,ADD-@NO,REWIND-@NO,
           MODE-@USED
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【ライブラリバックアップ】'
         GOTO ABEND
    END
./

    CPYFILE FILE-SHTDENF.TOKFLIB,TOFILE-SHTDENF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－売上伝票ファイル】'
         GOTO ABEND
    END

    CPYFILE FILE-ZAMZAIF.TOKFLIB,TOFILE-ZAMZAIF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－在庫マスタ　　　】'
         GOTO ABEND
    END

    CPYFILE FILE-HSHOTBL.TOKFLIB,TOFILE-HSHOTBL.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－商品変換ＴＢＬ　】'
         GOTO ABEND
    END

    CPYFILE FILE-HJYOKEN.TOKFLIB,TOFILE-HJYOKEN.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－条件ファイル　　】'
         GOTO ABEND
    END

    CPYFILE FILE-NYKFILF.TOKFLIB,TOFILE-NYKFILF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－入庫ファイル　　】'
         GOTO ABEND
    END

    CPYFILE FILE-HACHEDF.TOKFLIB,TOFILE-HACHEDF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－発注ヘッダ　　　】'
         GOTO ABEND
    END

    CPYFILE FILE-HACMEIF.TOKFLIB,TOFILE-HACMEIF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－発注明細　　　　】'
         GOTO ABEND
    END

    CPYFILE FILE-NYSFILF.TOKFLIB,TOFILE-NYSFILF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－入出庫ファイル　】'
         GOTO ABEND
    END

    CPYFILE FILE-SGYFILF.TOKFLIB,TOFILE-SGYFILF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－作業実績ファイル】'
         GOTO ABEND
    END

    CPYFILE FILE-JHSHENF.TOKFLIB,TOFILE-JHSHENF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－当日伝票ファイル】'
         GOTO ABEND
    END

    CPYFILE FILE-RUISEKF.TOKFLIB,TOFILE-RUISEKF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－累積ファイル　　】'
         GOTO ABEND
    END

    CPYFILE FILE-JISSYUF.TOKFLIB,TOFILE-JISSYUF.DATBAKLB,
            CRTFILE-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【バックアップ－累積集計ファイル】'
         GOTO ABEND
    END

/.##資源獲得##./
STEP090:

    ?STEP :=   'STEP090 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**資源獲得',TO-XCTL
    ASSIGN LIB-TOKFLIB!@XCL/TOKWLIB!@XCL
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【資源獲得】'
    DEFLIBL LIBL-TOKFLIB/TOKELIB
    ?OPR1  :=  'どこかの端末でシステム使用中です。'
    ?OPR2  :=  '資源の獲得が出来ません。ＲＥＦＳＹＳコマンドで'
    ?OPR3  :=  '使用中の端末を調査し、使用を中止させて下さい。'
    ?OPR4  :=  '　実行⇒⇒再度、資源の獲得をし、処理を続行します。'
    ?OPR5  :=  '　ＰＦ９⇒日次を中止します。'
         ?JMSG :=  '##端末使用中の倉庫があります。   ##'
         SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
         ?JMSG :=  '##端末の使用を止めて下さい。     ##'
         SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
         ?JMSG :=  '##端末 SIGNOFF を行なって下さい。##'
         SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
         ?JMSG :=  '##１５分後に再度実行します。     ##'
         SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
         CALL TWAIT900.XUCL
         GOTO STEP090
    END

/.#################################################################./
/.##在庫引落（条件マスタの日次日付により在庫引当）               ##./
/.#################################################################./
STEP100:

    ?STEP :=   'TNI0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DENJNL6,TOFILE-SHTDENL6.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-SHOTBL2,TOFILE-SHOTBL2.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-TNI0020B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【在庫引落】'
              GOTO ABEND END

/.#################################################################./
/.##売上更新（条件マスタの日次日付により売上計上データ抽出）     ##./
/.#################################################################./
STEP110:

    ?STEP :=   'SNI0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DENJNL4,TOFILE-SHTDENL4.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKU.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-TNI0030B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【売上更新】'
              GOTO ABEND END

/.#################################################################./
/.##データＣＯＰＹの為、３分待ち合わせ後、ＣＮＶＦＩＬＥ         ##./
/.#################################################################./
CPY:
    /.##30秒へ変更##./
    SNDMSG MSG-'データＣＯＰＹ待合せ中',TO-XCTL
    CALL TWAIT30.XUCL

    CNVFILE FILE-TOKU.TOKFLIB,TOFILE-TOKUWK.TOKWLIB,BF-4
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【ＤＴコピー】'
              GOTO ABEND END

/.#################################################################./
/.##売上計上データ確認リスト出力                                 ##./
/.#################################################################./
STEP120:

    ?STEP :=   'SNI0040L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-TOKU1,TOFILE-TOKU1.TOKFLIB
    OVRF      FILE-TOKMS3,TOFILE-TOKMS3.TOKFLIB
    CALL      PGM-SNI0040L.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【売上計上リスト】'
              GOTO ABEND END

/.#################################################################./
/.##入庫データ計上データ作成                                     ##./
/.#################################################################./
STEP130:

    ?STEP :=   'SZA0141B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYKFILF,TOFILE-NYKFILL1.TOKFLIB
    OVRF      FILE-HACHEDF,TOFILE-HACHEDL1.TOKFLIB
    OVRF      FILE-HACMEIF,TOFILE-HACMEIL1.TOKFLIB
    OVRF      FILE-SHOTBL4,TOFILE-SHOTBL4.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUZ.TOKFLIB
    CALL      PGM-SZA0141B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【入庫計上】'
              GOTO ABEND END

/.#################################################################./
/.##入出庫／作業実績データ　計上データ作成                       ##./
/.#################################################################./
STEP140:

    ?STEP :=   'SZA0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYSFILF,TOFILE-NYSFILL1.TOKFLIB
    OVRF      FILE-SGYFILF,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUZ.TOKFLIB
    CALL      PGM-SZA0202B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【入出庫／作業計上】'
              GOTO ABEND END

/.#################################################################./
/.##入庫／入出庫／作業実績データ　計上確認リスト発行             ##./
/.#################################################################./
STEP150:

    ?STEP :=   'SNI0050B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKUZ.TOKFLIB
    CALL      SNI0050B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【在庫系確認リスト】'
              GOTO ABEND END

/.#################################################################./
/.##売上／入庫／入出庫／作業実績データ－＞ワークバックアップ     ##./
/.#################################################################./
STEP160:

    ?STEP :=   'STEP160 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-TOKUWK.TOKWLIB,TOFILE-TOKU.TOKWLIB,ADD-@NO,
            BF-4
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【計上ＤＴ退避１】'
              GOTO ABEND END

STEP170:

    ?STEP :=   'STEP170 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-TOKUZ.TOKFLIB,TOFILE-TOKU.TOKWLIB,BF-4
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【計上ＤＴ退避２】'
              GOTO ABEND END

/.#################################################################./
/.##売上中間ファイルクリア                                       ##./
/.#################################################################./
    CLRFILE JHSHENF.TOKFLIB

/.#################################################################./
/.##売上／仕入／在庫計上データより実績累積Ｆ作成                 ##./
/.#################################################################./
/.##実績累積Ｆ作成##./
STEP180:

    ?STEP :=   'STEP180 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 実績累積Ｆ　作成 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKU.TOKWLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    CALL      PGM-SJS0010B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【実績累積Ｆ作成】'
              GOTO ABEND END

/.#################################################################./
/.##実績累積Ｆより、実績集計Ｆ集計                               ##./
/.#################################################################./
/.##実績集計ファイル作成                                        ./
STEP190:
    ?STEP :=   'STEP190 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 実績集計Ｆ　集計 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RUISEKL2,TOFILE-RUISEKL2.TOKFLIB
    OVRF      FILE-JISSYUL1,TOFILE-JISSYUL1.TOKFLIB
    CALL      PGM-SJS0030B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【実績累積Ｆ集計】'
              GOTO ABEND END

/.#################################################################./
/.##カーマ商管センター対応                                       ##./
/.#################################################################./
/.##未出庫数再計算##./
STEP200:
    ?STEP :=   'STEP200 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## カーマ商管センター対応 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-DJSYUKL5,TOFILE-DJSYUKL5.TOKKLIB
    OVRF      FILE-DJSYUKWK,TOFILE-DJSYUKWK.TOKKLIB
    CALL      PGM-SSY8798B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【カーマ商管センター対応】'
              GOTO ABEND END

/.#################################################################./
/.##未出庫数再計算                                               ##./
/.#################################################################./
/.##未出庫数再計算##./
STEP201:
    ?STEP :=   'STEP201 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 未出庫数　再計算 ##'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-PTA01000.TOKELIB

/.#################################################################./
/.##ジュンテンドーＴＣ分　未出庫数　再計算　　　                 ##./
/.#################################################################./
/.##未出庫数再計算##./
STEP205:
    ?STEP :=   'STEP205 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## ＴＣ　未出庫数　再計算 ##'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-PJT01810.OSKELIB

/.#################################################################./
/.##オンラインデータバックアップ                                 ##./
/.#################################################################./
/.##オンラインデータバックアップ##./
STEP210:

    ?STEP :=   'STEP200 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## ｵﾝﾗｲﾝﾃﾞｰﾀﾊﾞｯｸｱｯﾌﾟ ##'
    SNDMSG    ?MSGX,TO-XCTL

    SAVFILE FILE-BACK0128.ONLBLIB/BACK0256.ONLBLIB/
            BACK2048.ONLBLIB/BACK0264.ONLBLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【オンラインＤＴバックアップ】'
              GOTO ABEND
    ELSE
              CLRFILE BACK0128.ONLBLIB
              CLRFILE BACK0256.ONLBLIB
              CLRFILE BACK2048.ONLBLIB
              CLRFILE BACK0264.ONLBLIB
    END

/.#################################################################./
/.##端末ログ採取　　　　　　　　　　　                           ##./
/.#################################################################./
/.##端末ログ採取　　　　　　　　##./
STEP215:

    ?STEP :=   'STEP215 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## ﾀﾝﾏﾂﾛｸﾞｻｲｼｭ ##'
    SNDMSG    ?MSGX,TO-XCTL

    CALL PLG00300.TOKCLIBO

    SAVLIB LIB-WSLOGLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【端末操作ログ採取】'
              GOTO ABEND
    END

/.#################################################################./
/.##アクセスログ出力（ＤＳＰＳＬＯＧ）                           ##./
/.#################################################################./
STEP216:

    ?STEP :=   'STEP216 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## ｱｸｾｽﾛｸﾞｼｭﾂﾘｮｸ ##'
    SNDMSG    ?MSGX,TO-XCTL

    CALL PSYSLOGN.TOKCLIBO,PARA-('1')

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【アクセスログ出力】'
              GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

/.##正常セット##./
    ?KEKA1 :=  '日次更新処理が正常終了しました。'
    ?KEKA2 :=  '日次更新処理リストを確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    /.##資源の解放##./
    RELEASE LIB-TOKFLIB!@XCL/TOKWLIB!@XCL
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
              ACTWS WS-WKSTN530
              ACTWS WS-WKSTN531
              ACTWS WS-WKSTN532
              ACTWS WS-WKSTN533
              ACTWS WS-WKSTN534
              ACTWS WS-WKSTN535
              ACTWS WS-WKSTN536
              ACTWS WS-WKSTN550
              ACTWS WS-WKSTN551
              ACTWS WS-WKSTN552
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
              ACTWS WS-WKSTNH49      /.## 西日本１           ##./
              ACTWS WS-WKSTNH4A      /.## 西日本２           ##./
              ACTWS WS-WKSTNH6S      /.## 富岡新             ##./
              ACTWS WS-WKSTNH22      /.## 長野ｾﾙﾄｯﾌﾟ         ##./
              ACTWS WS-WKSTNH53      /.## 岡山総合花卉       ##./
              ACTWS WS-WKSTNHT4      /.## 花の海             ##./
              ACTWS WS-WKSTNH6B      /.## 片岡増山さん       ##./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

/.##異常セット##./
    ?KEKA1 :=  '日次更新処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    /.##資源の解放##./
    RELEASE LIB-TOKFLIB!@XCL/TOKWLIB!@XCL
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
              ACTWS WS-WKSTN530
              ACTWS WS-WKSTN531
              ACTWS WS-WKSTN532
              ACTWS WS-WKSTN533
              ACTWS WS-WKSTN534
              ACTWS WS-WKSTN535
              ACTWS WS-WKSTN536
              ACTWS WS-WKSTN550
              ACTWS WS-WKSTN551
              ACTWS WS-WKSTN552
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
              ACTWS WS-WKSTNH49      /.## 西日本１           ##./
              ACTWS WS-WKSTNH4A      /.## 西日本２           ##./
              ACTWS WS-WKSTNH6S      /.## 富岡新             ##./
              ACTWS WS-WKSTNH22      /.## 長野ｾﾙﾄｯﾌﾟ         ##./
              ACTWS WS-WKSTNH53      /.## 岡山総合花卉       ##./
              ACTWS WS-WKSTNHT4      /.## 花の海             ##./
              ACTWS WS-WKSTNH6B      /.## 片岡増山さん       ##./

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

```
