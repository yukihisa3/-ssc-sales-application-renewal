/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    CNI10200                             *  ./
/. *   JOB-NAME    :    日次更新                             *  ./
/. *                    日次更新自動更新                     *  ./
/. ***********************************************************  ./
    PGM  (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*2,OUT,VALUE-'  ' /.OK,NG./
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

/.##月次用のＤＡＴをセット（日次＋月次退避）##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**ｵｰﾄﾁｪﾝｼﾞｬ自動ｾｯﾄ',TO-XCTL

    SNDMSG MSG-'【月曜日用ｾｯﾄ】',TO-XCTL
    CTLMGZN DEV-DAT,MODE-@LOAD,SELECT-1
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【月曜日用ｾｯﾄ異常】'
         GOTO ABEND
    END

/.##データバックアップ##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**ﾗｲﾌﾞﾗﾘﾊﾞｯｸｱｯﾌﾟ',TO-XCTL
    SAVLIB LIB-TOKFLIB/TOKWLIB/ONLBLIB,TODEV-DAT,ADD-@NO,
           REWIND-@BEFORE,MODE-@USED
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【ライブラリバックアップ】'
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
    ?OPR1  :=  'どこかの端末でシステム使用中です。'
    ?OPR2  :=  '資源の獲得が出来ません。ＲＥＦＳＹＳコマンドで'
    ?OPR3  :=  '使用中の端末を調査し、使用を中止させて下さい。'
    ?OPR4  :=  '　実行⇒⇒再度、資源の獲得をし、処理を続行します。'
    ?OPR5  :=  '　ＰＦ９⇒日次を中止します。'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
         GOTO STEP090
    END

/.##ＤＡＴエジュクト##./
STEP095:

    ?STEP :=   'STEP095 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
/.  SNDMSG MSG-'**ｴｼﾞｭｸﾄ',TO-XCTL
    CTLMGZN DEV-DAT,MODE-@EJECT
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【ＤＡＴエジェクト】'
         GOTO ABEND
    END ./

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

    ?STEP :=   'STEP210 '
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

/.##プログラム正常終了（資源の開放）##./
RTN:

    /.##資源の解放##./
    RELEASE LIB-TOKFLIB!@XCL/TOKWLIB!@XCL
/.##正常セット##./
    ?KBN   :=  'OK'
    ?KEKA1 :=  '日次更新処理が正常終了しました。'
    ?KEKA2 :=  '日次更新処理リストを確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    /.##資源の解放##./
    RELEASE LIB-TOKFLIB!@XCL/TOKWLIB!@XCL
/.##異常セット##./
    ?KBN   :=  'NG'
    ?KEKA1 :=  '日次更新処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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