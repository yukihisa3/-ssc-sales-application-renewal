# D365SEG2

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/D365SEG2.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　Ｄ３６５連携機能構築                  *  ./
/. *   SYSTEM-NAME :    D365送受信制御　　　　               *  ./
/. *   JOB-ID      :    D365SEG2                             *  ./
/. *   JOB-NAME    :    Ｄ３６５送受信実行処理               *  ./
/. ***********************************************************  ./
    PGM (P1-?JIKONO,
         P2-?SIJINO,
         P3-?JSKBN,
         P4-?SKKBN,
         P5-?DTSYU,
         P6-?JDATE,
         P7-?JIKAN,
         P8-?JFILID,
         P9-?JFILLIB,
         P10-?MAEPG,
         P11-?MAELIB,
         P12-?ATOPG,
         P13-?ATOLIB,
         P14-?JTANCD)
 /.      P14-?JTANCD,
         P15-?JIKOKEKA)
 ./
/.##パラメタ定義##./
    PARA ?JIKONO   ,STRING*7,IN,VALUE-'       '  /.実行ＮＯ./
    PARA ?SIJINO   ,STRING*10,IN,VALUE-'          ' /.指示ＮＯ./
    PARA ?JSKBN    ,STRING*1,IN,VALUE-' '        /.自動手動区分./
    PARA ?SKKBN    ,STRING*1,IN,VALUE-' '        /.送受信区分./
    PARA ?DTSYU    ,STRING*2,IN,VALUE-'  '       /.データ種別./
    PARA ?JDATE    ,STRING*8,IN,VALUE-'        ' /.指示日付./
    PARA ?JIKAN    ,STRING*6,IN,VALUE-'      '   /.指示時刻./
    PARA ?JFILID   ,STRING*8,IN,VALUE-'        ' /.ファイル名./
    PARA ?JFILLIB  ,STRING*8,IN,VALUE-'        ' /.ＬＩＢ名./
    PARA ?MAEPG    ,STRING*8,IN,VALUE-'        ' /.前ＰＧ名./
    PARA ?MAELIB   ,STRING*8,IN,VALUE-'        ' /.前ＬＩＢ名./
    PARA ?ATOPG    ,STRING*8,IN,VALUE-'        ' /.後ＰＧ名./
    PARA ?ATOLIB   ,STRING*8,IN,VALUE-'        ' /.後ＬＩＢ名./
    PARA ?JTANCD   ,STRING*2,IN,VALUE-'  '       /.担当者./
    VAR  ?JIKOKEKA ,STRING*4,VALUE-'    '    /.処理結果./
/.##ワーク定義##./
    VAR  ?PGMEC      ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX     ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM      ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSGX       ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSG        ,STRING*99                  /.ﾒｯｾｰｼﾞ標示用./
    VAR  ?PGMID      ,STRING*8,VALUE-'D365SEG2'  /.PROGRAM-ID./
    VAR  ?STEP       ,STRING*8,VALUE-'        '  /.STEP-ID./
    VAR  ?PGNM       ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1      ,STRING*40                  /.      2    ./
    VAR  ?KEKA2      ,STRING*40                  /.      3    ./
    VAR  ?KEKA3      ,STRING*40                  /.      4    ./
    VAR  ?KEKA4      ,STRING*40                  /.      5    ./
    VAR  ?DTKENSU    ,STRING*3,VALUE-'   '       /.回線ﾁｪｯｸ結果./
    VAR  ?JIKOKBN    ,STRING*1,VALUE-' '         /.実行区分./
    VAR  ?SEIGYOKB   ,STRING*1,VALUE-' '         /.制御区分./
    VAR  ?KEKAKBN    ,STRING*4,VALUE-'    '      /.結果区分./
    VAR  ?SYORIKEK   ,STRING*1,VALUE-' '         /.処理結果./
    VAR  ?KEKAKBN1   ,STRING*4,VALUE-'    '      /.結果区分./
    VAR  ?DJIKAN     ,STRING*6,VALUE-'      '    /.結果区分./
    VAR  ?LIB        ,STRING*8,VALUE-'D365DLIB'  /.ﾗｲﾌﾞﾗﾘ名./
    VAR  ?LIBN       ,NAME                       /.ﾗｲﾌﾞﾗﾘ名./
    VAR  ?SNDFIL     ,NAME                       /.ﾊﾟﾗﾒﾀﾌｧｲﾙ名./
    VAR  ?SNDLIB     ,NAME!MOD                   /.ﾊﾟﾗﾒﾀﾗｲﾌﾞﾗﾘ名./
    VAR  ?SNDLIBN    ,STRING*17                  /.ﾊﾟﾗﾒﾀﾗｲﾌﾞﾗﾘ名./
    VAR  ?HMAEPG     ,NAME                       /.ﾊﾟﾗﾒﾀﾌｧｲﾙ名./
    VAR  ?HMAELIB    ,NAME!MOD                   /.ﾊﾟﾗﾒﾀﾗｲﾌﾞﾗﾘ名./
    VAR  ?HMAELIBN   ,STRING*17                  /.ﾊﾟﾗﾒﾀﾗｲﾌﾞﾗﾘ名./
    VAR  ?HATOPG     ,NAME                       /.ﾊﾟﾗﾒﾀﾌｧｲﾙ名./
    VAR  ?HATOLIB    ,NAME!MOD                   /.ﾊﾟﾗﾒﾀﾗｲﾌﾞﾗﾘ名./
    VAR  ?HATOLIBN   ,STRING*17                  /.ﾊﾟﾗﾒﾀﾗｲﾌﾞﾗﾘ名./
    VAR  ?MAECHK     ,STRING*1,VALUE-' '         /.実行区分./
    VAR  ?ATOCHK     ,STRING*1,VALUE-' '         /.実行区分./
    VAR  ?PGKEKA1    ,STRING*1,VALUE-' '         /.実行区分./
    VAR  ?MTSTATUS   ,STRING*4,VALUE-'    '      /.実行区分./
    VAR  ?MTKEKA     ,STRING*1,VALUE-' '         /.実行区分./
    VAR  ?FGLOOP     ,STRING*1                   /.実行区分./
    VAR  ?BUTANCD    ,STRING*6,VALUE-'      '    /.部門担当者./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'Ｄ３６５送受信実行制御'

/.##プログラム開始メッセージ##./

    ?MSG :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊Ｄ３６５送受信実行制御＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

    ?MSG :=  '$$$$$ SKKBN = '  && ?SKKBN  &&   ' $$$$$'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKSOLIB

    ?JIKOKEKA  :=  '0000'

/.送受信ファイル名を生成./
    ?SNDFIL   := %NAME(?JFILID)            /.名前型に変換./
    ?LIBN     := %NAME(?JFILLIB)           /.名前型に変換./
    ?SNDLIB   := %NCAT(?SNDFIL,?LIBN)      /.名前型の結合./
    ?SNDLIBN  := %STRING(?SNDLIB)          /.文字列型に変換./
    ?MSG := '## 送受信Ｆ = ' && ?SNDLIBN  &&'##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES /.画面に出力./

    ?BUTANCD  := '2920' && ?JTANCD

/.前処理ＰＧ名を生成./
    IF  ?MAEPG  =  '        ' THEN
        ?MAECHK  :=  ' '
    ELSE
        ?MAECHK  :=  '1'
        ?HMAEPG   := %NAME(?MAEPG)             /.名前型に変換./
        ?LIBN     := %NAME(?MAELIB)            /.名前型に変換./
        ?HMAELIB  := %NCAT(?HMAEPG,?LIBN)      /.名前型の結合./
        ?HMAELIBN := %STRING(?HMAELIB)         /.文字列型に変換./
        ?MSG := '## 前処理PG = ' && ?HMAELIBN  &&'##'
        SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES /.出力./
    END
/.後処理ＰＧ名を生成./
    IF  ?ATOPG  =  '        ' THEN
        ?ATOCHK  :=  ' '
    ELSE
        ?ATOCHK  :=  '1'
        ?HATOPG   := %NAME(?ATOPG)             /.名前型に変換./
        ?LIBN     := %NAME(?ATOLIB)            /.名前型に変換./
        ?HATOLIB  := %NCAT(?HATOPG,?LIBN)      /.名前型の結合./
        ?HATOLIBN := %STRING(?HATOLIB)         /.文字列型に変換./
        ?MSG := '## 後処理PG = ' && ?HATOLIBN  &&'##'
        SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES /.出力./
    END

/.##パラメタファイル作成##./
NVS0130B:

    ?STEP :=   'NVS0130B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CALL PGM-NVS0130B.TOKSOLIB,PARA-(?JIKONO,?SIJINO,?SKKBN,?JSKBN,
                                     ?DTSYU,?JDATE,?JIKAN,?JTANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
         ?KEKA4 := '【パラメタファイル作成】'
         ?KEKAKBN :=  '2001'
        GOTO  ABEND
    END

/.##前処理実行判定##./
MAEPGCHK:

    ?STEP :=   'MAEPGCHK'
    ?MSG :=  '***   '  && ?STEP   &&  '   ***'
    SNDMSG   ?MSG,TO-XCTL

    SNDMSG MSG-'＊前処理実行チェック＊',TO-XCTL
    IF  ?MAECHK  =  '1'   THEN
        CALL ?HMAELIB
             ?PGMEC    :=    @PGMEC
             ?PGMEM    :=    @PGMEM
             IF  ?PGMEC ^= 0 THEN
                 ?KEKA4 := '【前処理実行】'
                 ?KEKAKBN :=  '2002'
                 GOTO  ABEND
             END
    END

/.##ＴＲＩＧＧＥＲファイル作成##./
NVS0030B:

    ?STEP :=   'NVS0030B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    SNDMSG MSG-'＊ＴＲＩＧＧＥＲファイル作成＊',TO-XCTL
    CALL PGM-NVS0030B.TOKSOLIB,PARA-(?PGKEKA1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【ＴＲＩＧＧＥＲファイル作成】'
        ?KEKAKBN :=  '2003'
        GOTO  ABEND
    END
    IF  ?PGKEKA1 =  '1'  THEN
        ?KEKAKBN :=  '2004'
        GOTO  ABEND
    END

/.##ＦＴＰコネクト##./
PCNTFTPC:

    ?STEP :=   'PCNTFTPC'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    SNDMSG MSG-'＊ＦＴＰコネクト＊',TO-XCTL
    CNTFTPC RMTADR-'172.17.5.125',USER-'RENKEI',PASSWORD-'R@NK@I365'
/.  CNTFTPC RMTNAME-D365SVR,USER-'TEST',PASSWORD-'TEST'
./  ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【ＦＴＰコネクト　ＣＮＴＦＴＰＣ】'
        ?KEKAKBN :=  '2005'
        GOTO  ABEND
    END

/.##ファイル送信##./
PSNDFILE:

    ?STEP :=   'PSNDFILE'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

/.  IF  ?SKKBN  =  '2'  THEN  ./
    IF  ?SKKBN  =  '1'  THEN
        SNDMSG MSG-'＊ファイル送信　有＊',TO-XCTL
        SNDFTPC ENT-?SNDLIB,TOFILE-?JFILID,
                DTYPE-@BINARY
        ?PGMEM    :=    @PGMEM
        IF  ?PGMEC ^= 0 THEN
            ?KEKA4 := '【ファイル送信　ＳＮＤＦＴＰＣ】'
            ?KEKAKBN :=  '2006'
            GOTO  ABEND
        END
     ELSE
        SNDMSG MSG-'＊ファイル送信　無＊',TO-XCTL
     END

/.##ＴＲＩＧＧＥＲファイル送信##./
PTRIGGER:

    ?STEP :=   'PTRIGGER'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

        SNDMSG MSG-'＊ＴＲＩＧＧＥＲファイル送信＊',TO-XCTL
        SNDFTPC ENT-SNDTRIGG.D365DLIB,TOFILE-'SNDTRIGG',
                DTYPE-@BINARY
        IF  ?PGMEC ^= 0 THEN
            ?KEKA4 := '【ＴＲＩＧＧＥＲファイル送信】'
            ?KEKAKBN :=  '2007'
            GOTO  ABEND
        END

/.##ＡＮＳＥＲファイル初期化##./
PANSRCLR:

    ?STEP :=   'PANSRCLR'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

        SNDMSG MSG-'＊ＡＮＳＥＲファイル初期化＊',TO-XCTL
        CLRFILE FILE-SNDANSER.D365DLIB
        IF  ?PGMEC ^= 0 THEN
            ?KEKA4 := '【ＡＮＳＥＲファイル初期化】'
            ?KEKAKBN :=  '2007'
            GOTO  ABEND
        END

/.##ＡＮＳＥＲファイル待ち受け##./
NVS0050B:

    ?FGLOOP  := '0'

    WHILE  ?FGLOOP = '0'
      DO
        CALL WAITPG.TOKELIB,PARA-('000030')  /. 30秒待ち ./

        ?STEP := 'NVS0050B'
        ?MSG := '***   '  && ?STEP   &&   '   ***'
        SNDMSG   ?MSG,TO-XCTL

        CALL PGM-NVS0050B.TOKSOLIB,
             PARA-(?MTSTATUS,?MTKEKA)

        ?PGMEC := @PGMEC
        ?PGMEM := @PGMEM
        IF  ?PGMEC ^= 0 THEN  /.PG実行異常./
            ?KEKAKBN :=  '2008'
            ?FGLOOP := 'E'
            BREAK
        END

        IF  ?MTKEKA = ' ' THEN  /.受信済み./
            IF  ?MTSTATUS  ^=  '0000'  THEN
                ?KEKAKBN :=  ?MTSTATUS
            ELSE
                ?KEKAKBN :=  '0000'
            END
            ?FGLOOP  :=  'E'
            BREAK
        END
    END
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
         ?KEKA4 := '【ＡＮＳＥＲファイル待受チェック】'
         ?KEKAKBN :=  '2009'
        GOTO  ABEND
    END

/.##後処理実行判定##./
ATOPGCHK:

    ?STEP :=   'ATOPGCHK'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSG,TO-XCTL

    SNDMSG MSG-'＊後処理実行チェック＊',TO-XCTL
    IF  ?ATOCHK  =  '1'   THEN
        CALL ?HATOLIB,PARA-(?SIJINO,?SKKBN,?JSKBN,?DTSYU,?JDATE,
             ?JIKAN,?JFILID,?JFILLIB,?BUTANCD,?MTSTATUS)
             ?PGMEC    :=    @PGMEC
             ?PGMEM    :=    @PGMEM
             IF  ?PGMEC ^= 0 THEN
                 ?KEKA4 := '【後処理実行】'
                 ?KEKAKBN :=  '2002'
                 GOTO  ABEND
             END
    END

/.##ＦＴＰコネクト解除##./
CNTFTPOF:

    ?STEP :=   'CNTFTPOF'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSG,TO-XCTL

    SNDMSG MSG-'＊ＦＴＰコネクト解除１＊',TO-XCTL
    DCNTFTPC

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?JIKOKEKA  :=  ?KEKAKBN
    ?MSG :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    RETURN    PGMEC-@PGMEC

ABEND:
    SNDMSG MSG-'＊（異常）ＦＴＰコネクト解除２＊',TO-XCTL
             DCNTFTPC
             ?PGMEC    :=    @PGMEC
             ?PGMEM    :=    @PGMEM
             IF  ?PGMEC ^= 0 THEN
                 ?KEKA4 := '【（異常）ＦＴＰコネクト解除３】'
                 ?KEKAKBN :=  '2002'
                 GOTO  ABEND
             END
/.##異常セット##./
    ?STEP :=   'NVS0112B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CALL PGM-NVS0110B.TOKSOLIB,PARA-(?JDATE,?DJIKAN,'2',?JIKONO,
                                     ?SKKBN,?KEKAKBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【当日送受信制御ファイル情報更新２】'
        ?KEKAKBN :=  '1005'
        GOTO  ABEND
    END
    ?KEKA1 :=  'Ｄ３６５送受信実行制御が異常終了！！'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSGX(1)  :=    '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSGX(2)  :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSGX(3)  :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSG :=   ?MSGX(?I)
               SNDMSG    ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END
    ?JIKOKEKA  :=  ?KEKAKBN
    RETURN    PGMEC-@PGMEC

```
