/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    CNI10200                             *  ./
/. *   JOB-NAME    :    日次更新                             *  ./
/. *                    日次更新自動更新                     *  ./
/. ***********************************************************  ./
    PGM
/.###ﾊﾟﾗﾒﾀ定義####./
    VAR       ?KBN    ,STRING*2,VALUE-'  ' /.OK,NG./
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
    VAR       ?PGKEKAJ,STRING*1,VALUE-' '         /.ﾚﾌﾟﾘｶ結果./
    VAR       ?PGKEKAM,STRING*1,VALUE-' '         /.ﾚﾌﾟﾘｶ結果./
    VAR       ?PGKEKAZ,STRING*1,VALUE-' '         /.ﾚﾌﾟﾘｶ結果./

    DEFLIBL TOKELIB/TOKFLIB
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

/.  CALL PLG00300.TOKCLIBO

    SAVLIB LIB-WSLOGLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【端末操作ログ採取】'
              GOTO ABEND
    END
./
/.#################################################################./
/.##アクセスログ出力（ＤＳＰＳＬＯＧ）                           ##./
/.#################################################################./
STEP216:

    ?STEP :=   'STEP216 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## ｱｸｾｽﾛｸﾞｼｭﾂﾘｮｸ ##'
    SNDMSG    ?MSGX,TO-XCTL

/.  CALL PSYSLOGN.TOKCLIBO,PARA-('1')
./
/.#################################################################./
/.##各実績データ累積処理　　　　　　　　　　　　                 ##./
/.#################################################################./
/.##各実績データ累積処理##./
STEP217:

    ?STEP :=   'STEP217 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 各実績データ累積処理 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKU.TOKWLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-PCJISSL1,TOFILE-PCJISSL1.TOKKLIB
    CALL      PGM-SJS0210B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【各実績データ累積処理】'
              GOTO ABEND END

/.#################################################################./
/.##各実績データレプリカ処理　　　　　　　　　　                 ##./
/.#################################################################./
/.##各実績データレプリカ処理##./
STEP218:

    ?STEP :=   'STEP218 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 各実績データレプリカ処理 ##'
    SNDMSG    ?MSGX,TO-XCTL

/.  CALL REPLJ010.TOKCLIBO,PARA-(?PGKEKAJ)
    IF        ?PGKEKAJ  =   '2'  THEN
              ?KEKA4 := '【各実績データレプリカ処理】'
              GOTO ABEND
    ELSE
              ?MSGX :=  '## 各実績データレプリカ処理（正常） ##'
              SNDMSG    ?MSGX,TO-XCTL
    END
./
/.##マスタレプリカ処理（差分）##./
STEP219:

    ?STEP :=   'STEP219 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## マスタレプリカ処理（差分）##'
    SNDMSG    ?MSGX,TO-XCTL

/.  CALL REPLM010.TOKCLIBO,PARA-(?PGKEKAM)
    IF        ?PGKEKAM   =   '2'  THEN
              ?KEKA4 := '【マスタレプリカ処理（差分）】'
              GOTO ABEND
    ELSE
              ?MSGX :=  '## マスタレプリカ処理（差分）正常 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END
./
/.##在庫マスタレプリカ処理（一括）##./
STEP220:

    ?STEP :=   'STEP220 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 在庫マスタレプリカ処理（一括） ##'
    SNDMSG    ?MSGX,TO-XCTL

/.  CALL REPLZ010.TOKCLIBO,PARA-(?PGKEKAZ)
    IF        ?PGKEKAZ   =   '2'  THEN
              ?KEKA4 := '【在庫マスタレプリカ処理（一括）】'
              GOTO ABEND
    ELSE
              ?MSGX :=  '## 在庫マスタレプリカ処理（一括）正常##'
              SNDMSG    ?MSGX,TO-XCTL
    END
./
/.##プログラム正常終了（資源の開放）##./
RTN:

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