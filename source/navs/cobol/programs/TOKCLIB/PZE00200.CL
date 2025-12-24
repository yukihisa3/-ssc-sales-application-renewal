/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    全社在庫管理                         *  ./
/. *   JOB-ID      :    PZE00200                             *  ./
/. *   JOB-NAME    :    支店／営業所　統合　                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZE00200'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '本社計上データ送信処理'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.##実行確認##./
JIKKOUK:

    ?STEP :=   'JIKKOUK '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　【全社在庫データ統合処理】'
    ?OPR2  :=  ''
    ?OPR3  :=  '　全支店／営業所の在庫データ統合処理を行います'
    ?OPR4  :=  '　確認して下さい。'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【実行確認画面】'
              GOTO ABEND END

/.##全社在庫ﾏｽﾀﾃﾞｰﾀｸﾘｱ##./
ZAIKOCLR:

    ?STEP :=   'ZAIKOCLR'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '## 全社在庫ﾃﾞｰﾀ初期化 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CLRFILE ZENZAIF.TOKPLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【全社在庫ﾃﾞｰﾀ初期化】'
              GOTO ABEND END


/.##在庫ﾃﾞｰﾀ抽出##./
ZAIKODT:

    ?STEP :=   'ZAIKODT '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '## 在庫ﾃﾞｰﾀ抽出中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    OVRF FILE-ZAMZAIF,TOFILE-ZAMZAIF.TOKFLIB
    OVRF FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    OVRF FILE-ZENSYASF,TOFILE-ZENSYAHO.TOKPLIB
/.  CALL SZE0010B.TOKELIB ./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【在庫ﾃﾞｰﾀ抽出処理】'
              GOTO ABEND END

/.##在庫マスタ統合##./
SZE0020B:

    ?STEP :=   'SZE0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '## 本社自在庫ﾏｽﾀ出力中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    OVRF FILE-ZENZAIF,TOFILE-ZENZAIF.TOKPLIB
    OVRF FILE-HOU,TOFILE-ZENSYAHO.TOKPLIB
    OVRF FILE-FKU,TOFILE-ZENSYAKY.TOKFLIB
    OVRF FILE-SEU,TOFILE-ZENSYASE.TOKFLIB
    OVRF FILE-OKU,TOFILE-ZENSYAOK.TOKFLIB
    OVRF FILE-HKU,TOFILE-ZENSYAHK.TOKFLIB
    OVRF FILE-OSU,TOFILE-ZENSYAOS.TOKFLIB
    CALL PGM-SZE0020B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【全社在庫ﾃﾞｰﾀ統合】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '全社在庫データの統合処理が終了しました。'
    ?KEKA2 :=  '在庫照会／在庫表等の実行が可能です。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '在庫データ統合処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを出力し、ＮＡＶまで連絡して'
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