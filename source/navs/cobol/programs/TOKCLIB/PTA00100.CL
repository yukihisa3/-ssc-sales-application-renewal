/. ***********************************************************  ./
/. *     サカタのタネ　在庫システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    _卸　　　　　　　　　　　           *  ./
/. *   JOB-ID      :    PTA00100                             *  ./
/. *   JOB-NAME    :    _卸ファイル作成　                   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PTA00100'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.      5    ./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB
/.##処理開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?PGNM := '_卸データ作成'


    ?OPR1  :=  '　＃＃＃＃＃＃＃＃＃　_卸処理　＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　_卸ファイルの作成を行います。'
    ?OPR3  :=  '　宜しいですか？'
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞'
    ?OPR5  :=  '　　　※※前回分のデータは削除します。※※'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##本社・営業所_卸ＤＴバックアップ##./
DATABACK:

    ?STEP :=   'DATABACK'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SAVFILE FILE-ZTANADT.TOKFLIB/TANDFUK.TOKFLIB/
            TANDSEN.TOKFLIB/TANDOKA.TOKFLIB/TANDHOK.TOKFLIB/
            TANDOSA.TOKFLIB/TANDHON.TOKFLIB,TODEV-@NONE,MODE-@USED,
            TOPATH-'/HGNAS/PTA00100/',REP-@YES,COMPRESS-@YES
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '_卸ＤＴバックアップ'
              GOTO ABEND END

/.##前回分データ削除（本社）##./
DATADEL1:

    ?STEP :=   'DATADEL1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-TANDHON.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '本社前回分削除'
              GOTO ABEND END

/.##前回分データ削除（九州）##./
DATADEL2:

    ?STEP :=   'DATADEL2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-TANDFUK.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '九州前回分削除'
              GOTO ABEND END

/.##_卸データ件数リスト##./
PKY39010:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HON,TOFILE-TANDHON.TOKFLIB
    OVRF      FILE-FUK,TOFILE-TANDFUK.TOKFLIB
    OVRF      FILE-SEN,TOFILE-TANDSEN.TOKFLIB
    OVRF      FILE-OKA,TOFILE-TANDOKA.TOKFLIB
    OVRF      FILE-HOK,TOFILE-TANDHOK.TOKFLIB
    OVRF      FILE-OSA,TOFILE-TANDOSA.TOKFLIB
    CALL      PGM-SKY3901L.TOKELIB,PARA-('1')
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '_卸データ件数リスト'
              GOTO ABEND END

/.##_卸ファイル作成##./
STA0010B:

    ?STEP :=   'STA0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-ZAMZAIL4,TOFILE-ZAMZAIL4.TOKFLIB
    OVRF      FILE-ZTANADT,TOFILE-ZTANADT.TOKFLIB
    CALL      PGM-STA0010B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '本社_卸Ｆ作成'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '原票_を確認し、九州へ連絡して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
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