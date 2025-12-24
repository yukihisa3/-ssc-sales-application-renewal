/. ***********************************************************  ./
/. *   サカタのタネ　   本社システム                         *  ./
/. *   SYSTEM-NAME :    _卸管理                             *  ./
/. *   JOB-ID      :    ZTANASAV                             *  ./
/. *   JOB-NAME    :    情報システム部連携データ作成　       *  ./
/. *                                                         *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. *   UPDATE      :    2019/10/29 INOUE S11301570           *  ./
/. *                                     オフコンクラウド    *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'ZTANASAV'
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
/.##ＰＧ名称セット##./
    ?PGNM     :=     '電算室殿計上ＤＴ作成処理'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?WKSTN    :=      %STRING(@ORGWS)
    SNDMSG ?WKSTN,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKFLIB/TOKELIB
/.##ﾒｯｾｰｼﾞ出力##./
    ?OPR1  :=  '　＃＃＃＃＃＃＃＃＃　_卸管理＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　本社・営業所の_卸ＤＴのバックアップを行います。'
    ?OPR3  :=  ''
    ?OPR4  :=  '　入力／実行でバックアップを開始します。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##本社・営業所_卸ＤＴバックアップ##./
DATABACK:

    ?STEP :=   'DATABACK'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '_卸ＤＴバックアップ'
    /. 2019/10/29 -->
    SAVFILE FILE-ZTANADT.TOKFLIB/TANDFUK.TOKFLIB/
            TANDSEN.TOKFLIB/TANDOKA.TOKFLIB/TANDHOK.TOKFLIB/
            TANDOSA.TOKFLIB,TODEV-LTO,MODE-@USED <-- ./

    SAVFILE FILE-ZTANADT.TOKFLIB,TODEV-@NONE,MODE-@USED,
      /.社内TOPATH-'/NAVPC/ZTANASAV/',REP-@YES,COMPRESS-@YES./
            TOPATH-'/HGNAS/ZTANASAV/',REP-@YES,COMPRESS-@YES

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##_卸ＤＴＦへコピー（本社）##./
TANACPY0:

    ?STEP :=   'TANACPY0'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '計上ＤＴコピー（本社）'
    CNVFILE FILE-ZTANADT.TOKFLIB,TOFILE-TANDHON.TOKFLIB,
            ADD-@NO,BF-3
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##_卸ＤＴ集計（本社）##./
TANACPY1:

    ?STEP :=   'TANACPY1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '計上ＤＴ集計（本社）'
    CNVFILE FILE-TANDHON.TOKFLIB,TOFILE-TANDALL.TOKFLIB,
            ADD-@NO,BF-3
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##_卸ＤＴ集計（岡山）##./
TANACPY2:

    ?STEP :=   'TANACPY2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '計上ＤＴ集計（岡山）'
    CNVFILE FILE-TANDOKA.TOKFLIB,TOFILE-TANDALL.TOKFLIB,
            ADD-@YES,BF-3
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##_卸ＤＴ集計（北海道）##./
TANACPY3:

    ?STEP :=   'TANACPY3'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '計上ＤＴ集計（北海道）'
    CNVFILE FILE-TANDHOK.TOKFLIB,TOFILE-TANDALL.TOKFLIB,
            ADD-@YES,BF-3
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##_卸ＤＴ集計（大阪）##./
TANACPY4:

    ?STEP :=   'TANACPY4'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '計上ＤＴ集計（大阪）'
    CNVFILE FILE-TANDOSA.TOKFLIB,TOFILE-TANDALL.TOKFLIB,
            ADD-@YES,BF-3
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##_卸ＤＴ集計（福岡）##./
TANACPY5:

    ?STEP :=   'TANACPY5'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '計上ＤＴ集計（福岡）'
    CNVFILE FILE-TANDFUK.TOKFLIB,TOFILE-TANDALL.TOKFLIB,
            ADD-@YES,BF-3
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##_卸ＤＴ集計（仙台）##./
TANACPY6:

    ?STEP :=   'TANACPY6'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '計上ＤＴ集計（仙台）'
/.  CNVFILE FILE-TANDSEN.TOKFLIB,TOFILE-TANDALL.TOKFLIB,
            ADD-@YES,BF-3
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
                              ./
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
              GOTO ABEND END

/.##FPDｾｯﾄﾒｯｾｰｼﾞ出力##./
/.  ?OPR1  :=  '　＃＃＃＃＃＃＃＃＃　_卸処理　＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　電算室への計上ＦＰＤを作成します。'
    ?OPR3  :=  '　ＦＰＤをセットして下さい。'
    ?OPR4  :=  '　入力／実行でＦＰＤへのコピーを開始します。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
  ./
/.##特販部全データ＝＞電算室計上コピー##./
TANASAV:

    ?STEP :=   'TANASAV '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＊＊_卸データ複写中＊＊',TO-XCTL

/.  CNVFILE FILE-TANDALL.TOKFLIB,TOFILE-TOKTANAD,DEV-@WSFPD,
            ADD-@NO,BF-12  ./
    CNVFILE FILE-TANDALL.TOKFLIB,TOFILE-TOKTANAD.TOKFLIB,
            ADD-@NO,BF-12
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
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