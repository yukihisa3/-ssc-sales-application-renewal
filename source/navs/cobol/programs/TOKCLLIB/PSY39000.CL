/. ***********************************************************  ./
/. *   サカタのタネ　　本社システム　                        *  ./
/. *   SYSTEM-NAME :    ナフコＥＤＩ                　　     *  ./
/. *   JOB-ID      :    PSY39000                             *  ./
/. *   JOB-NAME    :    ナフコ送り状/納品明細書データ作成　　*  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./

    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./

    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./

    VAR ?PGMID    ,STRING*8,VALUE-'PSY39000'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./

    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD1   ,STRING*2,VALUE-'  '          /.担当者CD./

    VAR ?KBN      ,STRING*1,VALUE-' '           /. ｵﾝﾗｲﾝ/手書区分 ./
    VAR ?KANRNO   ,STRING*8,VALUE-'        '    /. 管理番号 ./
                                                /. 作場ＣＤ * 20 ./
    VAR ?SAKBCD   ,STRING*40,
                   VALUE-'                                        '

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ナフコ送り状／納品明細書データ作成'

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   BMSFLIB/TOKELIB/TOKFLIB/TOKELIBO/TOKDLIB/TOKSOLIB/
              TOKMDLIB/TOKDTLIB/TOKKLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD1 := %SBSTR(?TANCD,1,2)

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '倉庫ｺｰﾄﾞ取得'
              GOTO ABEND
    END
    ?MSGX :=  '倉庫＝'  && ?SOKCD && '代表倉庫＝' && ?DSOKCD
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.## ナフコ送り状／納品明細書データ作成指示 ##./
SSY3900I:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB

    CALL PGM-SSY3900I.TOKSOLIB,PARA-(?KANRNO,?SAKBCD,?KBN)

    ?PGMEC     :=    @PGMEC
    ?PGMEM     :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        IF ?PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           RETURN
        ELSE
           ?KEKA4 := '送り状／納品明細書データ作成指示'
           GOTO  ABEND
        END
    END

    ?MSGX :=  '管理番号＝'  && ?KANRNO
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '作場ＣＤ１　＝'  && %SBSTR(?SAKBCD,1,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ２　＝'  && %SBSTR(?SAKBCD,3,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ３　＝'  && %SBSTR(?SAKBCD,5,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ４　＝'  && %SBSTR(?SAKBCD,7,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ５　＝'  && %SBSTR(?SAKBCD,9,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ６　＝'  && %SBSTR(?SAKBCD,11,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ７　＝'  && %SBSTR(?SAKBCD,13,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ８　＝'  && %SBSTR(?SAKBCD,15,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ９　＝'  && %SBSTR(?SAKBCD,17,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１０＝'  && %SBSTR(?SAKBCD,19,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１１＝'  && %SBSTR(?SAKBCD,21,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１２＝'  && %SBSTR(?SAKBCD,23,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１３＝'  && %SBSTR(?SAKBCD,25,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１４＝'  && %SBSTR(?SAKBCD,27,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１５＝'  && %SBSTR(?SAKBCD,29,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１６＝'  && %SBSTR(?SAKBCD,31,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１７＝'  && %SBSTR(?SAKBCD,33,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１８＝'  && %SBSTR(?SAKBCD,35,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１９＝'  && %SBSTR(?SAKBCD,37,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ２０＝'  && %SBSTR(?SAKBCD,39,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'オ／手区分　＝'  && ?KBN
    SNDMSG    ?MSGX,TO-XCTL

/.##発行データ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE   NFLISTF1.TOKDTLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '発行データ初期化１'
              GOTO ABEND
    END

    CLRFILE   NFLISTF2.TOKDTLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '発行データ初期化２'
              GOTO ABEND
    END

    CLRFILE   NFLISTF.TOKDTLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '発行データ初期化３'
              GOTO ABEND
    END

/.##ナフコ送り状／納品明細書データ作成##./
SSY3910V:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SSY3910V.TOKSOLIB,PARA-(?TANCD1,?KBN,?KANRNO,
                                          ?SAKBCD)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '送り状／納品明細書データ作成'
              GOTO ABEND
    END

/.##送り状／納品明細書データマージ##./
MERGEF1:

    ?STEP :=   'MERGEF1 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

  /.MERGEF INFILE-NFLISTF1.TOKDTLIB!NFLISTF2.TOKDTLIB,
           OUTFILE-NFLISTF.TOKDTLIB,
           KEY-'F01!A',
           KEY1-'F02!A',
           KEY2-'F03!A',
           RCDL-@DSP    ./
    MERGE  INFILE-NFLISTF1.TOKDTLIB!NFLISTF2.TOKDTLIB,
           INRL-314,INBF-1,
           OUTFILE-NFLISTF.TOKDTLIB,
           KEY-1!4!DA,
           KEY1-6!1!CA,
           KEY2-8!4!DA,
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '送り状／納品明細書データ結合'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

/.##送り状／納品明細書データ転送##./
FIMPORT1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


    FIMPORT   FILE-NFLISTF.TOKDTLIB,
              PARA-NAFUKO,
              UNIT-7
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '送り状／納品明細書データ転送'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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