/. ***********************************************************  ./
/. *   （株）サカタのタネ                                    *  ./
/. *   SYSTEM-NAME :    ナフコ出荷支援システム　　　　       *  ./
/. *   JOB-ID      :    PSY39980                             *  ./
/. *   JOB-NAME    :    ナフコ店舗マスタ　　　　　　　       *  ./
/. *                    パターンＣＤ一括置換え　　　　　　　 *  ./
/. ***********************************************************  ./
    PGM
/.##ﾊﾟﾗﾒﾀ情報##./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY39980'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?KANRINO  ,STRING*8,VALUE-'00000000' /.管理番号    ./
    VAR       ?JDATE    ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?JTIME    ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?JTOKCD   ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?SKBCD    ,STRING*2,VALUE-'00'       /.作場./
    VAR       ?SOKCD    ,STRING*2,VALUE-'00'       /.実行倉庫    ./
    VAR       ?DSOKCD   ,STRING*2,VALUE-'00'       /.代表倉庫    ./
    VAR       ?ERRKBN   ,STRING*1,VALUE-'0'        /.エラー区分  ./
    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./
/.-----------------------------------------------------------./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ナフコ店舗マスタ　パターン一括置換え'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO ABEND
    END

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##確認画面##./
SHOM0900:

    ?STEP :=   'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ?OPR1  :=  '　　＜ナフコ店舗マスタ　パターンＣＤ一括置換＞'
    ?OPR2  :=  '　　　　パターンコードの一括置換を行います'
    ?OPR3  :=  '　　　　更新元ＣＳＶデータは、所定の場所に'
    ?OPR4  :=  '　　　　配置されていますか？'
    ?OPR5  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '処理確認画面'
              GOTO ABEND END

/.##使用ファイルロック##./
ASSIGN:

    ?STEP :=  'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES


    ASSIGN FILE-NFTENWK.TOKDLIB!@XCL
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   1020 THEN
       IF  ?PGMEC ^=   0    THEN
           SNDMSG 'ファイルが他で使用されています',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           SNDMSG '### NFTENWK.TOKDLIB ###',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  '一括置換えデータロック'
           GOTO   ABEND
       END
    END

    ASSIGN FILE-NFTENMS.TOKDLIB!@XCL
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   1020 THEN
       IF  ?PGMEC ^=   0    THEN
           SNDMSG 'ファイルが他で使用されています',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           SNDMSG '### NFTENMS.TOKDLIB ###',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  'ナフコ店舗マスタロック'
           GOTO   ABEND
       END
    END

CNVDF000:

    ?STEP :=   'CNVDF000'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ?MSGX :=  '***   取込元 = HGNAS  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '***   ﾌｧｲﾙID = NFTENWK.CSV ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CNVDF FILE-NFTENWK.TOKDLIB,
          PATH-'//HGNAS/NFTENWK.CSV',
          MODE-@IMP,
          ADD-@NO,
          ACTCHK-@NO

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 := 'ナフコ店舗パターン一括置換データ取込'
              GOTO ABEND END

/.##ナフコ店舗マスタ　パターンＣＤ一括置換##./
SSY3998B:

    ?STEP :=   'SSY3998B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL  SSY3998B.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ナフコ店舗マスタ　パターン一括置換'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

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