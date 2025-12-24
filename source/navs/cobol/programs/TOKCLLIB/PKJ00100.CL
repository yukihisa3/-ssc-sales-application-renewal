/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　　　　　       *  ./
/. *   JOB-NAME    :    顧客需要家ＩＤ登録データ取込         *  ./
/. *   JOB-ID      :    PKJ00100                             *  ./
/. *   DATE        :    2016/09/14                           *  ./
/. ***********************************************************  ./
    PGM

/.--------------------------------------------------------------./
    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PKJ00100'
    VAR  ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR  ?BUMON   ,STRING*4,VALUE-'    '     /.部門./
    VAR  ?TANCD8  ,STRING*8,VALUE-'        ' /.部門+担当者./
    VAR  ?TANCD   ,STRING*2,VALUE-'  '       /.担当者     ./
    VAR  ?SOKCD   ,STRING*2,VALUE-'00'       /.実行倉庫   ./
    VAR  ?DSOKCD  ,STRING*2,VALUE-'00'       /.代表倉庫   ./
    VAR  ?ERRKBN  ,STRING*1,VALUE-' '        /.エラー区分 ./
/.--------------------------------------------------------------./
    VAR  ?OPR1    ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?OPR2    ,STRING*50                 /.      2    ./
    VAR  ?OPR3    ,STRING*50                 /.      3    ./
    VAR  ?OPR4    ,STRING*50                 /.      4    ./
    VAR  ?OPR5    ,STRING*50                 /.      5    ./
/.--------------------------------------------------------------./
    VAR  ?MSG1    ,STRING*80                 /.開始終了MSG./
    VAR  ?PGNM    ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1   ,STRING*40                 /.      2    ./
    VAR  ?KEKA2   ,STRING*40                 /.      3    ./
    VAR  ?KEKA3   ,STRING*40                 /.      4    ./
    VAR  ?KEKA4   ,STRING*40                 /.      5    ./
/.---------------------------------------------------------------./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKFLIB/TOKDTLIB/D365DLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM := '顧客需要家ＩＤ登録データ取込'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
GETWKSTN:
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=  'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              GOTO   ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO   ABEND
    END


/.##使用ファイルロック##./
ASSIGN:

    ?STEP :=  'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.伝票EXCELデータ./
    ASSIGN FILE-ELKJDATA.TOKDTLIB!@XCL  /.EXCEL顧客受領家データ./
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF  ?PGMEC ^=   1020 THEN
        IF  ?PGMEC ^=   0    THEN
            SNDMSG '顧客需要家ＩＤ取込処理が行なえません！',
                           TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
            ?KEKA4 :=  'ＥＸＣＥＬ顧客需要家Ｍロック'
             GOTO   ABEND1
        END
    END

/.##取込確認画面##./
SHOM0900:

    ?STEP :=  'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ?OPR1 := '＜顧客需要家ＩＤデータ取込処理＞'
    ?OPR2 := 'ＥＸＣＥＬにて作成された顧客需要家ＩＤデー'
    ?OPR3 := 'タの取込処理を行ないます。データ作成は完了'
    ?OPR4 := 'しておりますか？'
    ?OPR5 := '処理実行か、打ち切りか選択してください！！'

    CALL   OHOM0900.TOKELIB,
           PARA-(?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '処理確認画面'
              GOTO   ABEND
    END

/.##ＥＸＣＥＬ顧客需要家データ転送##./
PFEXPORT:

    ?STEP :=  'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.　→伝票EXCELデータ(１) ./
    FEXPORT FILE-ELKJDATA.TOKDTLIB,
            MODE-@REP,
            PARA-KOKYAKU,
            UNIT-1
    ?PGMEC  := @PGMEC
    ?PGMES  := @PGMES
    IF      ?PGMEC ^= 0    THEN
            ?KEKA4 := 'ＥＸＣＥＬ顧客需要家ＤＴ転送異常！'
            GOTO   ABEND
    END

/.##顧客需要家ＩＤ取込ワーク削除##./
PCLRFILE:

    ?STEP :=  'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CLRFILE FILE-KYKJYKWK.TOKDTLIB
    ?PGMEC  := @PGMEC
    ?PGMES  := @PGMES
    IF      ?PGMEC ^= 0    THEN
            ?KEKA4 := '顧客需要家ＩＤ取込ワーク削除異常！'
            GOTO   ABEND
    END

/.##ＥＸＣＥＬ顧客需要家データチェック##./
SKJ0010B:

    ?STEP :=  'SKJ0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL   PGM-SKJ0010B.TOKSOLIB,PARA-(?BUMON,?TANCD,?ERRKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
       IF  ?PGMEC = 4010  THEN

              ?OPR1 := '＜顧客需要家ＩＤデータ取込―エラー！＞'
              ?OPR2 := '取込対象のデータが存在しません。ＥＸＣＥＬ'
              ?OPR3 := 'にてデータ作成を行なっておりますか？　　　'
              ?OPR4 := '再度、データを確認し再実行をお願い致します'
              ?OPR5 := '＜対象データ無！！！＞'
              CALL   OHOM0900.TOKELIB,
                     PARA-(?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
       ELSE
              ?KEKA4 := 'ＥＸＣＥＬ顧客需要家データチェック'
              GOTO   ABEND
       END
    END

/.##エラーチェック##./
ERRCHK00:

    ?STEP :=  'ERRCHK00'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    IF        ?ERRKBN  =  '1'   THEN
              ?OPR1 := '＜顧客需要家ＩＤデータ取込―エラー有＞'
              ?OPR2 := 'ＥＸＣＥＬデータの取込時、エラーが発生して'
              ?OPR3 := 'おります。実行することで、エラーＣＳＶが出'
              ?OPR4 := '力されます。ＣＳＶを確認ｓエラー箇所の訂正'
              ?OPR5 := 'をお願い致します。'

              CALL   OHOM0900.TOKELIB,
                     PARA-(?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    END

/.##顧客需要家IDデータＣＳＶ出力（取込時チェック）##./
SKJ0020V:

    ?STEP  :=  'SKJ0020V'
    ?MSGX  :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL      PGM-SKJ0020V.TOKSOLIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '顧客需要家ＩＤデータＣＳＶ出力'
              GOTO   ABEND
    END

/.##顧客需要家ＩＤデータ出力##./
PFIMPORT:

    ?STEP :=  'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    FIMPORT FILE-KYKJYKCV.TOKDTLIB,PARA-KOKYAKU,UNIT-2
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '顧客需要家ＩＤデータ出力'
              GOTO   ABEND
    END


/.--------------------------------------------------------------./

RTN:

    RELEASE FILE-ELKJDATA.TOKDTLIB!@XCL

    ?KEKA1 :=  '顧客需要家ＩＤ登録データ取込処理'
    ?KEKA2 :=  '　　　正常終了しました。'
    ?KEKA3 :=  '　　　ＣＳＶを確認してください！'
    ?KEKA4 :=  ''

    IF  ?ERRKBN  =  '1'  THEN
        ?KEKA1 :=  '顧客需要家ＩＤ登録データ取込処理'
        ?KEKA2 :=  '　エラー箇所があります。'
        ?KEKA3 :=  '　ＣＳＶを確認し修正後、再度、取込'
        ?KEKA4 :=  '　処理を行なって下さい。'
    END

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    RELEASE FILE-ELKJDATA.TOKDTLIB!@XCL

    ?KEKA1 :=  '顧客需要家ＩＤ登録データ取込処理が'
    ?KEKA2 :=  '異常終了しました。'
    ?KEKA3 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  := ?MSG(?I)
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    END

    RETURN    PGMEC-?PGMEC

ABEND1:

    RELEASE FILE-ELKJDATA.TOKDTLIB!@XCL

    ?KEKA1 :=  '顧客需要家ＩＤ登録データ取込処理関係の'
    ?KEKA2 :=  'ファイルが他で使用されております。'
    ?KEKA3 :=  '他端末で使用中でないか確認して下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  := ?MSG(?I)
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
