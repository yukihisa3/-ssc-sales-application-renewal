/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　　　　　       *  ./
/. *   JOB-NAME    :    顧客需要家ＩＤデータＣＳＶ出力　     *  ./
/. *   JOB-ID      :    PKJ00300                             *  ./
/. *   DATE        :    2020/11/27                           *  ./
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
    VAR  ?PGMID   ,STRING*8,VALUE-'PKJ00300'
    VAR  ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR  ?BUMON   ,STRING*4,VALUE-'    '     /.部門./
    VAR  ?TANCD8  ,STRING*8,VALUE-'        ' /.部門+担当者./
    VAR  ?TANCD   ,STRING*2,VALUE-'  '       /.担当者     ./
    VAR  ?SOKCD   ,STRING*2,VALUE-'00'       /.実行倉庫   ./
    VAR  ?DSOKCD  ,STRING*2,VALUE-'00'       /.代表倉庫   ./
    VAR  ?TOKCDS  ,STRING*8,VALUE-'        ' /.取引先開始./
    VAR  ?TOKCDE  ,STRING*8,VALUE-'        ' /.取引先終了./
    VAR  ?SYUKBN  ,STRING*1,VALUE-' '        /.エラー区分 ./
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
    DEFLIBL   TOKELIB/TOKELIBO/TOKFLIB/TOKDTLIB/D365DLIB/TOKMDLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM := '顧客需要家ＩＤデータＣＳＶ出力'

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


/.##顧客需要家ＩＤデータＣＳＶ出力指示##./
SKJ0030I:

    ?STEP :=  'SKJ0030I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL   SKJ0030I.TOKSOLIB,PARA-(?TOKCDS,?TOKCDE,?SYUKBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^= 0    THEN
              IF  ?PGMEC  =  4010  THEN
                  ?MSGX := '## 取消終了 ##'
                  SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                  RETURN
              END
              ?KEKA4 := '顧客需要家ＩＤデータＣＳＶ出力指示'
              GOTO   ABEND
    END
    ?MSGX :=  '#ﾊﾟﾗ取引先CD='&& ?TOKCDS && ' - ' && ?TOKCDE && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '#ﾊﾟﾗ出力区分='&& ?SYUKBN && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##処理判定##./
    IF  ?SYUKBN  =  '2'   THEN
        GOTO         SKJ0050V
    END

/.##顧客需要家ＩＤデータ出力（マスタ）##./
SKJ0040V:

    ?STEP :=  'SKJ0040V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL   SKJ0040V.TOKSOLIB,PARA-(?TOKCDS,?TOKCDE,?SYUKBN)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '顧客需要家ＩＤデータ出力（マスタ）'
              GOTO   ABEND
    END

/.##ＥＸＣＥＬ顧客需要家データ転送##./
PFIMPOR1:

    ?STEP :=  'PFIMPOR1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    IF   ?SYUKBN = ' '  THEN    /.##マスタリスト##./
         FIMPORT FILE-KYKJYKCV.TOKDTLIB,
                 PARA-KOKYAKU,
                 UNIT-3
         ?PGMEC  := @PGMEC
         ?PGMEM  := @PGMEM
         ?PGMES  := @PGMES
         IF      ?PGMEC ^= 0    THEN
            ?KEKA4 := '顧客需要家ＣＳＶ（マスタリスト）'
            GOTO   ABEND
         END
    END
    IF   ?SYUKBN = '1'  THEN    /.##修正用データ##./
         FIMPORT FILE-KYKJYKCV.TOKDTLIB,
                 PARA-KOKYAKU,
                 UNIT-4
         ?PGMEC  := @PGMEC
         ?PGMEM  := @PGMEM
         ?PGMES  := @PGMES
         IF      ?PGMEC ^= 0    THEN
            ?KEKA4 := '顧客需要家ＣＳＶ（修正用）'
            GOTO   ABEND
         END
    END

/.##終了へ##./
    GOTO  RTN

/.##顧客需要家ＩＤデータ出力（初期用）##./
SKJ0050V:

    ?STEP :=  'SKJ0050V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL   SKJ0050V.TOKSOLIB,PARA-(?TOKCDS,?TOKCDE)
    ?PGMEC  := @PGMEC
    ?PGMES  := @PGMES
    IF      ?PGMEC ^= 0    THEN
            ?KEKA4 := '顧客需要家ＩＤデータ出力（初期用）'
            GOTO   ABEND
    END

/.##ＥＸＣＥＬ顧客需要家データ転送##./
PFIMPOR2:

    ?STEP :=  'PFIMPOR2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

         FIMPORT FILE-KYKJYKCV.TOKDTLIB,
                 PARA-KOKYAKU,
                 UNIT-5
         ?PGMEC  := @PGMEC
         ?PGMES  := @PGMES
         IF      ?PGMEC ^= 0    THEN
            ?KEKA4 := '顧客需要家ＣＳＶ（マスタリスト）'
            GOTO   ABEND
         END

/.--------------------------------------------------------------./

RTN:

    ?KEKA1 :=  '顧客需要家ＩＤデータＣＳＶ出力'
    ?KEKA2 :=  '　　　正常終了しました。'
    ?KEKA3 :=  '　　　ＣＳＶを確認してください！'
    ?KEKA4 :=  ''

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    RELEASE FILE-ELKJDATA.TOKDTLIB!@XCL

    ?KEKA1 :=  '顧客需要家ＩＤデータＣＳＶ出力'
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