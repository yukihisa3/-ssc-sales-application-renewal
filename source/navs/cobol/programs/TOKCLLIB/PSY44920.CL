/. ***********************************************************  ./
/. *   サカタのタネ　ホームガーデン部　　　　　　　  　      *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳ対応　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PSY44920                             *  ./
/. *   JOB-NAME    :    流通ＢＭＳ出荷データ作成　　　　　   *  ./
/. *               :    　ビバホーム　　（オンライン分）　   *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSY44920'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,NAME!MOD
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK3   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?OK1    ,STRING*2,VALUE-'  '        /.実行確認1./
    VAR       ?OK2    ,STRING*2,VALUE-'  '        /.実行確認2./
    VAR       ?OK3    ,STRING*2,VALUE-'  '        /.実行確認3./
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
    VAR       ?PGCHK1 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK2 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK3 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門名./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?WS ,STRING*8,VALUE-'        '       /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?SOKCD  ,STRING*2,VALUE-'  '         /.自倉庫ｺｰﾄﾞ./
    VAR       ?DSOKCD ,STRING*2,VALUE-'  '         /.代表倉庫ｺｰﾄﾞ./
    VAR       ?JDATE  ,STRING*8,VALUE-'00000000'   /.受信日付./
    VAR       ?JTIME  ,STRING*4,VALUE-'0000'       /.受信時間./
    VAR       ?TORICD ,STRING*8,VALUE-'00000000'   /.取引先./
    VAR       ?SOKO   ,STRING*2,VALUE-'00'         /.出荷場所./
    VAR       ?NOUDT  ,STRING*8,VALUE-'00000000'   /.納品日./
    VAR       ?BMNCD  ,STRING*4,VALUE-'0000'       /.部門./
    VAR       ?TNCD   ,STRING*2,VALUE-'00'         /.担当者　./
    VAR       ?SITEI  ,STRING*1,VALUE-'2'          /.非定型　./
    VAR       ?SYURUI ,STRING*1,VALUE-'1'          /.種類　　./

    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＢＭＳ出荷ＤＴ作成（ビバホーム）'
    ?SYORINM1 :=  'ＢＭＳ出荷ＤＴ作成（ビバホーム）'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【流通ＢＭＳ出荷ＤＴ作成】'
    ?OPR2  :=  '流通ＢＭＳ出荷データを作成します。'
    ?OPR3  :=  '　　　（ビバホーム）　　　　　　　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB/TOKSOLIB

/.##ログインユーザー情報取得##./
STEP030:

    ?STEP :=   'STEP030'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END


/.##倉庫ｺｰﾄﾞ取得##./
STEP040:

    ?STEP :=   'STEP040'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##出荷メッセージファイル,出荷メッセージ送信ファイルロック##./
ASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-BMSSYKF.BMSFLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  '出荷メッセージファイルロック'
              GOTO ABEND
    END

    ASSIGN    FILE-SYKSNDF.BMSFLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  '出荷メッセージ送信ファイルロック'
              GOTO ABEND
    END

/.##流通BMS出荷データ作成指示##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL  SBM0060I.TOKELIBO,PARA-(?BUMON,?TANCD,?SOKCD,?DSOKCD,
                                  ?JDATE,?JTIME,?TORICD,?SOKO,
                                  ?NOUDT,?BMNCD,?TNCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      @PGMEC    ^=   0    THEN
       IF      @PGMEC    =  4010    THEN
               SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
             /.GOTO RTN./
               RETURN    PGMEC-@PGMEC
       ELSE
               ?KEKA4 := '【出荷データ作成指示】'
               GOTO ABEND
       END
    END

    ?MSGX :=  '受信日　＝'  && ?JDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '受信時刻＝'  && ?JTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '取引先　＝'  && ?TORICD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '出荷場所＝'  && ?SOKO
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '納品日　＝'  && ?NOUDT
    SNDMSG    ?MSGX,TO-XCTL

/.##出荷ＭＳＧレコード削除##./
STEP051:

    ?STEP :=   'STEP051 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-BMSSYKL3,TOFILE-BMSSYKL3.BMSFLIB
/.  CALL SBM9061B.TOKSOLIB,PARA-(?SITEI,?SYURUI,
./  CALL SBM9061C.TOKSOLIB,PARA-(?SITEI,?SYURUI,
                                 ?JDATE,?JTIME,?TORICD,?SOKO,
                                 ?NOUDT)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      @PGMEC    ^=   0    THEN
       IF      @PGMEC    =  4010    THEN
               GOTO RTN
       ELSE
               ?KEKA4 := '【出荷ＭＳＧレコード削除】'
               GOTO ABEND
       END
    END

/.##出荷データ作成##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-BMSHACL3,TOFILE-BMSHACL3.BMSFLIB
    OVRF FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF FILE-BMSSYKL3,TOFILE-BMSSYKL3.BMSFLIB
    CALL SSY4492B.TOKSOLIB,PARA-(?JDATE,?JTIME,?TORICD,?SOKO,
                                 ?NOUDT,?BMNCD,?TNCD,?SYORIKN1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      @PGMEC    ^=   0    THEN
       IF      @PGMEC    =  4010    THEN
               GOTO RTN
       ELSE
               ?KEKA4 := '【出荷ＤＴ作成（ビバホーム）】'
               GOTO ABEND
       END
    END

/.##プログラム正常終了##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '出荷データ作成処理が正常終了しました。'
    ?KEKA2 :=  '件数リスト等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB/BMSFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM2 := '出荷データ作成処理が正常終了しました。　　　　'
    ?SYORINM3 := '件数１：出荷件数（オンライン）　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN3,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了##./
ABEND:

/.  ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM    ./

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '出荷データ作成処理が異常終了しました。'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
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

    RETURN    PGMEC-?PGMEC
