/. ***********************************************************  ./
/. *   サカタのタネ　　　　　　　　　　　　　　　　  　      *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳ対応　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PKE10801                             *  ./
/. *   JOB-NAME    :    出荷データ作成（カインズＴＣ１）　   *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PKE10801'
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
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH1  ,STRING*5,VALUE-'CZSUM'       /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKKPLIB'    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?CZSUMF   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?CZSUMFN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?CZSUM1   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?CZSUM1N  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?CZSUM3   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?CZSUM3N  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
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
    VAR       ?TANCD8   ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?WS ,STRING*8,VALUE-'        '       /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?SOKCD  ,STRING*2,VALUE-'  '         /.自倉庫ｺｰﾄﾞ./
    VAR       ?DSOKCD ,STRING*2,VALUE-'  '         /.代表倉庫ｺｰﾄﾞ./
    VAR       ?NKEITAI,STRING*1,VALUE-'1'          /.納品形態./
                                                   /.1:TC 2:店直./
    VAR       ?DKIND  ,STRING*1,VALUE-'1'          /.データ種類./
                                                   /.1:出荷ＤＴ作成./
                                                   /.2:送信ＤＴ作成./
    VAR       ?SKIPSOKO,STRING*2,VALUE-'  '        /.削除除外倉庫./
    VAR       ?JDATE  ,STRING*8,VALUE-'00000000'   /.受信日付./
    VAR       ?JTIME  ,STRING*4,VALUE-'0000'       /.受信時間./
    VAR       ?TORICD ,STRING*8,VALUE-'00000000'   /.取引先./
    VAR       ?SOKO   ,STRING*2,VALUE-'00'         /.出荷場所./
    VAR       ?NOUDT  ,STRING*8,VALUE-'00000000'   /.納品日./
    VAR       ?BMNCD  ,STRING*4,VALUE-'0000'       /.部門./
    VAR       ?TNCD   ,STRING*2,VALUE-'00'         /.担当者　./
    VAR       ?SITEI  ,STRING*1,VALUE-'2'          /.非定型　./
    VAR       ?SYURUI ,STRING*1,VALUE-'1'          /.種類　　./

    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB/TOKKPLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '出荷データ作成（カインズＴＣ１）'
    ?SYORINM1 :=  '出荷データ作成（カインズＴＣ１）'

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

    ?OPR1  :=  '　　　　　　【流通ＢＭＳ出荷ＤＴ作成】'
    ?OPR2  :=  '　　　　カインズ　出荷データを作成します。'
    ?OPR3  :=  '　　　　　　　　（センター納品分）　　　　　　'
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
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD := ?TANCD8

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

    ?SOKCD := 'TK' /.強制置換./

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?SOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    /.検品結果集計ファイルPF./
    ?FILNMH   :=    ?FILNMH1
    ?FILNM    :=    ?FILNMH && ?SOKCD    /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?CZSUMF   :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?CZSUMFN  :=    %STRING(?CZSUMF)
    ?MSGX     :=    '## 結果集計 = ' && ?CZSUMFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    /.検品結果集計ファイルL1./
    ?FILNMH  :=  ?FILNMH1
    ?FILNM   :=  ?FILNMH && ?SOKCD && '1'  /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID   :=  %NAME(?FILNM)              /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID   :=  %NAME(?LIBNM)              /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?CZSUM1  :=  %NCAT(?FILID,?LIBID)       /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?CZSUM1N :=  %STRING(?CZSUM1)
    ?MSGX    :=  '## 結果集計 = ' && ?CZSUM1N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    /.検品結果集計ファイルL3./
    ?FILNMH  :=  ?FILNMH1
    ?FILNM   :=  ?FILNMH && ?SOKCD && '3'  /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID   :=  %NAME(?FILNM)              /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID   :=  %NAME(?LIBNM)              /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?CZSUM3  :=  %NCAT(?FILID,?LIBID)       /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?CZSUM3N :=  %STRING(?CZSUM3)
    ?MSGX    :=  '## 結果集計 = ' && ?CZSUM3N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ファイルロック##./
ASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-?CZSUMF!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  '検品結果データロック'
              GOTO ABEND
    END

    ASSIGN    FILE-CZSYKTCF.BMSFLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  'カインズ出荷データロック'
              GOTO ABEND
    END

    ASSIGN    FILE-CZKEPTCF.BMSFLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  'カインズ欠品データロック'
              GOTO ABEND
    END

/.##流通BMS出荷データ作成指示##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL  NKE1080I.TOKSOLIB,PARA-(?BUMON,?TANCD,?SOKCD,?DSOKCD,
                                  ?NKEITAI,?DKIND,
                                  ?JDATE,?JTIME,?TORICD,?SOKO,
                                  ?NOUDT,?BMNCD,?TNCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      @PGMEC    ^=   0    THEN
       IF      @PGMEC    =  4010    THEN
               SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
               ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
               SNDMSG    ?MSGX,TO-XCTL
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

    ?SOKO :=  'TK' /.強制置換./
    ?MSGX :=  '　　　　→'  && ?SOKO
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '納品日　＝'  && ?NOUDT
    SNDMSG    ?MSGX,TO-XCTL

/.##出荷ＭＳＧレコード削除##./
STEP051:

    ?STEP :=   'STEP051 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-CZSYKTC5,TOFILE-CZSYKTC5.BMSFLIB
    CALL SBM9063B.TOKSOLIB,PARA-(?JDATE,?JTIME,?TORICD,?SOKO,
                                 ?SKIPSOKO,?NOUDT)

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

/.##出荷データ作成（カインズＴＣ１）##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-CZSUMXX1,TOFILE-?CZSUM1
    OVRF FILE-CZSUMXX3,TOFILE-?CZSUM3
    OVRF FILE-BMSHACL3,TOFILE-BMSHACL3.BMSFLIB
    OVRF FILE-CZSYKTC3,TOFILE-CZSYKTC3.BMSFLIB
    OVRF FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    CALL NKE1091B.TOKSOLIB,PARA-(?JDATE,?JTIME,?TORICD,?SOKO,
                                 ?NOUDT,?BMNCD,?TNCD,?SYORIKN1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【出荷ＤＴ作成（カインズＴＣ）】'
              GOTO ABEND
    END

/.##出荷データ作成（カインズＴＣ１欠品行）##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-BMSHACL5,TOFILE-BMSHACL5.BMSFLIB
    OVRF FILE-CZSYKTC3,TOFILE-CZSYKTC3.BMSFLIB
    OVRF FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    CALL NKE1093B.TOKSOLIB,PARA-(?SYORIKN1,?JDATE,?JTIME,?TORICD,?SOKO,
                                 ?NOUDT,?BMNCD,?TNCD,?SYORIKN2)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【出荷ＤＴ作成（行欠品分）】'
              GOTO ABEND
    END

/.##欠品データ作成（カインズ）##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-CZSYKTC5,TOFILE-CZSYKTC5.BMSFLIB
    OVRF FILE-CZKEPTCF,TOFILE-CZKEPTCF.BMSFLIB
    CALL NKE1100B.TOKSOLIB,PARA-(?JDATE,?JTIME,?TORICD,?SOKO,
                                 ?NOUDT,?BMNCD,?TNCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【欠品ＤＴ作成（カインズＴＣ）】'
              GOTO ABEND
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
    ?SYORINM3 := '件数１：出荷件数（センター納品）'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN2,?SYORIKN3,
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