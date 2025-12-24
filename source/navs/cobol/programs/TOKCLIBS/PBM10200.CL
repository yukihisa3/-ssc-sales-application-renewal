/. ***********************************************************  ./
/. *   サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳ対応　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PBN10200                             *  ./
/. *   JOB-NAME    :    流通ＢＭＳイオン請求データ作成　     *  .
/. *   2013/05/22 NAV TAKAHASHI                              *  .
/. *   送信データ作成障害対応ＰＧ追加                        *  .
/. ***********************************************************  ./
    PGM (P1-?TORICD)
    PARA      ?TORICD ,STRING*8,IN,VALUE-'        ' /.取引先コード./
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PBM10200'
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
    VAR       ?BUMON    ,STRING*4,VALUE-'    '   /.部門名./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '     /.担当者CD./
    VAR       ?WS ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?SOKCD  ,STRING*2,VALUE-'  '         /.自倉庫ｺｰﾄﾞ./
    VAR       ?DSOKCD ,STRING*2,VALUE-'  '         /.代表倉庫ｺｰﾄﾞ./
    VAR       ?JDATE  ,STRING*8,VALUE-'00000000'   /.受信日付./
    VAR       ?JTIME  ,STRING*4,VALUE-'0000'       /.受信時間./
/.**VAR*******?TORICD ,STRING*8,VALUE-'00008737'*****取引先*****./
    VAR       ?SOKO   ,STRING*8,VALUE-'00000000'   /.出荷場所./
    VAR       ?NOUDT  ,STRING*8,VALUE-'00000000'   /.納品日./
    VAR       ?BMNCD  ,STRING*8,VALUE-'00000000'   /.部門./
    VAR       ?TNCD   ,STRING*5,VALUE-'00000'      /.担当者　./
    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '流通ＢＭＳ請求ＤＴ作成'
    ?SYORINM1 :=  '流通ＢＭＳ請求ＤＴ作成'

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

    ?OPR1  :=  '【流通ＢＭＳ請求ＤＴ作成】'
    ?OPR2  :=  'イオン流通ＢＭＳ請求データを作成します。'
    ?OPR3  :=  '　　　　　　　　　　　　　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO

/.##ログインユーザー情報取得##./
STEP030:

    ?STEP :=   'STEP030'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END

/.##バッチ_採番処理##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SJH8999B.TOKELIBO,PARA-(?JDATE,?JTIME)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ_採番処理】'
            GOTO ABEND
    END

/.##イオン請求合計ファイル,請求メッセージファイル,
                      請求メッセージ送信ファイルロック##./
ASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF        ?TORICD  =  '00008737'  THEN
        ASSIGN    FILE-SETGKFP.TOKFLIB!@XCL
    ELSE
        ASSIGN    FILE-SEIGK46.TOKFLIB!@XCL
    END
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  'イオン請求合計ファイルロック'
              GOTO ABEND
    END

    ASSIGN    FILE-BMSSEKF.BMSFLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  '請求メッセージファイルロック'
              GOTO ABEND
    END

    IF        ?TORICD  =  '00008737'  THEN
        ASSIGN    FILE-SEKIONF.ONLBLIB!@XCL
    ELSE
        ASSIGN    FILE-SEKKYUF.ONLBLIB!@XCL
    END
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  '請求メッセージ送信ファイルロック'
              GOTO ABEND
    END


/.##請求データ(全社)→対象取引先分抽出##./
PSORT1:

    ?STEP :=   'PSORT1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF        ?TORICD ^=  '00008737'  THEN
       IF        ?TORICD ^=  '00033189'   THEN
              ?KEKA4 :=  '　イオン　取引先コード指定に誤り！　'
              GOTO ABEND
       END
    END

    IF        ?TORICD  =  '00008737'  THEN
       SORTF  INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SETGKFP.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00008737',
              SELECT1-'F01!EQ!#00087371',
              SELECT2-'F01!EQ!#00087372',
              SELECT3-'F01!EQ!#00087373',
              SELECT4-'F01!EQ!#00087374',
              SELECT5-'F01!EQ!#00087375',
              SELECT6-'F01!EQ!#00087376', /.##2015/11/17追加##./
              RCDL-@DSP
       IF     @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【イオン分　請求データ抽出】'
              GOTO ABEND
       END
    END

    IF        ?TORICD  =  '00033189'  THEN
       SORTF  INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SEIGK46.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00033189',
              RCDL-@DSP
       IF     @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【イオン九州分　請求データ抽出】'
              GOTO ABEND
       END
    END

/.##イオン請求データ作成##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-IONFURL2,TOFILE-IONFURL2.BMSFLIB
    IF        ?TORICD  =  '00008737'  THEN
        OVRF FILE-SETGKFP,TOFILE-SETGKFP.TOKFLIB
    ELSE
        OVRF FILE-SETGKFP,TOFILE-SEIGK46.TOKFLIB
    END
    OVRF FILE-BMSHACL2,TOFILE-BMSHACL2.BMSFLIB
    OVRF FILE-BMSSEKL1,TOFILE-BMSSEKL1.BMSFLIB
    CALL SBM1020B.TOKELIBO,PARA-(?JDATE,?JTIME,?TORICD,
                                 ?BUMON,?TANCD,?SYORIKN1)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【イオン請求データ作成】'
                    GOTO ABEND
    END


/.##請求データフォーマット変換##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-BMSSEKL1,TOFILE-BMSSEKL1.BMSFLIB
    IF        ?TORICD  =  '00008737'  THEN
        OVRF FILE-SEKSNDF,TOFILE-SEKIONF.ONLBLIB
    ELSE
        OVRF FILE-SEKSNDF,TOFILE-SEKKYUF.ONLBLIB
    END
/.## SBM0190Bは取引先が分かれない為、未使用にする##./
/.##CALL  SBM0190B.TOKELIBO                      ##./
    CALL  SBM0191B.TOKELIBO
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【請求送信データフォーマット変換】'
                    GOTO ABEND
    END

/.##2013/05/22 NAV ST 以下を追加　送信ＤＴより発行##./
/.##請求送信データ確認リスト出力##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    IF        ?TORICD  =  '00008737'  THEN
        OVRF FILE-SEKIONF,TOFILE-SEKIONF.ONLBLIB
    ELSE
        OVRF FILE-SEKIONF,TOFILE-SEKKYUF.ONLBLIB
    END
    CALL  SBM2000L.TOKELIBO
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【請求送信データ確認リスト出力】'
                    GOTO ABEND
    END

/.##プログラム正常終了##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '請求データ作成処理が正常終了しました。'
    ?KEKA2 :=  '件数リスト等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM2 := '請求データ作成処理が正常終了しました。　　　　'
    ?SYORINM3 := '件数１：請求件数　　　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了##./
ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '請求データ作成処理が異常終了しました。'
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

    RETURN    PGMEC-@PGMEC
