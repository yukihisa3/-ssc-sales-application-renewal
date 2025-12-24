/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                    　　     *  ./
/. *   JOB-ID      :    PBM03200                             *  ./
/. *   JOB-NAME    :    流通ＢＭＳ　仕入伝票データ作成　手書 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNMH   ,STRING*6,VALUE-'      '
    VAR ?FILNMD   ,STRING*6,VALUE-'BMSDEN'
    VAR ?FILNMO   ,STRING*5,VALUE-'BMSDT'
    VAR ?LIBNM    ,STRING*8,VALUE-'BMSFLIB '
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?DENPYO   ,NAME!MOD
    VAR ?DENPYON  ,STRING*17
    VAR ?BMSDT    ,NAME!MOD
    VAR ?BMSDTN   ,STRING*17
    VAR ?BMSDT1   ,NAME!MOD
    VAR ?BMSDT1N  ,STRING*17
    VAR ?BMSDT2   ,NAME!MOD
    VAR ?BMSDT2N  ,STRING*17
    VAR ?BMSDT3   ,NAME!MOD
    VAR ?BMSDT3N  ,STRING*17
    VAR ?PGNAME   ,NAME
    VAR ?LIBNAME  ,NAME
    VAR ?OVRFNAME ,NAME
    VAR ?PGC      ,NAME!MOD
    VAR ?PGN      ,STRING*17
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PBM03200'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?SOKO     ,STRING*2,VALUE-'  '          /.出荷場所./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./
    VAR ?JYUN     ,STRING*1,VALUE-' '           /.出力順./
    VAR ?DENPTN   ,STRING*1,VALUE-' '           /.伝票種類./
    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日付./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時間./
    VAR ?TORICD   ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR ?NOUDT1   ,STRING*8,VALUE-'00000000'    /.開始納品日./
    VAR ?NOUDT2   ,STRING*8,VALUE-'00000000'    /.終了納品日./
    VAR ?TENCD1   ,STRING*5,VALUE-'00000'       /.開始店舗　./
    VAR ?TENCD2   ,STRING*5,VALUE-'00000'       /.終了店舗　./
    VAR ?PGCD     ,STRING*8,VALUE-'        '    /.ﾌﾟﾛｸﾞﾗﾑCD./
    VAR ?LIBCD    ,STRING*8,VALUE-'        '    /.ﾗｲﾌﾞﾗﾘCD./
    VAR ?KEYA     ,STRING*1,VALUE-'1'           /.KEY1./
    VAR ?KEYB     ,STRING*1,VALUE-'2'           /.KEY2./
    VAR ?KEYC     ,STRING*1,VALUE-'3'           /.KEY3./
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.ﾒｯｾｰｼﾞ2    ./
    VAR ?OPR3     ,STRING*50                    /.ﾒｯｾｰｼﾞ3    ./
    VAR ?OPR4     ,STRING*50                    /.ﾒｯｾｰｼﾞ4    ./
    VAR ?OPR5     ,STRING*50                    /.ﾒｯｾｰｼﾞ5    ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD1   ,STRING*2,VALUE-'12'          /.担当者CD./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '流通ＢＭＳ　仕入伝票データ作成　手書（ムサシ）'

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   BMSFLIB/TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKSOLIB/
              TOKMDLIB

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

/.##仕入伝票データ作成・転送##./
SSY4477I:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SSY4477I.TOKSOLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '納品明細書発行データ作成'
              GOTO ABEND
    END

/.##伝票発行ワークＳＯＲＴ##./
PSORT:

    ?STEP :=   'PSORT   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORTF  INFILE-ARDENWK.BMSFLIB,
           OUTFILE-ARDENWK.BMSFLIB,
           RCD-50,
           KEY-'F323!A',
           KEY1-'F304!A',
           KEY2-'F344!A',
           KEY3-'F345!A',
           KEY4-'F302!A',
           KEY5-'F402!A',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

/.##納品書発行データ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE ARDENTEF.BMSFLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '納品書発行データ初期化'
              GOTO ABEND
    END

/.##納品明細書発行データ作成##./
SSY4476V:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-BMSDENF,TOFILE-ARDENWK.BMSFLIB
    OVRF FILE-ARSIDEN1,TOFILE-ARDENTE1.BMSFLIB
    CALL      PGM-SSY4477V.TOKSOLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '納品明細書発行データ作成'
              GOTO ABEND
    END

/.##仕入伝票データセット##./
PCNVFILE:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CNVFILE FILE-ARDENTEF.BMSFLIB,TOFILE-ARDENTSF.BMSFLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '納品明細書データセット'
              GOTO ABEND
    END

/.##仕入伝票データ転送##./
PFIMPIRT:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


    FIMPORT FILE-ARDENTSF.BMSFLIB,PARA-ARDENF,UNIT-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '納品明細書データ転送'
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