/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携                         *  ./
/. *   JOB-ID      :    PSY08010                             *  ./
/. *   JOB-NAME    :    連携店舗変更（オンライン）           *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMES    ,STRING*5,VALUE-'     '
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PSY08010'
    VAR ?STEP     ,STRING*8
    VAR ?KEKA1    ,STRING*40                  /.      2    ./
    VAR ?KEKA2    ,STRING*40                  /.      3    ./
    VAR ?KEKA3    ,STRING*40                  /.      4    ./
    VAR ?KEKA4    ,STRING*40                  /.      5    ./
    VAR ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./

    VAR ?SOKO     ,STRING*02,VALUE-'00'          /.倉庫./
    VAR ?DSOKO    ,STRING*02,VALUE-'00'          /.代表倉庫./
    VAR ?BUMON    ,STRING*4,VALUE-'    '         /.部門./
    VAR ?TANCD8   ,STRING*8,VALUE-'      '       /.担当者./
    VAR ?TANCD    ,STRING*2,VALUE-'  '           /.担当者./

    VAR ?BDATE    ,STRING*08,VALUE-'00000000'    /.バッチ日./
    VAR ?BTIME    ,STRING*04,VALUE-'0000'        /.バッチ時./
    VAR ?BTORI    ,STRING*08,VALUE-'00000000'    /.バッチ取./
    VAR ?SOKCD    ,STRING*02,VALUE-'00'          /.倉庫    ./
    VAR ?NDATE    ,STRING*08,VALUE-'00000000'    /.納品日　./
    VAR ?CHGTEN   ,STRING*05,VALUE-'00000'       /.対象店舗./
    VAR ?CHGJYK   ,STRING*10,VALUE-'          '  /.変更需要家./

    VAR ?SDEN01   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN01   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN02   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN02   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN03   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN03   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN04   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN04   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN05   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN05   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN06   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN06   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN07   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN07   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN08   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN08   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN09   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN09   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN10   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN10   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN11   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN11   ,STRING*09,VALUE-'000000000'
    VAR ?SDEN12   ,STRING*09,VALUE-'000000000'
    VAR ?EDEN12   ,STRING*09,VALUE-'000000000'

    VAR ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                  /.      2    ./
    VAR ?OPR3     ,STRING*50                  /.      3    ./
    VAR ?OPR4     ,STRING*50                  /.      4    ./
    VAR ?OPR5     ,STRING*50                  /.      5    ./

/.##DEFLIBL##./
    DEFLIBL TOKELIB/TOKELIBO/TOKMDLIB/TOKFLIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?PGNM :=  '連携店舗変更（オンライン）'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKO,?DSOKO)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##連携店舗変更指示（オンライン）##./
TSY0801I:

    ?STEP :=   'TSY0801I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-KYKJYKL1,TOFILE-KYKJYKL1.TOKDTLIB
    OVRF      FILE-KYKJYKL2,TOFILE-KYKJYKL2.TOKDTLIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    CALL      PGM-TSY0801I.TOKSOLIB,
              PARA-(?SOKO,?DSOKO,?BUMON,?TANCD,        /.IN ./
                    ?BDATE,?BTIME,?BTORI,              /.OUT./
                    ?SOKCD,?NDATE,?CHGTEN,?CHGJYK,     /.OUT./
                    ?SDEN01,?EDEN01,?SDEN02,?EDEN02,   /.OUT./
                    ?SDEN03,?EDEN03,?SDEN04,?EDEN04,   /.OUT./
                    ?SDEN05,?EDEN05,?SDEN06,?EDEN06,   /.OUT./
                    ?SDEN07,?EDEN07,?SDEN08,?EDEN08,   /.OUT./
                    ?SDEN09,?EDEN09,?SDEN10,?EDEN10,   /.OUT./
                    ?SDEN11,?EDEN11,?SDEN12,?EDEN12)   /.OUT./
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 := '連携店舗変更指示（オンライン）'
              GOTO ABEND
         END
    END

    ?MSGX := '## バッチ日　 = ' && ?BDATE && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## バッチ時　 = ' && ?BTIME && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## バッチ取　 = ' && ?BTORI && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 倉庫ＣＤ　 = ' && ?SOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 納品日　　 = ' && ?NDATE && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 対象店舗　 = ' && ?CHGTEN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 需要家ＩＤ = ' && ?CHGJYK && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始01 = ' && ?SDEN01 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了01 = ' && ?EDEN01 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始02 = ' && ?SDEN02 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了02 = ' && ?EDEN02 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始03 = ' && ?SDEN03 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了03 = ' && ?EDEN03 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始04 = ' && ?SDEN04 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了04 = ' && ?EDEN04 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始05 = ' && ?SDEN05 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了05 = ' && ?EDEN05 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始06 = ' && ?SDEN06 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了06 = ' && ?EDEN06 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始07 = ' && ?SDEN07 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了07 = ' && ?EDEN07 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始08 = ' && ?SDEN08 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了08 = ' && ?EDEN08 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始09 = ' && ?SDEN09 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了09 = ' && ?EDEN09 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始10 = ' && ?SDEN10 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了10 = ' && ?EDEN10 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始11 = ' && ?SDEN11 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了11 = ' && ?EDEN11 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 伝票開始12 = ' && ?SDEN12 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '## 　　終了12 = ' && ?EDEN12 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##連携店舗変更更新（オンライン）##./
TSY0802B:

    ?STEP :=   'TSY0802B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    CALL      PGM-TSY0802B.TOKSOLIB,
              PARA-(?BDATE,?BTIME,?BTORI,              /.IN./
                    ?SOKCD,?NDATE,?CHGTEN,?CHGJYK,     /.IN./
                    ?SDEN01,?EDEN01,?SDEN02,?EDEN02,   /.IN./
                    ?SDEN03,?EDEN03,?SDEN04,?EDEN04,   /.IN./
                    ?SDEN05,?EDEN05,?SDEN06,?EDEN06,   /.IN./
                    ?SDEN07,?EDEN07,?SDEN08,?EDEN08,   /.IN./
                    ?SDEN09,?EDEN09,?SDEN10,?EDEN10,   /.IN./
                    ?SDEN11,?EDEN11,?SDEN12,?EDEN12)   /.IN./

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '連携店舗変更更新（オンライン）'
              GOTO ABEND END

RTN:

    ?PGMEC    :=    @PGMEC
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '連携店舗変更（オンライン）異常終了。'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
