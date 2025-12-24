/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                   　　     *  ./
/. *   JOB-ID      :    PSY02198                             *  ./
/. *   JOB-NAME    :    オンラインデータ発行　　　　　　　  *  ./
/. *               :    　　　　　　　                       *  ./
/. ***********************************************************  ./
    PGM  (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*1,IN,VALUE-' ' /.1=自,2=選択,3.倉 ./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNMH   ,STRING*6,VALUE-'      '
    VAR ?FILNMD   ,STRING*6,VALUE-'DENPYO'
    VAR ?FILNMO   ,STRING*5,VALUE-'ONLDT'
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '
    VAR ?LIBNMW   ,STRING*8,VALUE-'TOKWWLIB'
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?JHSDENF  ,NAME!MOD
    VAR ?JHSDENFN ,STRING*17
    VAR ?DENPYO   ,NAME!MOD
    VAR ?DENPYON  ,STRING*17
    VAR ?ONLDT    ,NAME!MOD
    VAR ?ONLDTN   ,STRING*17
    VAR ?ONLDT1   ,NAME!MOD
    VAR ?ONLDT1N  ,STRING*17
    VAR ?ONLDT2   ,NAME!MOD
    VAR ?ONLDT2N  ,STRING*17
    VAR ?ONLDT3   ,NAME!MOD
    VAR ?ONLDT3N  ,STRING*17
    VAR ?ONLDT4   ,NAME!MOD
    VAR ?ONLDT4N  ,STRING*17
    VAR ?ONLDT5   ,NAME!MOD
    VAR ?ONLDT5N  ,STRING*17
    VAR ?ONLDT6   ,NAME!MOD
    VAR ?ONLDT6N  ,STRING*17
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY02198'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?SOKO     ,STRING*2,VALUE-'  '          /.入力倉庫ｺｰﾄﾞ./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./
    VAR ?JYUN     ,STRING*1,VALUE-' '           /.出力順./
    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日付./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時間./
    VAR ?TORICD   ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR ?NOUDT1   ,STRING*8,VALUE-'00000000'    /.開始納品日./
    VAR ?NOUDT2   ,STRING*8,VALUE-'00000000'    /.終了納品日./
    VAR ?BUMON1   ,STRING*4,VALUE-'0000'        /.開始部門./
    VAR ?BUMON2   ,STRING*4,VALUE-'0000'        /.終了部門./
    VAR ?TENCD1   ,STRING*5,VALUE-'00000'       /.開始店舗./
    VAR ?TENCD2   ,STRING*5,VALUE-'00000'       /.終了店舗./
    VAR ?CENR1    ,STRING*1,VALUE-'0'           /.開始ｾﾝﾀｰ区分./
    VAR ?CENR2    ,STRING*1,VALUE-'0'           /.終了ｾﾝﾀｰ区分./
    VAR ?PGCD     ,STRING*8,VALUE-'        '    /.ﾌﾟﾛｸﾞﾗﾑCD./
    VAR ?LIBCD    ,STRING*8,VALUE-'        '    /.ﾗｲﾌﾞﾗﾘCD./
    VAR ?OVRFN    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名./
    VAR ?KEYA     ,STRING*1,VALUE-'1'           /.KEY1./
    VAR ?KEYB     ,STRING*1,VALUE-'2'           /.KEY2./
    VAR ?KEYC     ,STRING*1,VALUE-'3'           /.KEY3./
    VAR ?KEYD     ,STRING*1,VALUE-'4'           /.KEY4./
    VAR ?KEYE     ,STRING*1,VALUE-'5'           /.KEY5./
    VAR ?KEYF     ,STRING*1,VALUE-'6'           /.KEY6./
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL TOKFLIB/TOKELIB/TOKDLIB

/.##自倉庫／選択倉庫判定##./
CHK:
    ?FILNMH := 'JHSDEN'
    IF      ?KBN   =    '3'   THEN
            ?FILNMH := 'SOKDEN'
    END
/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##オンラインデータ発行指示入力##./
SKY2198I:

    ?STEP :=   'SSY2198I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKDLIB

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF     FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF     FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF     FILE-DENHJYO1,TOFILE-DENHJYO1.TOKKLIB
    CALL  PGM-SKY2198I.TOKELIBO,PARA-(?SOKO,?JYUN,?NOUDT1,?NOUDT2
                                ,?TENCD1,?TENCD2,?BUMON1,?BUMON2
                                ,?CENR1,?CENR2
                                ,?TORICD,?PGCD,?LIBCD,?OVRFN
                                ,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?SOKO && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?SOKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?JHSDENF  :=    %NCAT(?FILID,?LIBID)
    ?JHSDENFN :=    %STRING(?JHSDENF)
    ?MSGX     :=    '## 倉庫ﾌｧｲﾙ名 = ' && ?JHSDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNMD && ?SOKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?DENPYO   :=    %NCAT(?FILID,?LIBID)
    ?DENPYON  :=    %STRING(?DENPYO)
    ?MSGX     :=    '## 伝票ﾌｧｲﾙ名 = ' && ?DENPYON && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ONLDT    :=    %NCAT(?FILID,?LIBID)
    ?ONLDTN   :=    %STRING(?ONLDT)
    ?MSGX     :=    '## 中間F(PF)  = ' && ?ONLDTN  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO && ?KEYA
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNMW)
    ?ONLDT1   :=    %NCAT(?FILID,?LIBID)
    ?ONLDT1N  :=    %STRING(?ONLDT1)
    ?MSGX     :=    '## 中間F(LF1) = ' && ?ONLDT1N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO && ?KEYB
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNMW)
    ?ONLDT2   :=    %NCAT(?FILID,?LIBID)
    ?ONLDT2N  :=    %STRING(?ONLDT2)
    ?MSGX     :=    '## 中間F(LF2) = ' && ?ONLDT2N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO  && ?KEYC
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNMW)
    ?ONLDT3   :=    %NCAT(?FILID,?LIBID)
    ?ONLDT3N  :=    %STRING(?ONLDT3)
    ?MSGX     :=    '## 中間F(LF3) = ' && ?ONLDT3N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO  && ?KEYD
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNMW)
    ?ONLDT4   :=    %NCAT(?FILID,?LIBID)
    ?ONLDT4N  :=    %STRING(?ONLDT4)
    ?MSGX     :=    '## 中間F(LF4) = ' && ?ONLDT4N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO  && ?KEYE
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNMW)
    ?ONLDT5   :=    %NCAT(?FILID,?LIBID)
    ?ONLDT5N  :=    %STRING(?ONLDT5)
    ?MSGX     :=    '## 中間F(LF5) = ' && ?ONLDT5N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO  && ?KEYF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNMW)
    ?ONLDT6   :=    %NCAT(?FILID,?LIBID)
    ?ONLDT6N  :=    %STRING(?ONLDT6)
    ?MSGX     :=    '## 中間F(LF6) = ' && ?ONLDT6N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


/.##オンラインデータ物理Ｆへコピー##./
PSETPF:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-?JHSDENF,TOFILE-?ONLDT,ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    SNDMSG ?SOKO  ,TO-XCTL.@ORGPROF
    SNDMSG ?JYUN  ,TO-XCTL.@ORGPROF
    SNDMSG ?JDATE ,TO-XCTL.@ORGPROF
    SNDMSG ?JTIME ,TO-XCTL.@ORGPROF
    SNDMSG ?TORICD,TO-XCTL.@ORGPROF
    SNDMSG ?PGCD  ,TO-XCTL.@ORGPROF
    SNDMSG ?LIBCD,TO-XCTL.@ORGPROF
    SNDMSG ?OVRFN,TO-XCTL.@ORGPROF
    SNDMSG ?NOUDT1,TO-XCTL.@ORGPROF
    SNDMSG ?NOUDT2,TO-XCTL.@ORGPROF
    SNDMSG ?TENCD1,TO-XCTL.@ORGPROF
    SNDMSG ?TENCD2,TO-XCTL.@ORGPROF
    SNDMSG ?BUMON1,TO-XCTL.@ORGPROF
    SNDMSG ?BUMON2,TO-XCTL.@ORGPROF
    SNDMSG ?CENR1,TO-XCTL.@ORGPROF
    SNDMSG ?CENR2,TO-XCTL.@ORGPROF

    DLTOVRF FILE-JHSDENF
/.##出力順別抽出処理##./
SKY2199B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-ONLDT1,TOFILE-?ONLDT1
    OVRF      FILE-ONLDT2,TOFILE-?ONLDT2
    OVRF      FILE-ONLDT3,TOFILE-?ONLDT3
    OVRF      FILE-ONLDT4,TOFILE-?ONLDT4
    OVRF      FILE-ONLDT5,TOFILE-?ONLDT5
    OVRF      FILE-ONLDT6,TOFILE-?ONLDT6
    OVRF      FILE-JHSDENF,TOFILE-?DENPYO
    CALL      PGM-SKY2199B.TOKELIBO,PARA-(?JYUN,?NOUDT1,?NOUDT2
                                  ,?TENCD1,?TENCD2
                                  ,?BUMON1,?BUMON2,?CENR1,?CENR2)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.  仕入伝票発行（ＴＡ_型）                              ./
PSY32020:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG    ?PGCD,TO-XCTL
    IF  ?PGCD = 'SSY1407L'      THEN
      OVRPRTF   FILE-PRTF,TOFILE-DOITOPRT.TOKELIBO,MEDLIB-TOKELIBO
/.## ﾌﾟﾘﾝﾀｰ切替 ##./
        IF  ?KBN   =    '1'   THEN
            CHGCMVAR '@OUTQN',XOUTQ1
        END
        IF  ?KBN   =    '2'   THEN
            CHGCMVAR '@OUTQN',XOUTQ1
        END
        IF  ?KBN   =    '3'   THEN
            IF  ?WS  =  'WKSTNH83'  THEN
                CHGCMVAR '@OUTQN',XXPXLX83
            END
        END
    END
    IF  ?PGCD = 'SSY1406L'   THEN
      OVRPRTF   FILE-PRTF,TOFILE-DOITOPRT.TOKELIB,MEDLIB-TOKELIB
/.## ﾌﾟﾘﾝﾀｰ切替 ##./
        IF  ?KBN   =    '1'   THEN
            CHGCMVAR '@OUTQN',XOUTQ1
        END
        IF  ?KBN   =    '2'   THEN
            CHGCMVAR '@OUTQN',XOUTQ1
        END
        IF  ?KBN   =    '3'   THEN
            IF  ?WS  =  'WKSTNH83'  THEN
                CHGCMVAR '@OUTQN',XXPXLX83
            END
        END
    END
    IF  ?PGCD = 'SSY1411L'   THEN
      OVRPRTF   FILE-XU04LP,TOFILE-DONKIPRT.TOKELIBO
/.## ﾌﾟﾘﾝﾀｰ切替 ##./
        IF  ?KBN   =    '1'   THEN
            CHGCMVAR '@OUTQN',XOUTQ1
        END
        IF  ?KBN   =    '2'   THEN
            CHGCMVAR '@OUTQN',XOUTQ1
        END
        IF  ?KBN   =    '3'   THEN
            IF  ?WS  =  'WKSTNH83'  THEN
                CHGCMVAR '@OUTQN',XXPXLX83
            END
            IF  ?WS  =  'WKSTNH6A'  THEN
                CHGCMVAR '@OUTQN',XXPSKRQL
            END
            IF  ?WS  =  'WKSTNH6B'  THEN
                CHGCMVAR '@OUTQN',XXPSKRQL
            END
            IF  ?WS  =  'WKSTNHS1'  THEN
                CHGCMVAR '@OUTQN',XXPSKRQL
            END
            IF  ?WS  =  'WKSTNHS2'  THEN
                CHGCMVAR '@OUTQN',XXPSKRQL
            END
        END
    END
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?OVRFNAME := %NAME(?OVRFN)
    OVRF      FILE-?OVRFNAME,TOFILE-?DENPYO
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    CALL      PGM-?PGC
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

/.## ﾌﾟﾘﾝﾀｰ切替 ##./
    IF ?PGCD = 'SSY1407L' THEN
       IF      ?KBN   =    '1'   THEN
            CHGCMVAR '@OUTQN',XSYSLSTQ
       END
       IF      ?KBN   =    '2'   THEN
            CHGCMVAR '@OUTQN',XSYSLSTQ
       END
       IF      ?KBN   =    '3'   THEN
            IF  ?WS  =  'WKSTNH83'  THEN
                CHGCMVAR '@OUTQN',XXPTOMQ
            END
       END
    END
    IF ?PGCD = 'SSY1406L' THEN
       IF      ?KBN   =    '1'   THEN
            CHGCMVAR '@OUTQN',XSYSLSTQ
       END
       IF      ?KBN   =    '2'   THEN
            CHGCMVAR '@OUTQN',XSYSLSTQ
       END
       IF      ?KBN   =    '3'   THEN
            IF  ?WS  =  'WKSTNH83'  THEN
                CHGCMVAR '@OUTQN',XXPTOMQ
            END
       END
    END
    IF ?PGCD = 'SSY1411L' THEN
       IF      ?KBN   =    '1'   THEN
            CHGCMVAR '@OUTQN',XSYSLSTQ
       END
       IF      ?KBN   =    '2'   THEN
            CHGCMVAR '@OUTQN',XSYSLSTQ
       END
       IF      ?KBN   =    '3'   THEN
            IF  ?WS  =  'WKSTNH83'  THEN
                CHGCMVAR '@OUTQN',XXPTOMQ
            END
            IF  ?WS  =  'WKSTNH6A'  THEN
                CHGCMVAR '@OUTQN',XXPKATQ
            END
            IF  ?WS  =  'WKSTNH6B'  THEN
                CHGCMVAR '@OUTQN',XXPKATQ
            END
            IF  ?WS  =  'WKSTNHS1'  THEN
                CHGCMVAR '@OUTQN',XXPSKRQ1
            END
            IF  ?WS  =  'WKSTNHS2'  THEN
                CHGCMVAR '@OUTQN',XXPSKRQ1
            END
       END
    END
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
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