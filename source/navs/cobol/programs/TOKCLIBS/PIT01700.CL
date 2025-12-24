/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    マスタメンテ                         *  ./
/. *   JOB-ID      :    PIT01700                             *  ./
/. *   JOB-NAME    :    担当者マスタ保守（ＩＴ統制）         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PIT01700'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMONI   ,STRING*4,VALUE-'    '     /.部門CD./
    VAR       ?TANTOI   ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?UPDT     ,STRING*8,VALUE-'        ' /.更新日./
    VAR       ?UPTM     ,STRING*6,VALUE-'      '   /.更新時間./
    VAR       ?UPDTE    ,STRING*8,VALUE-'99999999'
    VAR       ?UPTME    ,STRING*6,VALUE-'999999'
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##部門ｺｰﾄﾞ担当者ｺｰﾄﾞ取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMONI,?TANTOI)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##担当者マスタメンテ##./
SIT0170I:

    ?STEP :=   'SIT0170I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF     FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF     FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF     FILE-MSTLOGL1,TOFILE-MSTLOGL1.TOKKLIB
    CALL PGM-SIT0170I.TOKELIBO ,PARA-(?BUMONI,?TANTOI,?UPDT,
                                      ?UPTM)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##担当者ﾏｽﾀﾘｽﾄ##./
SIT0180L:

    ?STEP :=   'SIT0180L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
/.  OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIBO ./
/.##本社ﾚｰｻﾞｰへ出力##./
    IF ?BUMONI = '2920' THEN
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    ELSE
       OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
    END
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-MSTLOGL3,TOFILE-MSTLOGL3.TOKKLIB
    CALL      PGM-SIT0180L.TOKELIBO,PARA-(?BUMONI,?TANTOI,?UPDT,?UPTM,
                                          ?UPDTE,?UPTME)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

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