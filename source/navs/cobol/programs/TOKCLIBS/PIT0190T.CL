/. ***********************************************************  ./
/. *     サカタのタネ　ＩＴ統制　　　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    マスタ更新履歴プルーフ               *  ./
/. *   JOB-ID      :    PIT01900  2009/03/23 - 03/26         *  ./
/. *   JOB-NAME    :    マスタ更新履歴出力指示               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PIT01900'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門CD./
    VAR       ?TANCD    ,STRING*8,VALUE-'        ' /.担当者./
    VAR       ?SOUSASYU ,STRING*1,VALUE-' '        /.操作種別./
    VAR       ?TANST    ,STRING*8,VALUE-'  '       /.担当者CD./
    VAR       ?MKUBUN   ,STRING*2,VALUE-'01'       /.マスタ区分./
    VAR       ?NYUST    ,STRING*8,VALUE-'20090324' /.更新開始日./
    VAR       ?NYUED    ,STRING*8,VALUE-'20090325' /.更新終了日./
    VAR       ?STIME    ,STRING*6,VALUE-'000000'   /.時刻./
    VAR       ?ETIME    ,STRING*6,VALUE-'999999'   /.時刻./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?P1       ,STRING*34                 /.帳票出力ﾊﾟﾗﾒﾀ./

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

/.##部門ｺｰﾄﾞ取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

              ?MSGX :=  '操作種別 = ' && ?SOUSASYU
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '部門 = ' && ?BUMON
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '担当者 = ' && ?TANCD
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  'マスタ区分 = ' && ?MKUBUN
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '更新日開始 = ' && ?NYUST
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '更新日終了 = ' && ?NYUED
              SNDMSG    ?MSGX,TO-XCTL


/.##各マスタリスト##./

/.##本社ﾚｰｻﾞｰへ出力##./
       OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
/.  END  ./
    IF ?MKUBUN = '01'  THEN
       GOTO    SIT0040L
    END
    IF ?MKUBUN = '02'  THEN
       GOTO    SIT0060L
    END
    IF ?MKUBUN = '03'  THEN
       GOTO    SIT0080L
    END
    IF ?MKUBUN = '04'  THEN
       GOTO    SIT0100L
    END
    IF ?MKUBUN = '05'  THEN
       GOTO    SIT0120L
    END
    IF ?MKUBUN = '06'  THEN
       GOTO    SIT0140L
    END
    IF ?MKUBUN = '07'  THEN
       GOTO    SIT0160L
    END
    IF ?MKUBUN = '08'  THEN
       GOTO    SIT0180L
    END
    IF ?MKUBUN = '09'  THEN
       GOTO    SIT0025L
    END

    GOTO  ABEND
SIT0040L:
    ?STEP :=   'SIT0040L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0040L.TOKELIBO,PARA-(?BUMON,?TANCD,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK
SIT0060L:
    ?STEP :=   'SIT0060L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0060L.TOKELIBO,PARA-(?BUMON,?TANST,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK
SIT0080L:
    ?STEP :=   'SIT0080L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0080L.TOKELIBO,PARA-(?BUMON,?TANST,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK
SIT0100L:
    ?STEP :=   'SIT0100L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0100L.TOKELIBO,PARA-(?BUMON,?TANST,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK
SIT0120L:
    ?STEP :=   'SIT0120L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0120L.TOKELIBO,PARA-(?BUMON,?TANST,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK
SIT0140L:
    ?STEP :=   'SIT0140L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0140L.TOKELIBO,PARA-(?BUMON,?TANST,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK
SIT0160L:
    ?STEP :=   'SIT0160L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0160L.TOKELIBO,PARA-(?BUMON,?TANST,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK
SIT0180L:
    ?STEP :=   'SIT0180L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0180L.TOKELIBO,PARA-(?BUMON,?TANST,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK
SIT0025L:
    ?STEP :=   'SIT0025L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    CALL PGM-SIT0025L.TOKELIBO,PARA-(?BUMON,?TANST,
                   ?NYUST,?STIME,?NYUED,?ETIME)
    GOTO  PGCHK

PGCHK:
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