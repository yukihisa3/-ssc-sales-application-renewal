/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    マスタリスト                         *  ./
/. *   JOB-ID      :    PIT00500                             *  ./
/. *   JOB-NAME    :    仕入先マスタメンテ （ＩＴ統制）      *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PIT00500'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門CD./
    VAR       ?TANTO    ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?UPDT     ,STRING*8,VALUE-'        ' /.更新日./
    VAR       ?UPTM     ,STRING*6,VALUE-'      '   /.更新時間./
    VAR       ?UPDTE    ,STRING*8,VALUE-'99999999' /.更新日./
    VAR       ?UPTME    ,STRING*6,VALUE-'999999'   /.更新時間./
    VAR       ?NYUED    ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?PMODE    ,STRING*1,VALUE-' '        /.担当者CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?P1       ,STRING*34                 /.帳票出力ﾊﾟﾗﾒﾀ./
    VAR       ?STIME    ,STRING*4,VALUE-'0000'     /.時刻./
    VAR       ?ETIME    ,STRING*4,VALUE-'9999'     /.時刻./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKFLIBO

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
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANTO)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##仕入先ﾏｽﾀﾒﾝﾃ##./
SIT0050I:

    ?STEP :=   'SIT0050I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-MSTLOGL1,TOFILE-MSTLOGL1.TOKKLIB
    CALL      PGM-SIT0050I.TOKELIBO,PARA-(?BUMON,?TANTO,?UPDT,?UPTM)
    IF        @PGMEC    ^=   0  THEN
              IF  @PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  GOTO  ABEND
              END
    END

/.##仕入先ﾏｽﾀﾘｽﾄ##./
SIT0060L:

    ?STEP :=   'SIT0060L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

/.##本社ﾚｰｻﾞｰへ出力##./
    IF ?BUMON = '2920' THEN
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    ELSE
       IF ?BUMON = '2940' THEN
          OVRPRTF FILE-XU04LP,TOFILE-AMAGILBP.XUCL
       ELSE
          OVRPRTF FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
       END
    END
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-MSTLOGL3,TOFILE-MSTLOGL3.TOKKLIB
    CALL      PGM-SIT0060L.TOKELIBO,PARA-(?BUMON,?TANTO,?UPDT,?UPTM,
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