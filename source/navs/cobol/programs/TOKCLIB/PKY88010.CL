/. ***********************************************************  ./
/. *     サカタのタネ                                        *  ./
/. *   SYSTEM-NAME :                                         *  ./
/. *   JOB-ID      :    PKY88010                             *  ./
/. *   JOB-NAME    :    データ退避                           *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PKY88010'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                /.      2    ./
    VAR       ?OPR3     ,STRING*50                /.      3    ./
    VAR       ?OPR4     ,STRING*50                /.      4    ./
    VAR       ?OPR5     ,STRING*50                /.      5    ./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./
    VAR ?KEYA     ,STRING*1,VALUE-'1'           /.KEY1./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB

    ?OPR1  :=  '　ユポラベルデータを退避を行います。'
    ?OPR2  :=  '　ＦＰＤをセットして下さい。'
    ?OPR3  :=  ''
    ?OPR4  :=  ''
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)


/.##ﾃﾞｰﾀ退避##./
PCNVFILE:

    ?STEP :=   'PCNVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-ZYUPO8A.TOKFLIB,TOFILE-NISYUPO1.TOKWLIB,
            ADD-@NO,BF-27
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##ＦＰＤコピー##./
FPDCPY:

    ?STEP :=   'FPDCPY  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-ZYUPO8A.TOKFLIB,PARA-YUPO1,UNIT-1
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