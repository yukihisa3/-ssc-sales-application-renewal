/. ***********************************************************  ./
/. *     サカタのタネ　特販システム                          *  ./
/. *   SYSTEM-NAME :                                         *  ./
/. *   JOB-ID      :    PKY95020                             *  ./
/. *   JOB-NAME    :    宛名タックシール                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PKY95020'
    VAR       ?STEP     ,STRING*8
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./
    VAR ?KEYA     ,STRING*1,VALUE-'1'           /.KEY1./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNMH   ,STRING*6,VALUE-'      '
    VAR ?FILNMD   ,STRING*6,VALUE-'DENPYO'
    VAR ?FILNMO   ,STRING*5,VALUE-'ZOKUR'
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?JHSDENF  ,NAME!MOD
    VAR ?JHSDENFN ,STRING*17
    VAR ?DENPYO   ,NAME!MOD
    VAR ?DENPYON  ,STRING*17
    VAR ?ZOKUR    ,NAME!MOD
    VAR ?ZOKURN   ,STRING*17
    VAR ?ZOKUR1   ,NAME!MOD
    VAR ?ZOKUR1N  ,STRING*17

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END
/.##西尾特別処理##./
    IF        ?WS = 'WKSTNH86'  THEN
              ?SOKCD := '8A'
    END
    IF        ?WS = 'WKSTNH8A'  THEN
              ?SOKCD := '8B'
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?SOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKCD
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZOKUR    :=    %NCAT(?FILID,?LIBID)
    ?ZOKURN   :=    %STRING(?ZOKUR)
    ?MSGX     :=    '## 中間F(PF)  = ' && ?ZOKURN  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKCD && ?KEYA
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZOKUR1   :=    %NCAT(?FILID,?LIBID)
    ?ZOKUR1N  :=    %STRING(?ZOKUR1)
    ?MSGX     :=    '## ユポF(LF1) = ' && ?ZOKUR1N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  宛名タックシール                                        ./
SKY1302L:

    ?STEP :=   'SKY1302L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
/. *************** テスト用 **************************** ./
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
/. ***************************************************** ./
/.  OVRPRTF   FILE-PRTF,TOFILE-PRTF2.TOKELIB             ./
    OVRF      FILE-ZOKURIF,TOFILE-?ZOKUR
    CALL      PGM-SKY1302L.TOKELIB
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