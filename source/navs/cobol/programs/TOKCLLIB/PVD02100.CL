/. ***********************************************************  ./
/. *     サカタのタネ　新基幹システム                        *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携（入荷予定管理）         *  ./
/. *   JOB-ID      :    PVD02100                             *  ./
/. *   JOB-NAME    :    発注消し込み入力チェックリスト       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PVD02100'
    VAR       ?STEP     ,STRING*8
    VAR       ?SOKO     ,STRING*2
    VAR       ?JISOKO   ,STRING*2
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門名./
/.##ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?NWKSTN   :=        @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL    D365DLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKMDLIB/TOKELIBO

/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WKSTN,?BUMON)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##倉庫ＣＤ取得##./
SKY1601B:
    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WKSTN,?SOKO,?JISOKO)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##発注消込チェックリスト##./
NVD0210L:
    ?STEP :=   'NVD0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-NYKFILL2,TOFILE-NYKFILL2.D365DLIB
    OVRF      FILE-HACHEDL1,TOFILE-HACHEDL1.D365DLIB
    OVRF      FILE-HACMEIL1,TOFILE-HACMEIL1.D365DLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-NVD0210L.TOKSOLIB,PARA-(?SOKO,?JISOKO,?BUMON)
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