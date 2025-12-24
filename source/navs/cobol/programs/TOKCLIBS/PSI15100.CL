/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    セキチュー支払照合　　　　           *  ./
/. *   JOB-ID      :    PSI15100                             *  ./
/. *   JOB-NAME    :    支払明細書作成 植物                  *  ./
/. *   新レイアウトバージョン                                *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSI15100'
    VAR       ?STEP     ,STRING*8
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名ｾｯﾄ##./
    ?PGNM :=  'セキチュー植物：支払明細書発行'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
/.  SNDMSG    ?MSGX,TO-XCTL  ./

    DEFLIBL   TOKELIBO/TOKFLIB/TOKKLIB

/.##支払明細書##./
PSI15100:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SSIHARAD,TOFILE-SEKICHUS.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI1510L.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【支払明細書発行】'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIB
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '支払明細書発行が異常終了しました。'
    ?KEKA2 :=  '再度、実行して頂き同様な場合はＮＡＶへ'
    ?KEKA3 :=  '連絡して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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