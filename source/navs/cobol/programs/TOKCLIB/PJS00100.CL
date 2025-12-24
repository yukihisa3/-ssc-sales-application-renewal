/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    実績管理システム　　　　　           *  ./
/. *   JOB-ID      :    PJS00100                             *  ./
/. *   JOB-NAME    :    実績累積Ｆ作成                       *  ./
/. *   UPDATE      :    20120224　基幹サーバ統合             *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PJS00100'
    VAR       ?STEP   ,STRING*8

/.##実績累積Ｆ作成##./
SJS0010B:

    ?STEP :=   'SJS0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 実績累積Ｆ　作成 ##'
    SNDMSG    ?MSGX,TO-XCTL

 /. 20111110 TOKU -> TOKUJ  ./
 /. OVRF      FILE-TOKU,TOFILE-TOKU.TOKWLIB      ./
    OVRF      FILE-TOKUJ,TOFILE-TOKUJ.TOKWLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    OVRF      FILE-HIDUKEL1,TOFILE-HIDUKEL1.TOKWLIB
    CALL      PGM-SJS0010B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##実績集計ファイル作成                                        ./
SJS0030B:
    ?STEP :=   'SJS0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 実績集計Ｆ　集計 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RUISEKL2,TOFILE-RUISEKL2.TOKFLIB
    OVRF      FILE-JISSYUL1,TOFILE-JISSYUL1.TOKFLIB
    OVRF      FILE-JISSSYU1,TOFILE-JISSSYU1.TOKKLIB  /.20111110 ADD./
    CALL      PGM-SJS0030B.TOKELIB
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