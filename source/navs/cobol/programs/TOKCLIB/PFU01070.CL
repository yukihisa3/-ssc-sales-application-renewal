/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫システム　　　　　　　           *  ./
/. *   JOB-ID      :    PFU01070                             *  ./
/. *   JOB-NAME    :    日次振替                             *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PFU01070'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    DEFLIBL TOKELIB/TOKFLIB
/.##振替ｴﾗｰ部門毎振分け##./
SFU0107B:

    ?STEP :=   'SFU0107B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKFRRE2.TOKWLIB
    OVRF      FILE-HON,TOFILE-TOKFHO.TOKWLIB
    OVRF      FILE-FUK,TOFILE-TOKFFU.TOKWLIB
    OVRF      FILE-SEN,TOFILE-TOKFSE.TOKWLIB
    OVRF      FILE-HOK,TOFILE-TOKFHK.TOKWLIB
    OVRF      FILE-OSA,TOFILE-TOKFOS.TOKWLIB
    OVRF      FILE-OKA,TOFILE-TOKFOK.TOKWLIB
    OVRF      FILE-BUTOKMF1,TOFILE-BUTOKML1.TOKFLIB
    OVRF      FILE-BUTOKMF2,TOFILE-BUTOKML2.TOKFLIB
    CALL      PGM-SFU0107B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##日次振替ﾘｽﾄ発行##./
SFU0110L:

    ?PGMID := 'SFU0110L'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替リスト発行       ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-FURIKAL2,TOFILE-FURIKAL2.TOKFLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF FILE-BUTOKML1,TOFILE-BUTOKML1.TOKFLIB
    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
/.  CALL PGM-SFU0110L.TOKELIB  ./
    IF   @PGMEC      ^=  0
       THEN GOTO     ABEND
    END

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