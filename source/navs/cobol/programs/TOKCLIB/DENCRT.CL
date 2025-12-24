/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    オンライン                           *  ./
/. *   JOB-ID      :    PSKTCNV                              *  ./
/. *   JOB-NAME    :    量販店データ変換                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSKTCNV '
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL SKTFLIB/SKTELIB
/.  FILE ASSIGN ./
FASSIN:

/.  ASSIGN  FILE-HKYOTU.SKTFLIB!@XCL

    IF      @PGMEC  ^=   0
       THEN GOTO         ASSERR
    END
    ASSIGN  FILE-HRYOJNL.SKTFLIB!@XCL

    IF      @PGMEC  ^=   0
       THEN GOTO         ASSERR
    END     ./

/.  処理確認                                                    ./
OSKT045:

    ?STEP :=   'OSKT045 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-SKTELIB
    CALL      PGM-OSKT045.SKTELIB
    IF        @PGMEC     =   155  THEN
              SNDMSG MSG-'***   取消終了 OSKT045   ***',TO-XCTL
              GOTO RTN   END
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.  量販データＦコンバート                                      ./
OSKT050:

    ?STEP :=   'OSKT050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-SKTELIB
    OVRF      FILE-RYOJNL1,TOFILE-RYOJNL1.SKTFLIB
/.  OVRF      FILE-HKYOTU,TOFILE-HKYOTU.SKTFLIB  ./
    OVRF      FILE-HKYOTU,TOFILE-RYOWK.SKTFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.SKTFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.SKTFLIB
    CALL      PGM-OSKT050.SKTELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.取引先、納品日、出荷場所、伝票番号、店舗コード、行番号./
     SORT     INFILE-RYOWK.SKTFLIB,INRL-256,INBF-15,
              OUTFILE-RYOWK.SKTFLIB,OUTBF-15,
              KEY-1!8!CA,KEY1-54!5!PA,KEY2-38!2!CA,KEY3-9!9!CA,
              KEY4-33!5!CA,KEY5-18!2!CA,
              SEQ-@YES,RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.  伝票_ＣＮＴ                                                ./
OSKT055:

    ?STEP :=   'OSKT055 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-SKTELIB
    OVRF      FILE-KYOTUX,TOFILE-RYOWK.SKTFLIB
    OVRF      FILE-HDENJNL,TOFILE-SHTDENF.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SSY0604T.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.  条件Ｆメモ_更新                                              ./
OSKT056:

    ?STEP :=   'OSKT056 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.SKTFLIB
    CALL      PGM-OSKT056.SKTELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    CLRFILE FILE-HKYOTU.SKTFLIB
    CLRFILE FILE-HRYOJNL.SKTFLIB

    GOTO      RTN

ASSERR:

    PAUSE     '他の端末で処理がおこなわれています　　　　　　'

    GOTO      FASSIN
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