/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理サブシステム                 *  ./
/. *   JOB-ID      :    PSSY0023                             *  ./
/. *   JOB-NAME    :    手書伝票データ振分け                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0023'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.ライブラリリストの登録./
    DEFLIBL TOKFLIB/TOKELIB

/.##伝票発行場所処理確認##./
SSY0024B:

    ?STEP :=   'SSY0024B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY0024I.TOKELIB
    IF        @PGMEC     =   155  THEN
              SNDMSG MSG-'***   取消終了 OSKT057   ***',TO-XCTL
              GOTO RTN   END
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##伝票発行場所振り分け##./
SSY0023B:

    ?STEP :=   'SSY0023B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'***   伝票発行場所振分中   ***',TO-XCTL
    OVRF      FILE-SHTDENL6,TOFILE-SHTDENL6.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-JHTDEN01,TOFILE-JHTDEN01.TOKFLIB /.本社./
    OVRF      FILE-JHTDEN02,TOFILE-JHTDEN6A.TOKFLIB /.片岡./
    OVRF      FILE-JHTDEN03,TOFILE-JHTDEN63.TOKFLIB /.フバサミ./
    OVRF      FILE-JHTDEN04,TOFILE-JHTDEN84.TOKFLIB /.鴻巣./
    OVRF      FILE-JHTDEN05,TOFILE-JHTDEN86.TOKFLIB /.西尾./
    OVRF      FILE-JHTDEN06,TOFILE-JHTDENT5.TOKFLIB /.大和./
    OVRF      FILE-JHTDEN07,TOFILE-JHTDEN90.TOKFLIB /.手綱./
    OVRF      FILE-JHTDEN08,TOFILE-JHTDEN83.TOKFLIB /.富岡./
    OVRF      FILE-JHTDEN09,TOFILE-JHTDEN09.TOKFLIB /.↓以下予備./
    OVRF      FILE-JHTDEN10,TOFILE-JHTDEN10.TOKFLIB
    OVRF      FILE-JHTDEN11,TOFILE-JHTDEN11.TOKFLIB
    OVRF      FILE-JHTDEN12,TOFILE-JHTDEN12.TOKFLIB
    OVRF      FILE-JHTDEN13,TOFILE-JHTDEN13.TOKFLIB
    OVRF      FILE-JHTDEN14,TOFILE-JHTDEN14.TOKFLIB
    OVRF      FILE-JHTDEN15,TOFILE-JHTDEN15.TOKFLIB
    OVRF      FILE-JHTDEN16,TOFILE-JHTDEN16.TOKFLIB
    OVRF      FILE-JHTDEN17,TOFILE-JHTDEN17.TOKFLIB
    OVRF      FILE-JHTDEN18,TOFILE-JHTDEN18.TOKFLIB
    OVRF      FILE-JHTDEN19,TOFILE-JHTDEN19.TOKFLIB
    OVRF      FILE-JHTDEN20,TOFILE-JHTDEN20.TOKFLIB
    OVRF      FILE-SHOTBL2,TOFILE-SHOTBL2.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL
    CALL      PGM-SSY0023B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    /.##各配送場所ﾌｧｲﾙ(PF)をｺﾋﾟｰする。##./
    SETPF FILE-JHTDEN01.TOKFLIB,TOFILE-JHTTE01.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    SETPF FILE-JHTDEN6A.TOKFLIB,TOFILE-JHTTE6A.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    SETPF FILE-JHTDEN63.TOKFLIB,TOFILE-JHTTE63.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    SETPF FILE-JHTDEN84.TOKFLIB,TOFILE-JHTTE84.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    SETPF FILE-JHTDEN86.TOKFLIB,TOFILE-JHTTE86.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    SETPF FILE-JHTDENT5.TOKFLIB,TOFILE-JHTTET5.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    SETPF FILE-JHTDEN90.TOKFLIB,TOFILE-JHTTE90.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    SETPF FILE-JHTDEN83.TOKFLIB,TOFILE-JHTTE83.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    CNVFILE FILE-JHTDEN6A.TOKFLIB,TOFILE-JHTDEN62.TOKFLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-JHTDEN87.TOKFLIB,TOFILE-JHTDEN60.TOKFLIB,
            ADD-@NO,BF-1

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