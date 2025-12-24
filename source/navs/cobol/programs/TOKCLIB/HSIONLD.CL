/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    イオンオンラインシステム             *  ./
/. *   JOB-ID      :    HSIONLD                              *  ./
/. *   JOB-NAME    :    イオン請求データ配信                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'HSIONLD '
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM     ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃ＣＶＣＳ（アークランド）作成＃＃',TO-XCTL

/.     ２５６ｂｙｔｅから２８０ｂｙｔｅへ変換                   ./
CVCS280X:

    DEFLIBL   CVCSLBI/CVCSOBJ

    OVRF      FILE-CVCSS008,TOFILE-CVCSS008.CVCSLBI
    OVRF      FILE-CVCSS011,TOFILE-CVCSS011.CVCSLBI
    OVRF      FILE-CVCSS017,TOFILE-CVCSS017.CVCSLBI
    OVRF      FILE-CVCSS018,TOFILE-CVCSS018.CVCSLBI

    ?STEP :=   'CVCS280X'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-INFILE,TOFILE-SEIWK.TOKFLIB
    OVRF      FILE-OUTFILE,TOFILE-CVCSL007.CVCSLBI
    CALL      PGM-CVCS280X.TOKELIB,PARA-('BB304600Q 52            ')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


    OVRF      FILE-CVCSCARD,TOFILE-IONSEI.CVCSLBI
    OVRF      FILE-CVCSL007,TOFILE-CVCSL007.CVCSLBI
    CALL      PGM-CVCS1004.CVCSOBJ,PARA-('1')
    IF        @PGMEC   ^=   0    THEN
                                 GOTO     ABEND     END
    CALL      PGM-CVCS1006.CVCSOBJ
    IF        @PGMEC   ^=   0    THEN
                                 GOTO     ABEND     END

    OVRF      FILE-CVCSCARD,TOFILE-CARD0010.CVCSLBI
    CALL      CLI1010.CVCSLBI

    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',TO-XCTL
    SNDMSG MSG-'＃　イオン殿　　　　　　　　＃',TO-XCTL
    SNDMSG MSG-'＃　＜請求データ作成完了＞　＃',TO-XCTL
    SNDMSG MSG-'＃確認後、送信処理を開始して＃',TO-XCTL
    SNDMSG MSG-'＃下さい。　　　　　　　　　＃',TO-XCTL
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',TO-XCTL

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