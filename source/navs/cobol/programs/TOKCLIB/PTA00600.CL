/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理システム                     *  ./
/. *   JOB-ID      :    PTA00600                             *  ./
/. *   JOB-NAME    :    実_更新                             *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. *   UPDATE      :    2020/01/20 INOUE S11301570           *  ./
/. *                                     オフコンクラウド    *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PTA00600'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


/.  在庫管理システム                                           ./
SAVFILE:

    ?OPR1  :=  '　＃＃＃＃＃＃＃　_卸確定更新処理　＃＃＃＃＃＃　'
    ?OPR2  :=  '　_卸確定処理処理を行います。'
    ?OPR3  :=  '　宜しいですか？'
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?OPR1  :=  '　＃＃＃＃＃＃＃　_卸確定更新処理　＃＃＃＃＃＃　'
    ?OPR2  :=  '　在庫ファイルのバックアップを行います。'
    ?OPR3  :=  ''
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##SAVFILE FILE-ZAMZAIF.TOKFLIB,TODEV-LTO,ADD-@YES##./
/.##SAVFILE FILE-ZAMZAIF.TOKFLIB,TODEV-@NONE,MODE-@USED,##./
/.##        TOPATH-'/NAVPC/TANAOROSI',REP-@YES,COMPRESS-@YES##./

    SAVFILE FILE-ZAMZAIF.TOKFLIB,TODEV-@NONE,MODE-@USED,
      /.社内TOPATH-'/NAVPC/PTA00600/',REP-@YES,COMPRESS-@YES./
            TOPATH-'/HGNAS/PTA00600/',REP-@YES,COMPRESS-@YES

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.## DEFLIBL ##./
    DEFLIBL TOKELIB/TOKFLIB
/. ./
STA0070B:

    ?STEP :=   'STA0070B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZAMZAIF,TOFILE-ZAMZAIF.TOKFLIB
    CALL      PGM-STA0070B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

STA0060B:

    ?STEP :=   'STA0060B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZTANADT2,TOFILE-ZTANADT2.TOKFLIB
    OVRF      FILE-OUTFILE,TOFILE-ZTANAWK.TOKFLIB
    CALL      PGM-STA0060B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

STA0080B:

    ?STEP :=   'STA0080B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZTANADT,TOFILE-ZTANAWK.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-STA0080B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

STA0090B:

    ?STEP :=   'STA0090B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZAMZAIF,TOFILE-ZAMZAIF.TOKFLIB
    CALL      PGM-STA0090B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

PTA01000:

    DLTOVRF   FILE-*

    CALL      PTA01000.TOKELIB

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