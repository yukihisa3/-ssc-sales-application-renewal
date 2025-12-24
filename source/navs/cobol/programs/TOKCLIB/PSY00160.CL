/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（ケーヨー新システム）    *  ./
/. *   SYSTEM-NAME :    配送発注集計表 （西尾分）            *  ./
/. *   JOB-ID      :    PSY00160                             *  ./
/. *   JOB-NAME    :    変換～コード変換～ルート取得～       *  ./
/. *               :    ＳＯＲＴ１～合計計算～ＳＯＲＴ２～   *  ./
/. *                    配送発注集計表                       *./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY00160'
    VAR       ?STEP     ,STRING*8
    VAR       ?BATNO    ,STRING*20,VALUE-'00000000000000000000'
    VAR       ?SOKOCD   ,STRING*4,VALUE-'0000'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB

/.###配送発注集計表出力###./
PSY00160:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB

    CALL      PGM-SSY0016I.TOKELIB,PARA-(?BATNO,?SOKOCD)
    ?PGMEC := @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              IF ?PGMEC  =   4001 THEN
                  SNDMSG MSG-'＃取消終了＃',TO-XCTL.@ORGPROF
                  GOTO   RTN
              ELSE
                  GOTO   ABEND
              END
    END

/.出荷場所，ルート，ルート順番をセット                          ./
SSY0017B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'***   ﾙｰﾄｼﾞｮｳﾎｳ ｾｯﾄ   ***',TO-XCTL


    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SHWHISF,TOFILE-SHWHISF.TOKFLIB
    OVRF      FILE-TENRMS1,TOFILE-TENRMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY0017B.TOKELIB,PARA-(?BATNO,?SOKOCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.ＳＯＲＴ(納品日，出荷場所，商品）                             ./
SORT1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWHISF.TOKFLIB,INRL-256,INBF-1,
              OUTFILE-SHWHISF.TOKFLIB,OUTBF-1,
              KEY-54!5!CA,
              KEY1-38!2!CA,
              KEY2-199!13!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.商品毎数量集計                                                ./
SSY0018B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'***   ｼｮｳﾋﾝｺﾞｳｹｲ      ***',TO-XCTL

    OVRF      FILE-SHWHAIF,TOFILE-SHWHISF.TOKFLIB
    OVRF      FILE-SHWHISGF,TOFILE-SHWHISGF.TOKFLIB
    CALL      PGM-SSY0018B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.合計ファイル                                            ./
CNVFILE:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'ワークファイル_配送伝票データへコピー',TO-XCTL

    CNVFILE   FILE-SHWHISGF.TOKFLIB,TOFILE-SHWHISF.TOKFLIB,
              ADD-@YES,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    CLRFILE   FILE-SHWHISGF.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.ＳＯＲＴ(納品日，出荷場所，ルート，商品，ルート順）           ./
SORT2:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWHISF.TOKFLIB,INRL-256,INBF-1,
              OUTFILE-SHWHISF.TOKFLIB,OUTBF-1,
              KEY-38!2!CA,
              KEY1-54!5!CA,
              KEY2-253!2!DA,
              KEY3-199!13!CA,
              KEY4-255!2!DA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.配送発注集計表出力                                            ./
SSY0019L:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'*** ﾊｲｿｳﾊｯﾁｭｳｼｭｳｹｲﾋｮｳ ***',TO-XCTL

    OVRF      FILE-INFILE,TOFILE-SHWHISF.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TENRMS2,TOFILE-TENRMS2.TOKFLIB
    OVRF      FILE-HTOKMS,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY0019L.TOKELIB
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