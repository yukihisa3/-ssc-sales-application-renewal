/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSE00810                             *  ./
/. *   JOB-NAME    :    更新                                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE00810'
    VAR       ?STEP     ,STRING*8
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

/.##伝票ファイル更新##./
/.※伝票Ｆの更新は、請求データ抽出時に行なっている為行なわない。./
/.　旧ＰＧ－ＩＤ＝ＳＳＫＴ３６０（ＳＫＴＳＬＩＢに保存）        ./

/.  SORTF     INFILE-SETGKFA.TOKFLIB,       ##ケーヨー抽出##
              OUTFILE-SETGKFB.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00000173',
              RCDL-@DSP     ./
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SELECT SORT ERR 00000173 ##',TO-XCTL
              GOTO ABEND END

    SORTF     INFILE-SETGKFA.TOKFLIB,     /.##ジョイフル山新抽出##./
              OUTFILE-SETGKFG.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00001041',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SELECT SORT ERR 00001041 ##',TO-XCTL
              GOTO ABEND END

    SORTF     INFILE-SETGKFA.TOKFLIB,     /.##ビーバー登山抽出##./
              OUTFILE-SETGKFK.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00006002',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## SELECT SORT ERR 00006002 ##',TO-XCTL
              GOTO ABEND END

/.##ケーヨー請求データ削除##./
SSE0080B:

    ?STEP :=   'SSE0080B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SEIGKF21,TOFILE-SETGKFB1.TOKFLIB
    CALL      PGM-SSE0080B.TOKELIB
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