/. ***********************************************************  ./
/. *     サカタのタネ　ＨＧ部基幹システム　　　　　          *  ./
/. *   SYSTEM-NAME :    ＡＣＯＳ計上処理　　　　　　　　     *  ./
/. *   JOB-ID      :    ACOSUP1                              *  ./
/. *   JOB-NAME    :    ＨＧ部ＤＴ→ＡＣＯＳ計上ＤＴ編集     *  ./
/. *   UPDATE      :    2018/01/30 NAV TAKAHASHI             *  ./
/. ***********************************************************  ./
    PGM
/.##ﾜｰｸｱﾘｱ##./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'ACOSUP1 '
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKMDLIB/TOKSOLIB/TOKCOLIB

/.# 売上／仕入在庫データバックアップ（別ＰＧ）                   #./
STEP1:

    ?MSGX     := '## STEP1 売上／仕入在庫ﾃﾞｰﾀﾊﾞｯｸｱｯﾌﾟ'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL CACOSBK1.TOKCOLIB

/.# 日次売上／仕入在庫データバックアップ　　　　                 #./
STEP2:

    ?MSGX     := '## STEP2-1 売上計上ﾃﾞｰﾀﾊﾞｯｸｱｯﾌﾟ'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKUWK.TOKWLIB,TOFILE-TOKUHOU.TOKBLIB,ADD-@NO,
            BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP2-1 ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    ?MSGX     := '## STEP2-2 仕入在庫計上ﾃﾞｰﾀﾊﾞｯｸｱｯﾌﾟ'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKUZ.TOKFLIB,TOFILE-TOKUHOS.TOKBLIB,ADD-@NO,
            BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP2-2 ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.# 新情報システム部売上／仕入在庫別　件数リスト                 #./
STEP3:

    ?MSGX     := '## STEP3 売上／仕入在庫別件数ﾘｽﾄ'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-TOKUWK,TOFILE-TOKUWK.TOKWLIB
    OVRF      FILE-TOKUZ,TOFILE-TOKUZ.TOKFLIB
    CALL      PGM-SKY2302L.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP3   ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.# 計上編集データ統合　　　　　　　　　　　　                   #./
STEP4:

    ?MSGX     := '## STEP4-1 (売上)計上編集DT=>統合'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKUWK.TOKWLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP4-1 ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    ?MSGX     := '## STEP4-2 (仕入在庫)計上編集DT=>統合'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKUZ.TOKFLIB,TOFILE-ACOSWK.TOKWLIB,ADD-@YES,
            BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP4-2 ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.# 計上編集（統合）データバックアップ                           #./
STEP5:

    ?MSGX     := '## STEP5 計上編集（統合）ﾃﾞｰﾀﾊﾞｸｱｯﾌﾟ'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-ACOSWK.TOKWLIB,TOFILE-ACOSWK.TOKBLIB,ADD-@NO,
            BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP5   ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.# 売上／仕入在庫計上データ初期化　　                           #./
STEP6:

    ?MSGX     := '## STEP6-1 売上計上データ初期化'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-TOKUWK.TOKWLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP6-1 ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    ?MSGX     := '## STEP6-2 仕入在庫計上データ初期化'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-TOKUZ.TOKFLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP6-2 ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    CLRFILE FILE-TOKUJWK.TOKWLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP6-3 ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    CLRFILE FILE-TOKUJZ.TOKKLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP6-4 ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.# ＡＣＯＳ計上Ｆ　ＳＯＲＴ                                     #./
STEP7:

    ?MSGX     := '## STEP7   計上編集ﾌｧｲﾙSORT'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    SORT    INFILE-ACOSWK.TOKWLIB,INRL-256,INBF-1,
            OUTFILE-ACOSWK.TOKWLIB,OUTBF-1,
            KEY-1!26!CA,
            RCDL-@DSP
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP7   ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.# ＡＣＯＳ計上Ｆ　ダブリチェック                               #./
STEP8:

    ?MSGX     := '## STEP8   計上編集ﾌｧｲﾙﾀﾞﾌﾞﾘﾁｪｯｸ'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-ACOS,TOFILE-ACOSWK.TOKWLIB
    OVRF      FILE-ACOSOK,TOFILE-ACOSFILE.TOKWLIB
    OVRF      FILE-ACOSERR,TOFILE-ACOSERR.TOKWLIB
    CALL      PGM-SKY3102L.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP8   ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.# ＡＣＯＳ計上Ｆ　件数リスト                                   #./
STEP9:

    ?MSGX     := '## STEP9   ACOS計上件数ﾘｽﾄ'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-ACOSFILE,TOFILE-ACOSFILE.TOKWLIB
    CALL      PGM-SKY2902L.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP9   ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.# 統合計上データ退避処理                                       #./
STEP10:

    ?MSGX     := '## STEP10  ACOS統合計上ﾃﾞｰﾀ退避処理'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL CACOSBK2.TOKCOLIB

/.# 部門間在庫移動データ作成用　データセット                     #./
STEP11:

    ?MSGX     := '## STEP11  部門間在庫移動ﾃﾞｰﾀｺﾋﾟｰ'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-ACOSFILE.TOKWLIB,TOFILE-ACOSDATA.TOKDTLIB,
            ADD-@NO,BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC ^=   0    THEN
              ?MSGX     := '## STEP11  ABEND ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

RTN:

    ?MSGX     := '## ACOSUP1 正常終了 ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:

    ?MSGX     := '## ACOSUP1 異常終了 ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
