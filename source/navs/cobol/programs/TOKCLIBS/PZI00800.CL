/. ***********************************************************  ./
/. *   （株）サカタのタネ                                    *  ./
/. *   SYSTEM-NAME :    在庫ＥＸＣＥＬ連携　　　　　　       *  ./
/. *   JOB-ID      :    PZI00800                             *  ./
/. *   JOB-NAME    :    在庫EXCEL更新結果一覧　　　　　      *  ./
/. ***********************************************************  ./
    PGM

/.--------------------------------------------------------------./
    VAR   ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR   ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR   ?PGMEC   ,INTEGER
    VAR   ?PGMES   ,STRING*5,VALUE-'     '
    VAR   ?PGMECX  ,STRING*11
    VAR   ?PGMEM   ,STRING*99
    VAR   ?MSG     ,STRING*99(6)
    VAR   ?MSGX    ,STRING*99
    VAR   ?PGMID   ,STRING*8,VALUE-'PZI00800'
    VAR   ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR   ?BUMON   ,STRING*4,VALUE-'    '     /.部門./
    VAR   ?TANCD8  ,STRING*8,VALUE-'        ' /.部門+担当者./
    VAR   ?TANCD   ,STRING*2,VALUE-'  '       /.担当者     ./
    VAR   ?DATEF   ,STRING*8,VALUE-'00000000' /.日付FROM   ./
    VAR   ?DATET   ,STRING*8,VALUE-'00000000' /.日付TO     ./
    VAR   ?SKBN1   ,STRING*1,VALUE-' '        /.作業区分1  ./
    VAR   ?SKBN2   ,STRING*1,VALUE-' '        /.作業区分2  ./
    VAR   ?SKBN3   ,STRING*1,VALUE-' '        /.作業区分3  ./
    VAR   ?SKBN4   ,STRING*1,VALUE-' '        /.作業区分4  ./
    VAR   ?SKBN5   ,STRING*1,VALUE-' '        /.作業区分5  ./
    VAR   ?SKBN6   ,STRING*1,VALUE-' '        /.作業区分6  ./
    VAR   ?SOKCD   ,STRING*2,VALUE-'00'       /.実行倉庫   ./
    VAR   ?DSOKCD  ,STRING*2,VALUE-'00'       /.代表倉庫   ./
/.--------------------------------------------------------------./
    VAR   ?OPR1    ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?OPR2    ,STRING*50                 /.      2    ./
    VAR   ?OPR3    ,STRING*50                 /.      3    ./
    VAR   ?OPR4    ,STRING*50                 /.      4    ./
    VAR   ?OPR5    ,STRING*50                 /.      5    ./
/.--------------------------------------------------------------./
    VAR   ?MSG1    ,STRING*80                 /.開始終了MSG./
    VAR   ?PGNM    ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?KEKA1   ,STRING*40                 /.      2    ./
    VAR   ?KEKA2   ,STRING*40                 /.      3    ./
    VAR   ?KEKA3   ,STRING*40                 /.      4    ./
    VAR   ?KEKA4   ,STRING*40                 /.      5    ./
/.-----------------------------------------------------------./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB/ZAIEXCEL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM := '在庫ＥＸＣＥＬ更新結果一覧'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=  'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              GOTO   ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO   ABEND
    END

/.##出力指示##./
SZI0080I:

    ?STEP :=  'SZI0080I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIBO

    CALL      PGM-SZI0080I.TOKELIBO,
              PARA-(?BUMON,?TANCD,?DATEF,?DATET,
                    ?SKBN1,?SKBN2,?SKBN3,?SKBN4,?SKBN5,?SKBN6)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC  = 4010 THEN
              SNDMSG '取消終了',TO-XCTL.@ORGPROF,
                     JLOG-@YES,SLOG-@YES
              RETURN PGMEC-?PGMEC
    END
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '出力指示　　　'
              GOTO   ABEND
    END

    SNDMSG    '指定値',TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　部門コード　　＝' && ?BUMON
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　担当者コード　＝' && ?TANCD
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　更新日（開始）＝' && ?DATEF
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　更新日（終了）＝' && ?DATET
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　作業区分１　　＝' && ?SKBN1
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　作業区分２　　＝' && ?SKBN2
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　作業区分３　　＝' && ?SKBN3
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　作業区分４　　＝' && ?SKBN4
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　作業区分５　　＝' && ?SKBN5
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '　作業区分６　　＝' && ?SKBN6
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##在庫ＥＸＣＥＬ更新結果一覧##./
SZI0090L:

    ?STEP :=  'SZI0090L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL      PGM-SZI0090L.TOKELIBO,
              PARA-(?BUMON,?TANCD,?DATEF,?DATET,
                    ?SKBN1,?SKBN2,?SKBN3,?SKBN4,?SKBN5,?SKBN6)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '更新結果一覧'
              GOTO   ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:


    ?KEKA1 :=  '在庫ＥＸＣＥＬ更新結果一覧が'
    ?KEKA2 :=  '異常終了しました。'
    ?KEKA3 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  := ?MSG(?I)
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
