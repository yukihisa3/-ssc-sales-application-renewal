/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    消費税軽減税率対応　　　　　　       *  ./
/. *   JOB-NAME    :    消費税率取得サブ　　　　　　　　　   *  ./
/. *   JOB-ID      :    PKYTAXPG                             *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM  (P1-?ZEIKBN,P2-?ZEIDATE)

/.##INパラメタ##./
    PARA  ?ZEIKBN  ,STRING*1,IN,VALUE-' '
    PARA  ?ZEIDATE ,STRING*8,IN,VALUE-'00000000'
/.--------------------------------------------------------------./
    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PKYTAXPG'
    VAR  ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR  ?ZEIRITU ,STRING*5,VALUE-'     '    /.税率./
    VAR  ?DZEIRITU,STRING*5,VALUE-'     '    /.代表税率./
    VAR  ?ERRKBN  ,STRING*1,VALUE-' '        /.エラー区分./
/.--------------------------------------------------------------./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKSOLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##消費税率取得サブ##./
SKYTAXPG:

    ?STEP :=  'SKYTAXPG'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL SKYTAXPG.TOKSOLIB,PARA-(?ZEIKBN,?ZEIDATE,?ERRKBN,
                                 ?ZEIRITU,?DZEIRITU)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              GOTO   ABEND
    END
    ?MSGX :=  '税区分     = ' && ?ZEIKBN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '日付　     = ' && ?ZEIDATE
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  'エラー     = ' && ?ERRKBN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '消費税     = ' && ?ZEIRITU
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    ?MSGX :=  '代表消費税 = ' && ?DZEIRITU
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

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