/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    自動受信　　　　　　　　　　　       *  ./
/. *   JOB-NAME    :    実行履歴リスト発行　　　　　　　　　 *  ./
/. *   JOB-ID      :    PCM00100                             *  ./
/. *   DATE        :    2022/05/16                           *  ./
/. ***********************************************************  ./
    PGM

/.--------------------------------------------------------------./
    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PCM00100'
    VAR  ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR  ?BUMON   ,STRING*4,VALUE-'    '        /.部門./
    VAR  ?TANCD8  ,STRING*8,VALUE-'        '    /.部門+担当者./
    VAR  ?TANCD   ,STRING*2,VALUE-'  '          /.担当者     ./
/.  VAR  ?SOKCD   ,STRING*2,VALUE-'00' ./       /.実行倉庫   ./
/.  VAR  ?DSOKCD  ,STRING*2,VALUE-'00' ./       /.代表倉庫   ./
/.---------------------------------------------------------------./
    VAR  ?KDATEF  ,STRING*8,VALUE-'00000000'    /.起動日（開始） ./
    VAR  ?KDATET  ,STRING*8,VALUE-'00000000'    /.起動日（終了） ./
    VAR  ?KTIMEF  ,STRING*6,VALUE-'000000'      /.起動時（開始） ./
    VAR  ?KTIMET  ,STRING*6,VALUE-'000000'      /.起動時（終了） ./
    VAR  ?WSIDF   ,STRING*8,VALUE-'        '    /.WSID  （開始） ./
    VAR  ?WSIDT   ,STRING*8,VALUE-'        '    /.WSID  （終了） ./
    VAR  ?TANF    ,STRING*2,VALUE-'  '          /.担当者（開始） ./
    VAR  ?TANT    ,STRING*2,VALUE-'  '          /.担当者（終了） ./
    VAR  ?SYORIF  ,STRING*10,VALUE-'0000000000' /.処理_（開始） ./
    VAR  ?SYORIT  ,STRING*10,VALUE-'0000000000' /.処理_（終了） ./
    VAR  ?OUTKBN  ,STRING*1,VALUE-' '           /.出力区分　     ./
/.---------------------------------------------------------------./
    VAR  ?OPR1    ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?OPR2    ,STRING*50                 /.      2    ./
    VAR  ?OPR3    ,STRING*50                 /.      3    ./
    VAR  ?OPR4    ,STRING*50                 /.      4    ./
    VAR  ?OPR5    ,STRING*50                 /.      5    ./
/.--------------------------------------------------------------./
    VAR  ?MSG1    ,STRING*80                 /.開始終了MSG./
    VAR  ?PGNM    ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1   ,STRING*40                 /.      2    ./
    VAR  ?KEKA2   ,STRING*40                 /.      3    ./
    VAR  ?KEKA3   ,STRING*40                 /.      4    ./
    VAR  ?KEKA4   ,STRING*40                 /.      5    ./
/.---------------------------------------------------------------./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB/
              TOKSOLIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM := '実行履歴リスト発行'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
GETWKSTN:
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

/.##実行履歴リスト発行指示##./
DCM0010I:

    ?STEP :=  'DCM0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      DCM0010I.TOKSOLIB,
              PARA-(?KDATEF,?KDATET,
                    ?KTIMEF,?KTIMET,
                    ?WSIDF,?WSIDT,
                    ?TANF,?TANT,
                    ?SYORIF,?SYORIT,
                    ?OUTKBN)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC =  4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN    PGMEC-?PGMEC
    END
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '実行履歴リスト発行指示'
              GOTO      ABEND
    END

    ?MSGX :=  '## 起動日付（開始） = ' && ?KDATEF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##　　　　（終了） = ' && ?KDATET
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 起動時刻（開始） = ' && ?KTIMEF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##　　　　（終了） = ' && ?KTIMET
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## ＷＳ番号（開始） = ' && ?WSIDF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##　　　　（終了） = ' && ?WSIDT
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 担当者　（開始） = ' && ?TANF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##　　　　（終了） = ' && ?TANT
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 処理ＮＯ（開始） = ' && ?SYORIF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##　　　　（終了） = ' && ?SYORIT
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 出力区分　　　　 = ' && ?OUTKBN
    SNDMSG    ?MSGX,TO-XCTL

/.##リスト出力先制御##./
CHGSPLQ:

    ?STEP :=  'CHGSPLQ '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE      ?OUTKBN   OF
    /.レーザー./
      /.実機./
      /. #' '#  OVRPRTF FILE-XU04LP,TOFILE-KAHMALBP.XUCL ./
      /.社内./
         #' '#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL

    /.連帳./                           /.↓現地はXU04LPでOK？./
       #'1'#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    END

/.##実行履歴リスト発行##./
DCM0020L:

    ?STEP  :=  'SDE0090L'
    ?MSGX  :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL      PGM-DCM0020L.TOKSOLIB,
              PARA-(?KDATEF,?KDATET,
                    ?KTIMEF,?KTIMET,
                    ?WSIDF,?WSIDT,
                    ?TANF,?TANT,
                    ?SYORIF,?SYORIT)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '実行履歴リスト'
              GOTO ABEND
    END

/.--------------------------------------------------------------./

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:

    ?KEKA1 :=  '実行履歴リスト'
    ?KEKA2 :=  'リスト発行が異常終了しました。'
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