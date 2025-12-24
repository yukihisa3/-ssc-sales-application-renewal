/. ***********************************************************  ./
/. *     サカタのタネ　営業第２部　ＮＡＶＳ　　　　          *  ./
/. *   SYSTEM-NAME :    ＨＨＴ_卸機能　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PTN01200                             *  ./
/. *   JOB-NAME    :    _卸予定データ突合＆原票ＮＯ採番　   *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PTN01200'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*2,VALUE-'  '          /.倉庫ＣＤ  ./
    VAR ?P2       ,STRING*2,VALUE-'  '          /.倉庫(代表)./
    VAR ?P3       ,STRING*1,VALUE-' '           /.出力区分./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(開始)./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(終了)./
    VAR ?P6       ,STRING*1,VALUE-'1'           /.帳票巣別./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.      2    ./
    VAR ?OPR3     ,STRING*50                    /.      3    ./
    VAR ?OPR4     ,STRING*50                    /.      4    ./
    VAR ?OPR5     ,STRING*50                    /.      5    ./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNM1   ,STRING*6,VALUE-'SKTNSF'
    VAR ?FILNM2   ,STRING*6,VALUE-'SKTANA'
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKDTLIB'
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?SKTNSFF  ,NAME!MOD
    VAR ?SKTNSFFN ,STRING*17
    VAR ?SKTANAF  ,NAME!MOD
    VAR ?SKTANAFN ,STRING*17
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '_卸予定データ突合＆原票ＮＯ採番'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKMDLIB/ZAIEXCEL/TOKFLIB/TOKKLIB
           /TOKDTLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P1,?P2)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##処理確認画面##./
PPAUSE:

    ?STEP :=   'PPAUSE  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＜_卸予定ＤＴ突合＆採番＞＃＃'
    ?OPR2  :=  '　_卸予定データ突合＆原票ＮＯの採番を'
    ?OPR3  :=  '　行ないます。'
    ?OPR4  :=  '　確認後、実行して下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##前回分バックアップ→初期化##./
PBAKCLRF:

    ?STEP :=   'PBAKCLRF'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-KKTANAF.TOKDTLIB,TOFILE-KKTANAB.TOKDTLIB,
          ADD-@NO,ACTCHK-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '前回分ＢＫ（確定）'
              GOTO ABEND END

    CLRFILE FILE-KKTANAF.TOKDTLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '確定ＤＴ初期化'
              GOTO ABEND END

    SETPF FILE-MMTANAF.TOKDTLIB,TOFILE-MMTANAB.TOKDTLIB,
          ADD-@NO,ACTCHK-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '前回分ＢＫ（未突合せ）'
              GOTO ABEND END

    CLRFILE FILE-MMTANAF.TOKDTLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '未突合せＤＴ初期化'
              GOTO ABEND END

/.##_卸予定→_卸確定ＤＴ作成##./
STN0120B:

    ?STEP :=   'STN0120B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-STN0120B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸予定→_卸確定ＤＴ作成'
              GOTO ABEND END

/.##_卸結果→_卸確定更新##./
STN0170B:

    ?STEP :=   'STN0170B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-STN0170B.TOKSOLIB,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸結果→_卸確定更新'
              GOTO ABEND END

/.##未突合_卸結果ＤＴ確定更新./
STN0180B:

    ?STEP :=   'STN0180B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-STN0180B.TOKSOLIB,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸結果→_卸確定更新'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '＜_卸予定ＤＴ突合＆採番＞'
    ?KEKA2 :=  '_卸予定ＤＴ突合＆採番が正常終了'
    ?KEKA3 :=  'しました。作成結果の確認をお願い致し'
    ?KEKA4 :=  'ます。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '＜_卸予定ＤＴ突合＆採番＞'
    ?KEKA2 :=  '処理が異常終了しました。ＬＯＧを採取し'
    ?KEKA3 :=  'ＮＡＶへ連絡して下さい'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
