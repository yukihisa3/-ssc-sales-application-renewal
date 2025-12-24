/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＥＤＩＣオンライン構築　　　　　　　 *  ./
/. *   JOB-ID      :    PED00400                             *  ./
/. *   JOB-NAME    :    ＥＤＩＣ不照合リスト発行               *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PED00400'
    VAR ?STEP     ,STRING*8
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

    VAR ?OUTPTN   ,STRING*1,VALUE-' '           /.出力パターン./
    VAR ?TORICD   ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR ?JYUDT    ,STRING*8,VALUE-'00000000'    /.受信日./
    VAR ?JYUTM    ,STRING*4,VALUE-'0000'        /.受信時刻./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ＥＤＩＣ　不照合リスト発行'

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKDLIB/TOKWLIB

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ＥＤＩＣ不照合ワーク初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-EWFUSHF.TOKWLIB
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＥＤＩＣ不照合ワーク初期化'
              GOTO ABEND
    END

/.##ＥＤＩＣ不照合リスト発行指示##./
SED0040I:

    ?STEP :=   'SED0040I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL  PGM-SED0040I.TOKELIBO,PARA-(?TORICD,?JYUDT,?JYUTM)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 := 'ＥＤＩＣ不照合リスト発行指示'
              GOTO ABEND
         END
    END


    ?MSGX :=  '取引先　＝'  && ?TORICD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '受信日付＝'  && ?JYUDT
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '受信時刻＝'  && ?JYUTM
    SNDMSG    ?MSGX,TO-XCTL

/.##ＥＤＩＣ不照合データ抽出##./
SED0040W:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.##指定された取引先ＣＤにより、請求合計Ｆ変更##./
    /.##ビーバートザン##./
    IF       ?TORICD  =  '00006002'   THEN
             OVRF FILE-SETGKF,TOFILE-SETGKFU3.TOKKLIB
    END
    /.##マキバ        ##./
    IF       ?TORICD  =  '00000275'   THEN
             OVRF FILE-SETGKF,TOFILE-SETGKBB1.TOKKLIB
    END
    /.##片倉工業      ##./
    IF       ?TORICD  =  '00120020'   THEN
             OVRF FILE-SETGKF,TOFILE-SETGKBB1.TOKKLIB
    END

    OVRF FILE-SETGKF,TOFILE-SETGKFU3.TOKKLIB
    CALL     PGM-SED0040W.TOKELIBO,PARA-(?TORICD,?JYUDT,?JYUTM)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＥＤＩＣ不照合データ抽出'
              GOTO ABEND
    END

/.##ＥＤＩＣ不照合リスト発行##./
SED0040L:

    ?STEP :=   'SED0040L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SED0040L.TOKELIBO
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＥＤＩＣ不照合リスト発行'
              GOTO ABEND
    ELSE
              GOTO RTN
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMECX   :=    %STRING(?PGMEC)

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0040I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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