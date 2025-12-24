/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳオンラインシステム　　　　 *  ./
/. *   JOB-ID      :    PBMC0500                             *  ./
/. *   JOB-NAME    :    流通BMS返品リスト発行（カインズ）    *  ./
/. *               :    2014/05/21               VER3        *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PBMC0500'
    VAR       ?STEP     ,STRING*8
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

    VAR       ?OUTPTN   ,STRING*1,VALUE-' '   /.出力パターン./
    VAR       ?TORICD   ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR       ?DTPTN    ,STRING*1,VALUE-' '   /.日付パターン./
    VAR       ?DATEF    ,STRING*8,VALUE-'00000000'    /.開始日付./
    VAR       ?DATET    ,STRING*8,VALUE-'00000000'    /.終了日付./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '流通ＢＭＳ　返品リスト発行（カインズ）'

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##流通ＢＭＳ返品リスト発行指示##./
SBM0160I:

    ?STEP :=   'SBM0160I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   BMSFLIB/TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
  /.OVRF     FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB  ./
    CALL  PGM-SBM0160I.TOKELIBO,PARA-(?OUTPTN,?TORICD,?DTPTN,
                                      ?DATEF,?DATET)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 := '流通ＢＭＳ返品リスト発行指示（カインズ）'
              GOTO ABEND
         END
    END

  /.SNDMSG  ?OUTPTN,TO-XCTL
    SNDMSG  ?TORICD,TO-XCTL
    SNDMSG  ?DTPTN,TO-XCTL
    SNDMSG  ?DATEF,TO-XCTL
    SNDMSG  ?DATET,TO-XCTL  ./

    ?MSGX :=  '出力種類＝'  && '？？？'
    IF      ?OUTPTN = '1'  THEN
            ?MSGX :=  '出力種類＝'  && '帳票'
    END
    IF      ?OUTPTN = '2'  THEN
            ?MSGX :=  '出力種類＝'  && 'ＣＳＶ'
    END
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '取引先　＝'  && ?TORICD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '日付種類＝'  && '？？？'
    IF      ?DTPTN  = '1'  THEN
            ?MSGX :=  '日付種類＝'  && '受信日'
    END
    IF      ?DTPTN  = '2'  THEN
            ?MSGX :=  '日付種類＝'  && '計上日'
    END
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '開始日付＝'  && ?DATEF
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '終了日付＝'  && ?DATET
    SNDMSG    ?MSGX,TO-XCTL


/.##流通BMS返品データ抽出（カインズ）##./
SBMC050B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF     FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF     FILE-BMSHEPL2,TOFILE-BMSHEPL2.BMSFLIB
    OVRF     FILE-BMSHEPL3,TOFILE-BMSHEPL3.BMSFLIB
    OVRF     FILE-BMSHE3W,TOFILE-BMSHE3W.BMSFLIB  ./
    CALL     PGM-SBMC050B.TOKELIBO,PARA-(?TORICD,?DTPTN,
                                         ?DATEF,?DATET)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '流通ＢＭＳ返品データ抽出（カインズ）'
              GOTO ABEND
    END

    IF        ?OUTPTN   ^=   '1'     THEN
              GOTO SBMC070V
    END

/.##流通BMS返品リスト出力（カインズ）##./
SBMC060L:

    ?STEP :=   'SBMC060L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-BMSHE3W1,TOFILE-BMSHE3W1.BMSFLIB ./
    CALL      PGM-SBMC060L.TOKELIBO,PARA-(?TORICD,?DTPTN,
                                         ?DATEF,?DATET)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '流通ＢＭＳ返品リスト出力（カインズ）'
              GOTO ABEND
    ELSE
              GOTO RTN
    END

/.##流通BMS返品ＣＳＶデータ出力（カインズ）##./
SBMC070V:

    ?STEP :=   'SBMC070V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
  /.OVRF      FILE-BMSHEPW1,TOFILE-BMSHE3W1.BMSFLIB
    OVRF      FILE-BMSHEPC,TOFILE-BMSHE3C.BMSFLIB  ./
    CALL      PGM-SBMC070V.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '流通ＢＭＳ返品ＣＳＶデータ出力（カインズ）'
              GOTO ABEND
    END

/.##ＣＳＶデータ多階層へコンバート（カインズ）##./
CNVDF000:

    ?STEP :=   'CNVDF000'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVDF FILE-BMSHE3C.BMSFLIB,
/.  PATH-'/SAKATA/DATA/PRKANRI/BMSHE3C.CSV',MODE-@EXP,ADD-@NO,
    ACTCHK-@NO    ./
    PATH-'/NAVPC/BMSHE3C.CSV',MODE-@EXP,ADD-@NO,
    ACTCHK-@NO,HEADLINE-@YES,EXTCNV-@YES
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    := '返品ＣＳＶデータ転送（カインズ）'
              GOTO ABEND
    ELSE
              GOTO RTN
    END
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
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