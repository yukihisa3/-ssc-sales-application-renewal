/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭサンワオンラインシステム　　　 *  ./
/. *   JOB-ID      :    PSY75200                             *  ./
/. *   JOB-NAME    :    ＤＣＭサンワ支払明細書発行           *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PSY75200'
    VAR ?STEP     ,STRING*8
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

    VAR ?OUTPTN   ,STRING*1,VALUE-'0'           /.出力区分./
    VAR ?TORICD   ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR ?NENGETU  ,STRING*6,VALUE-'000000'      /.支払締年月./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ＤＣＭサンワ　支払明細書発行'

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKDLIB/TOKKLIB/TOKWLIB

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##支払明細書発行指示##./
SSY7520I:

    ?STEP :=   'SSY7520I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL  PGM-SSY7520I.TOKELIBO,PARA-(?OUTPTN,?TORICD,?NENGETU)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 := '支払明細書発行指示'
              GOTO ABEND
         END
    END


    ?MSGX :=  '出力種類＝'  && '？？？'
    IF      ?OUTPTN = '1'  THEN
            ?MSGX :=  '出力種類＝'  && '帳票'
    END
    IF      ?OUTPTN = '2'  THEN
            ?MSGX :=  '出力種類＝'  && 'ＣＳＶ'
    END
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '取引先　　＝'  && ?TORICD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '支払締年月＝'  && ?NENGETU
    SNDMSG    ?MSGX,TO-XCTL

/.##出力区分が２の場合、CSV出力へ##./
    ?MSGX :=  'OUTPTN = '  && ?OUTPTN
    SNDMSG    ?MSGX,TO-XCTL
    IF        ?OUTPTN   =   '2'     THEN
              GOTO SSY7520V
    END

/.##支払明細書発行##./
SSY7520L:

    ?STEP :=   'SSY7520L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRPRTF FILE-PRTF,TOFILE-KAHMAPRT.XUCL,SEP-@NO
    CALL      PGM-SSY7520L.TOKELIBO,PARA-(?TORICD,?NENGETU)

    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '受領書発行'
              GOTO ABEND
    ELSE
              GOTO RTN
    END

/.##支払明細書ＣＳＶデータ出力##./
SSY7520V:

    ?STEP :=   'SSY7520V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SSY7520V.TOKELIBO,PARA-(?TORICD,?NENGETU)
    ?PGMEC := @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '受領書ＣＳＶデータ出力'
              GOTO ABEND
    END

/.##ＣＳＶデータ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-SWSIHAWK.TOKDLIB,PARA-DCMSNW,UNIT-2
    ?PGMEC := @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＣＳＶデータ転送'
              GOTO ABEND
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