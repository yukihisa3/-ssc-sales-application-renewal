/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷サブシステム 　       　　　     *  ./
/. *   JOB-ID      :    PSV00560                             *  ./
/. *   JOB-NAME    :    オンラインＤＴＣＳＶデータ作成　　   *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSV00560'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
/.###PGﾊﾟﾗﾒﾀｴﾘｱ###./
    VAR ?BATNO    ,STRING*20,VALUE-'                    '
    VAR ?SOKOCD   ,STRING*4,VALUE-'    '
    VAR ?HIDUKE   ,STRING*8,VALUE-'        '
    VAR ?JIKAN    ,STRING*4,VALUE-'    '
    VAR ?TOKCD    ,STRING*8,VALUE-'        '
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    DEFLIBL TOKELIB/TOKFLIB

/.###発注集計表（一覧）出力###./
PSV00560:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB

    CALL      PGM-CSV0050I.TOKELIB,PARA-(?BATNO,?SOKOCD,
                                         ?HIDUKE,?JIKAN,?TOKCD)
    ?PGMEC := @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              IF ?PGMEC  =   4001 THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF
                  GOTO   RTN
              ELSE
                  GOTO   ABEND
              END
    END

    ?MSGX :=  '##日付     = ' && ?HIDUKE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##時間     = ' && ?JIKAN
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##取引先CD = ' && ?TOKCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##倉庫ｺｰﾄﾞ = ' && ?SOKOCD
    SNDMSG    ?MSGX,TO-XCTL

/.###発注集計データ抽出###./
PSY00110:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-HACYUPRT,TOFILE-HACYUPRT.TOKKLIB

    CALL      PGM-SSY0011B.TOKELIB,PARA-(?BATNO,?SOKOCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ＣＳＶデータ出力１##./
CSV0010B:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-HACTEN,TOFILE-HACTEN.TOKKLIB
    CALL      PGM-CSV0016B.TOKELIBO,PARA-(?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###商品変換ﾃｰﾌﾞﾙ初期化##./
PCLRFILE:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-CSYOF.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ＣＳＶデータ出力２##./
CSV0026B:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-CSYOL1,TOFILE-CSYOL1.TOKKLIB
    OVRF      FILE-HACYUPRT,TOFILE-HACYUPRT.TOKKLIB
    CALL      PGM-CSV0026B.TOKELIBO,PARA-(?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ＣＳＶデータ出力３##./
CSV0030B:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CSYOL1,TOFILE-CSYOL1.TOKKLIB
    OVRF      FILE-CSYOSF,TOFILE-CSYOSF.TOKKLIB
    CALL      PGM-CSV0036B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.## SORT(量販品コード順)##./
/.## 取引先+納入場所+納品日+頁+出力順+量販商品ＣＤ ##./
SORT1:
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-CSYOSF.TOKKLIB,INRL-320,INBF-12,
              OUTFILE-CSYOSF.TOKKLIB,OUTBF-12,
              KEY-1!8!CA,
              KEY1-9!2!CA,
              KEY2-11!8!CA,
              KEY3-41!5!CA,
              KEY4-40!1!CA,
              KEY5-19!13!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ＣＳＶデータ出力４##./
CSV0040B:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HACTEN,TOFILE-HACTEN.TOKKLIB
    OVRF      FILE-CSYOSF,TOFILE-CSYOSF.TOKKLIB
    OVRF      FILE-CDATA,TOFILE-CDATA.TOKKLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-CSV0046B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###テキスト出力##./
PFIMPORT:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CDATA.TOKKLIB,PARA-CSVDATA,UNIT-2
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

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