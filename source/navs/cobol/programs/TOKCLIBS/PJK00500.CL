/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PJK00500                             *  ./
/. *   JOB-NAME    :    欠品リスト発行                       *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'JHSDEN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?JHSDENF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JHSDENFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PJK00500'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*2,VALUE-'00'          /.自倉庫ｺｰﾄﾞ./
    VAR ?P2       ,STRING*2,VALUE-'00'          /.代表倉庫ｺｰﾄﾞ./
    VAR ?KBN      ,STRING*1,VALUE-'0'           /.出力区分./
    VAR ?NOUDT1   ,STRING*8,VALUE-'00000000'    /.開始納品日./
    VAR ?NOUDT2   ,STRING*8,VALUE-'99999999'    /.終了納品日./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '欠品リスト発行（受注自動欠品）'
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKFLIB/TOKDLIB/TOKWLIB

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P1,?P2)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '倉庫ｺｰﾄﾞ取得'
              GOTO ABEND
    END


    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##欠品ファイル初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   KPNDATF.TOKDLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '欠品ファイル初期化'
              GOTO ABEND
    END

/.##欠品リスト発行指示##./
SJK0050I:

    ?STEP :=   'SJK0050I '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-KPNDATL1,TOFILE-KPNDATL1.TOKDLIB
    OVRF      FILE-KPNDATL2,TOFILE-KPNDATL2.TOKDLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL      PGM-SJK0050I.TOKELIBO,PARA-(?P1,?P2,?NOUDT1,?NOUDT2,
                                        ?KBN)
    IF        @PGMEC    ^=   0    THEN
              ?PGMEC := @PGMEC
              IF    ?PGMEC  =  4010  THEN
                    ?MSGX := '##取消終了##'
                    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                    RETURN PGMEC-@PGMEC
              ELSE
                    ?KEKA4 :=  '欠品リスト発行指示'
                    GOTO   ABEND
              END
    END
    IF        ?KBN       =  '1'   THEN
              GOTO SJK0060L
    ELSE
              IF        ?KBN       =  '2'   THEN
                         GOTO SJK0070L
              ELSE
                         GOTO PCLRFIL2
              END
    END
/.##欠品リスト（伝票単位出力）##./
SJK0060L:

    ?STEP :=   'SJK0060L '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KPNDATL1,TOFILE-KPNDATL1.TOKDLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SJK0060L.TOKELIBO,PARA-(?NOUDT1,?NOUDT2)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '欠品リスト（伝票単位出力）'
              GOTO ABEND
    END
    GOTO       RTN
/.##欠品リスト（商品単位出力）##./
SJK0070L:

    ?STEP :=   'SJK0070L '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KPNDATL2,TOFILE-KPNDATL2.TOKDLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SJK0070L.TOKELIBO,PARA-(?NOUDT1,?NOUDT2)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '欠品リスト（商品単位出力）'
              GOTO ABEND
    END
    GOTO       RTN

/.##欠品出力用ファイルを初期化##./
PCLRFIL2:

    ?STEP :=   'PCLRFIL2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-CSVKEPF.TOKDLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '欠品出力用ファイル初期化'
              GOTO ABEND
    END

/.##欠品ＣＳＶデータ作成##./
SJK0080B:

    ?STEP :=   'SJK0080B '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SJK0080B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '欠品ＣＳＶデータ作成'
              GOTO ABEND
    END

/.##欠品ＣＳＶデータを作成##./
SJK0090V:

    ?STEP :=   'SJK0090V '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KEPCSVL1,TOFILE-KEPCSVL1.TOKDLIB
    OVRF      FILE-CSVKEPWK,TOFILE-CSVKEPWK.TOKDLIB
    CALL      PGM-SJK0090V.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '欠品ＣＳＶデータ作成'
              GOTO ABEND
    END

/.##欠品ＣＳＶデータ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CSVKEPWK.TOKDLIB,PARA-JIDOKEPP,UNIT-3
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '欠品ＣＳＶデータ転送'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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