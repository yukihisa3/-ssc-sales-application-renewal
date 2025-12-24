# PSY9401E

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PSY9401E.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　　　　　　　　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    ヨドバシ　　　ＥＤＩ　　　　　　　   *  ./
/. *   JOB-ID      :    PSY9401E                             *  ./
/. *   JOB-NAME    :    ヨドバシ出荷通知データ連携　　　　　 *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY9401E'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8,VALUE-'        '    /.STEP-ID./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?JTORI    ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?SOKCD    ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?NDATE    ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?KENSU    ,STRING*7,VALUE-'0000000'     /.作成件数./
    VAR ?SYORINM1 ,STRING*40
    VAR ?SYORINM2 ,STRING*50
    VAR ?SYORINM3 ,STRING*50
    VAR ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?JOB      ,STRING*01,VALUE-'3'        /.3:出荷通知CSV出力./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ヨドバシ出荷通知データ連携'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKSOLIB/TOKFLIB/TOKKLIB/TOKDTLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.## ログインユーザー情報取得 ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログインユーザー情報取得'
              GOTO ABEND
    END
    ?TANCD := ?TANCD8

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##ヨドバシ出荷通知データ作成指示##./
SSY9401I:

    ?STEP :=  'SSY9401I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-SHTDENLJ,TOFILE-SHTDENLJ.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY9401I.TOKSOLIB
             ,PARA-(?JDATE,?JTIME,?JTORI,?SOKCD,?NDATE,?DSOKCD)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '出荷通知データ作成指示'
              GOTO ABEND
         END
    END

    ?MSGX :=  'バッチ（日）＝' && ?JDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'バッチ（時）＝' && ?JTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'バッチ（取）＝' && ?JTORI
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '出力倉庫　　＝' && ?SOKCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '日付　　　　＝' && ?NDATE
    SNDMSG    ?MSGX,TO-XCTL

/.##ヨドバシ出荷通知データ抽出##./
SSY9402B:

    ?STEP :=   'SSY9402B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLJ,TOFILE-SHTDENLJ.TOKKLIB
    OVRF      FILE-YODJOHL2,TOFILE-YODJOHL2.TOKDTLIB
    OVRF      FILE-YODSNDPF,TOFILE-YODSNDPF.TOKDTLIB
    CALL      PGM-SSY9402C.TOKSOLIB,PARA-(?JDATE,?JTIME,?JTORI,?SOKCD,
                                          ?NDATE,?SYORIKN1)

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷通知データ抽出'
              GOTO ABEND
    END

    SNDMSG    ?SYORIKN1,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ヨドバシ出荷通知データ作成##./
SSY9403B:

    ?STEP :=   'SSY9403B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-YODSNDL2,TOFILE-YODSNDL2.TOKDTLIB
    OVRF      FILE-YODSNDSF,TOFILE-YODSNDSF.TOKDTLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSY9403D.TOKSOLIB

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷通知データ作成'
              GOTO ABEND
    END

    SNDMSG    ?SYORIKN1,TO-XCTL.@ORGPROF,JLOG-@YES

/.##出荷通知データ転送##./
FIMPORT1:

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-YODSNDSF.TOKDTLIB,
            TYPE-@FILE,
            PARA-YODOBASI,
            UNIT-3,
            OPR-@NO

    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '出荷通知データ転送'
                    GOTO ABEND
    END

    IF        ?SYORIKN1 = '0000000' THEN
              GOTO      ABEND
    END

/.##結果リスト##./
SSN0050L:

    ?STEP :=   'SSN0050L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIBO/TOKELIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM1 := '【ヨドバシ出荷通知データ作成・引き渡し】'
    ?SYORINM2 := 'ヨドバシ出荷通知データセット終了しました。'
    ?SYORINM3 := '件数１：出荷通知件数'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '結果リスト'
              GOTO ABEND
    END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '確認後、出荷処理を開始して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    =  0        THEN
       IF     ?SYORIKN1 = '0000000' THEN
              ?KEKA1   := '対象データ０件です。'
              ?KEKA2   := '指示条件や売上伝票データ有無を'
              ?KEKA3   := '確認してください。'
       END
    ELSE
              ?KEKA1 :=  '処理が異常終了しました。'
              ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
              ?KEKA3 :=  ''
    END
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
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

    RETURN    PGMEC-?PGMEC

```
