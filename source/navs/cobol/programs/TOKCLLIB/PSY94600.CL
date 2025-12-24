/. ***********************************************************  ./
/. *     サカタのタネ　　　　　　　　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    ヨドバシ　　　ＥＤＩ　　　　　　　   *  ./
/. *   JOB-ID      :    PSY94600                             *  ./
/. *   JOB-NAME    :    ヨドバシ納期変更出荷確定　　　　　　 *  ./
/. *                    確認ＣＳＶ出力　  　　　　　　　　　 *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY94600'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8,VALUE-'        '    /.STEP-ID./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?JSDATE   ,STRING*8,VALUE-'00000000'    /.実行日付  ./
    VAR ?JEDATE   ,STRING*8,VALUE-'00000000'    /.実行日付  ./
    VAR ?JSTIME   ,STRING*6,VALUE-'000000'      /.実行時刻  ./
    VAR ?JETIME   ,STRING*6,VALUE-'000000'      /.実行時刻  ./
    VAR ?HKKBN    ,STRING*1,VALUE-' '           /.発行区分./
    VAR ?SOKCD    ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?DENNO    ,STRING*180                   /.伝票番号./
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
    ?PGNM := 'ヨドバシ確認ＣＳＶ出力'
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


/.##ヨドバシ確認ＣＳＶ出力指示##./
SSY9460I:

    ?STEP :=  'SSY9460I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
 /. OVRF      FILE-SHTDENLJ,TOFILE-SHTDENLJ.TOKKLIB ./
 /. OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB     ./
 /. OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB   ./
    CALL      PGM-SSY9460I.TOKSOLIB,
              PARA-(?JSDATE,?JEDATE,?HKKBN)

    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  'ヨドバシ確認ＣＳＶ出力指示'
              GOTO ABEND
         END
    END

    ?JSTIME := '000000'
    ?JETIME := '999999'
    ?MSGX :=  '実行日　開始＝' && ?JSDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '実行時刻開始＝' && ?JSTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '実行日　終了＝' && ?JEDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '実行時刻終了＝' && ?JETIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '発行区分　　＝' && ?HKKBN
    SNDMSG    ?MSGX,TO-XCTL

/.##ヨドバシ確認ＣＳＶ出力##./
SSY9430V:

    ?STEP :=   'SSY9430V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-YODNKKL2,TOFILE-YODNKKL2.TOKDTLIB
    CALL      PGM-SSY9430V.TOKSOLIB,
              PARA-(?JSDATE,?JEDATE,
                    ?JSTIME,?JETIME,
                    ?HKKBN,?SYORIKN1)

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ヨドバシ確認ＣＳＶ出力'
              GOTO ABEND
    END

    ?MSGX :=  'ＣＳＶ件数　＝' && ?SYORIKN1
    SNDMSG    ?MSGX,TO-XCTL

    IF        ?SYORIKN1 = '0000000' THEN
              ?KEKA4 :=  '確認ＣＳＶ出力'
              GOTO      ABEND
    END

/.##ヨドバシ納期変更出荷確定確認CSV転送##./
FIMPORT1:

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-YODNKKCV.TOKDTLIB,
            TYPE-@FILE,
            PARA-YODOBASI,
            UNIT-5,
            OPR-@NO

    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '確認ＣＳＶデータ転送'
                    GOTO ABEND
    END
RTN:

 /. OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '処理結果表示'
              GOTO ABEND
    END
 ./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    =  0        THEN
       IF     ?SYORIKN1 = '0000000' THEN
              ?KEKA1   := '対象データ０件です。'
              ?KEKA2   := '指示条件やデータ有無を'
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
