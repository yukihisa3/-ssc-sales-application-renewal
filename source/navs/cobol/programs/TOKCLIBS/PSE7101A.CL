/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    Ｊ本田請求システム　　　             *  ./
/. *   JOB-ID      :    PSE7101A                             *  ./
/. *   JOB-NAME    :    Ｊ本田請求データ作成　　             *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSE7101A'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                  /.      2    ./
    VAR ?OPR3     ,STRING*50                  /.      3    ./
    VAR ?OPR4     ,STRING*50                  /.      4    ./
    VAR ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'Ｊ本田請求処理'
    ?SYORINM1 :=  'Ｊ本田請求処理'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃　ジョイフル本田　請求締処理　＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　ジョイフル本田請求締処理を行います。'
    ?OPR3  :=  '　　確認後、入力／実行ボタンを押して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##請求データバックアップ##./
PBACKUP:

    ?STEP :=   'PBACKUP '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.# SAVFILE FILE-JDSYUKF.TOKKLIB/JDSEIKF.TOKKLIB/
            JDJSEKF.TOKKLIB,TODEV-MO,MODE-@USED
#./ IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'Ｊ本田データバックアップ'
              GOTO ABEND
    END

/.##請求データ削除##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  CLRFILE FILE-JDSEIKF.TOKKLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'Ｊ本田請求１削除'
              GOTO ABEND
    END
./
    CLRFILE FILE-JDJSEKF.TOKKLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'Ｊ本田請求２削除'
              GOTO ABEND
    END
/.##サカタ請求締処理##./
SSE7101B:

    ?STEP :=   'SSE7101B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENL5,TOFILE-SHTDENL5.TOKFLIB
    OVRF      FILE-JDSEIKL1,TOFILE-JDSEIKL1.TOKKLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSE7101A.TOKELIBO,PARA-(?SYORIKN1)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '売上請求ＤＴ作成'
              GOTO ABEND
    END

/.##仕入実績ﾌｧｲﾙ請求更新##./
SSE7102B:

    ?STEP :=   'SSE7102B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JDSYUKL1,TOFILE-JDSYUKL1.TOKKLIB
    OVRF      FILE-JDJSEKL1,TOFILE-JDJSEKL1.TOKKLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSE7102B.TOKELIB,PARA-(?SYORIKN2)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '仕入実績請求作成'
              GOTO ABEND
    END

/.##請求データ作成##./
SSE7103B:

    ?STEP :=   'SSE7103B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JDSEIKL1,TOFILE-JDSEIKL1.TOKKLIB
    OVRF      FILE-JDJSEKL1,TOFILE-JDJSEKL1.TOKKLIB
    CALL      PGM-SSE7103B.TOKELIB,PARA-(?SYORIKN3)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '請求データ統合'
              GOTO ABEND
    END


RTN:

    ?KEKA1 :=  'ジョイフル本田請求処理が正常終了しました。'
    ?KEKA2 :=  '確認して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKFLIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'Ｊ本田請求処理が正常終了しました。件数を確認し　'
    ?SYORINM3 := 'て下さい。１：売上、２：出荷、３：統合　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?KEKA1 :=  'ジョイフル本田請求処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKFLIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'Ｊ本田請求処理が異常終了しました。　　　　　　　'
    ?SYORINM3 := 'ログを採取して、ＮＡＶへ連絡して下さい。　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
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