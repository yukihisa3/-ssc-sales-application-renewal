/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳ                           *  ./
/. *   JOB-ID      :    PBM04300                             *  ./
/. *   JOB-NAME    :    請求データ削除（ビバホーム）　　　   *  ./
/. ***********************************************************  ./
    PGM (P1-?PRTORI)
    PARA      ?PRTORI ,STRING*8,IN,VALUE-'        ' /.取引先コード./
    VAR       ?TORICD   ,STRING*8,VALUE-'        ' /.取引先コード./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PBM04300'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR       ?SIMEBI   ,STRING*8,VALUE-'00000000'    /.締日./
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?SYORINM1 :=  '流通ＢＭＳビバホーム請求データ消込'
    ?PGNM     :=  '流通ＢＭＳビバホーム請求データ消込'

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/BMSFLIB

/.##支払ワークファイルロック##./
ASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-BMSSI6W.BMSFLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  'ビバホーム支払ワークファイルロック'
              GOTO ABEND
    END

/.##請求データ削除指示##./
SBM0252I:

    ?STEP :=   'SBM0252I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM0252I.TOKELIBO,PARA-(?PRTORI,?TORICD,?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '請求データ削除指示'
              GOTO ABEND
         END
    END

    ?MSGX :=  '消込取引先＝' && ?TORICD && '　締日　＝' && ?SIMEBI
    SNDMSG    ?MSGX,TO-XCTL

/.##流通ＢＭＳ支払ワーク初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-BMSSI6W.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '流通ＢＭＳ支払ワーク初期化'
              GOTO ABEND
    END

PHANTEI:

    ?STEP :=   'PHANTEI '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?TORICD  =  ?PRTORI     THEN
        ?MSGX :=  '＃削除処理開始！！＃'
        SNDMSG    ?MSGX,TO-XCTL
        GOTO      SBM0209B
    ELSE
        SNDMSG    '指定された取引先コードが異なります！',TO-XCTL
        RETURN    PGMEC-@PGMEC
    END

/.##支払ワーク作成（個別取引先ＣＤの場合）##./
SBM0209B:

    ?STEP :=   'SBM0209B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-BMSSIHL2,TOFILE-BMSSIHL2.BMSFLIB
    OVRF      FILE-BMSSI6W,TOFILE-BMSSI6W.BMSFLIB
    CALL      PGM-SBM0209B.TOKSOLIB,PARA-(?TORICD,?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '支払ワーク作成'
              GOTO ABEND
    ELSE
              ?MSGX :=  '＃請求合計Ｆ消込処理　開始＃'
              SNDMSG    ?MSGX,TO-XCTL
              GOTO SBM0430B
    END

/.##請求合計Ｆ消し込み##./
SBM0430B:

    ?STEP :=   'SBM0430B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SETGKFP3,TOFILE-SETGKFV3.TOKKLIB
    OVRF      FILE-BMSSIHW1,TOFILE-BMSSI6W1.BMSFLIB
    CALL      PGM-SBM0430B.TOKSOLIB,PARA-(?SYORIKN1)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##請求合計Ｆ圧縮処理##./
CNDPF:

    ?STEP :=   'CNDPF   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DACTLF FILE-SETGKFV1.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＤＡＣＴＬＦ１'
              GOTO ABEND END
    DACTLF FILE-SETGKFV2.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＤＡＣＴＬＦ２'
              GOTO ABEND END
    DACTLF FILE-SETGKFV3.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＤＡＣＴＬＦ３'
              GOTO ABEND END
    CNDPF  FILE-SETGKFV.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＣＮＤＰＦ'
              GOTO ABEND END
    ACTLF  FILE-SETGKFV1.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＡＣＴＬＦ１'
              GOTO ABEND END
    ACTLF  FILE-SETGKFV2.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＡＣＴＬＦ２'
              GOTO ABEND END
    ACTLF  FILE-SETGKFV3.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＡＣＴＬＦ３'
              GOTO ABEND END

RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ビバホーム請求データ消込処理が'
    ?KEKA2 :=  '正常終了しました。'
    ?KEKA3 :=  '消込結果を確認して下さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM2 := '流通ＢＭＳ請求ＤＴ消込処理が正常終了しました。'
    ?SYORINM3 := '件数を確認して下さい。（ビバホーム）　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ビバホーム　請求データ消込処理が　　　　　'
    ?KEKA2 :=  '異常終了しました。ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  '↓下記が、エラー発生箇所です。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM2 := 'ビバホーム請求ＤＴ消込処理が異常終了しました。　'
    ?SYORINM3 := 'ログを採取して、ＮＡＶへ連絡して下さい。　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
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