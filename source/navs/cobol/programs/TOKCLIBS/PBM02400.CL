/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳ                           *  ./
/. *   JOB-ID      :    PBM02400                             *  ./
/. *   JOB-NAME    :    流通ＢＭＳイオンデータ削除           *  ./
/. *   2013/03/01 画面指示追加　本社／九州の処理方法を変更   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?TORICD   ,STRING*8,VALUE-'        ' /.取引先コード./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PBM02400'
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
    ?SYORINM1 :=  '流通ＢＭＳイオン請求データ消込処理'
    ?PGNM     :=  '流通ＢＭＳイオン請求データ消込処理'

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/BMSFLIB

/.##請求データ削除指示##./
SBM0250I:

    ?STEP :=   'SBM0250I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM0250I.TOKELIBO,PARA-(?TORICD,?SIMEBI)
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

    CLRFILE FILE-BMSSIHW.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '流通ＢＭＳ支払ワーク初期化'
              GOTO ABEND
    END

/.##処理判定　パラ：取引先＝８７３７の場合は連続処理へ##./
/.##パラ：取引先＝８７３７以外は、個別取引先処理へ　　##./
PHANTEI:

    ?STEP :=   'PHANTEI '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?TORICD  =  '00008737'  THEN
        ?MSGX :=  '＃連続処理開始！！＃'
        SNDMSG    ?MSGX,TO-XCTL
        GOTO      REZ87372
    END

/.##支払ワーク作成（個別取引先ＣＤの場合）##./
SBM0260B:

    ?STEP :=   'SBM0260B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-BMSSIHL2,TOFILE-BMSSIHL2.BMSFLIB
    OVRF      FILE-BMSSIHW,TOFILE-BMSSIHW.BMSFLIB
    CALL      PGM-SBM0260B.TOKELIBO,PARA-(?TORICD,?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '支払ワーク作成'
              GOTO ABEND
    ELSE
              ?MSGX :=  '＃イオン消込処理　開始＃'
              SNDMSG    ?MSGX,TO-XCTL
              GOTO SBM0240B
    END

/.##支払ワーク作成（取引先＝８７３７２）##./
REZ87372:

    ?STEP :=   'REZ87372'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-BMSSIHL2,TOFILE-BMSSIHL2.BMSFLIB
    OVRF      FILE-BMSSIHW,TOFILE-BMSSIHW.BMSFLIB
    CALL      PGM-SBM0260B.TOKELIBO,PARA-('00087372',?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '支払ワーク作成（８７３７２）'
              GOTO ABEND
    END

/.##支払ワーク作成（取引先＝８７３７３）##./
REZ87373:

    ?STEP :=   'REZ87373'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-BMSSIHL2,TOFILE-BMSSIHL2.BMSFLIB
    OVRF      FILE-BMSSIHW,TOFILE-BMSSIHW.BMSFLIB
    CALL      PGM-SBM0260B.TOKELIBO,PARA-('00087373',?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '支払ワーク作成（８７３７３）'
              GOTO ABEND
    END

/.##支払ワーク作成（取引先＝８７３７４）##./
REZ87374:

    ?STEP :=   'REZ87374'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-BMSSIHL2,TOFILE-BMSSIHL2.BMSFLIB
    OVRF      FILE-BMSSIHW,TOFILE-BMSSIHW.BMSFLIB
    CALL      PGM-SBM0260B.TOKELIBO,PARA-('00087374',?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '支払ワーク作成（８７３７４）'
              GOTO ABEND
    END

/.##支払ワーク作成（取引先＝８７３７５）##./
REZ87375:

    ?STEP :=   'REZ87375'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-BMSSIHL2,TOFILE-BMSSIHL2.BMSFLIB
    OVRF      FILE-BMSSIHW,TOFILE-BMSSIHW.BMSFLIB
    CALL      PGM-SBM0260B.TOKELIBO,PARA-('00087375',?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '支払ワーク作成（８７３７５）'
              GOTO ABEND
    END
/.##2015/11/19 NAV イオンリテールストア追加 ##./
/.##支払ワーク作成（取引先＝８７３７６）##./
REZ87376:

    ?STEP :=   'REZ87376'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-BMSSIHL2,TOFILE-BMSSIHL2.BMSFLIB
    OVRF      FILE-BMSSIHW,TOFILE-BMSSIHW.BMSFLIB
    CALL      PGM-SBM0260B.TOKELIBO,PARA-('00087376',?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '支払ワーク作成（８７３７６）'
              GOTO ABEND
    END

/.##請求合計Ｆ消し込み##./
SBM0240B:

    ?STEP :=   'SBM0240B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF        ?TORICD  =  '00033189'  THEN
        OVRF      FILE-SETGKFP3,TOFILE-SEIGK463.TOKFLIB
    ELSE
        OVRF      FILE-SETGKFP3,TOFILE-SETGKFP3.TOKFLIB
    END
    OVRF      FILE-BMSSIHW1,TOFILE-BMSSIHW1.BMSFLIB
    CALL      PGM-SBM0240B.TOKELIBO,PARA-(?SYORIKN1)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
/.##請求合計Ｆ圧縮処理##./
CNDPF:

    ?STEP :=   'CNDPF   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF        ?TORICD  ^= '00033189'  THEN
      DACTLF FILE-SETGKFP1.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＤＡＣＴＬＦ１（８７３７）'
                GOTO ABEND END
      DACTLF FILE-SETGKFP2.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＤＡＣＴＬＦ２（８７３７）'
                GOTO ABEND END
      DACTLF FILE-SETGKFP3.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＤＡＣＴＬＦ３（８７３７）'
                GOTO ABEND END
      CNDPF  FILE-SETGKFP.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＣＮＤＰＦ　　（８７３７）'
                GOTO ABEND END
      ACTLF  FILE-SETGKFP1.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＡＣＴＬＦ１　（８７３７）'
                GOTO ABEND END
      ACTLF  FILE-SETGKFP2.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＡＣＴＬＦ２　（８７３７）'
                GOTO ABEND END
      ACTLF  FILE-SETGKFP3.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＡＣＴＬＦ３　（８７３７）'
                GOTO ABEND END
    END
    IF        ?TORICD  =  '00033189'  THEN
      DACTLF FILE-SEIGK461.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＤＡＣＴＬＦ１（３３１８９）'
                GOTO ABEND END
      DACTLF FILE-SEIGK462.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＤＡＣＴＬＦ２（３３１８９）'
                GOTO ABEND END
      DACTLF FILE-SEIGK463.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＤＡＣＴＬＦ３（３３１８９）'
                GOTO ABEND END
      CNDPF  FILE-SEIGK46.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＣＮＤＰＦ　　（３３１８９）'
                GOTO ABEND END
      ACTLF  FILE-SEIGK461.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＡＣＴＬＦ１　（３３１８９）'
                GOTO ABEND END
      ACTLF  FILE-SEIGK462.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＡＣＴＬＦ２　（３３１８９）'
                GOTO ABEND END
      ACTLF  FILE-SEIGK463.TOKFLIB
      IF        @PGMEC    ^=   0    THEN
                ?KEKA4 :=  'ＡＣＴＬＦ３　（３３１８９）'
                GOTO ABEND END
    END
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'イオン請求データ消込処理が正常終了'
    ?KEKA2 :=  'しました。'
    ?KEKA3 :=  '消込結果を確認して下さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM2 := '流通ＢＭＳ請求ＤＴ消込処理が正常終了しました。'
    ?SYORINM3 := '件数を確認して下さい。　　　　　　　　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'イオン請求データ消込処理が異常終了'
    ?KEKA2 :=  'しました。ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  '↓下記が、エラー発生箇所です。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM2 := '流通ＢＭＳ請求ＤＴ消込処理が異常終了しました。　'
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