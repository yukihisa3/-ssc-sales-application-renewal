/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳ　　　　支払照合システム　 *  ./
/. *   JOB-ID      :    PBM02200    2012/11/27               *  ./
/. *   JOB-NAME    :    不照合リスト発行                     *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PBM02200'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.取引先CD./
    VAR ?P2       ,STRING*8,VALUE-'00000000'    /.締日./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門CD./
    VAR ?TANTO    ,STRING*8,VALUE-'        '    /.担当者CD./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '流通ＢＭＳ　不照合リスト発行'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/BMSFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##部門ｺｰﾄﾞ担当者ｺｰﾄﾞ取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANTO)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##不照合リスト発行指示./
SBM0220I:

    ?STEP :=   'SBM0220I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM0220I.TOKELIBO,PARA-(?P1,?P2)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END

  /.SNDMSG  ?P1,TO-XCTL
    SNDMSG  ?P2,TO-XCTL   ./
    ?MSGX :=  '取引先＝' && ?P1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '締日　＝' && ?P2
    SNDMSG    ?MSGX,TO-XCTL

/.##支払ワーク作成##./
SBM0205B:

    ?STEP :=   'SBM0205B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-BMSSIHL2,TOFILE-BMSSIHL2.BMSFLIB
    OVRF      FILE-BMSSIHW,TOFILE-BMSSIHW.BMSFLIB
    CALL      PGM-SBM0205A.OSKELIB,PARA-(?P1,?P2)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '支払ワーク作成'
              GOTO ABEND END

 /.対象データ抽出./
PSORT:

    ?STEP :=   'PSORT   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?P1 = '00087373'  THEN
        ?MSGX :=  '*** 取CD = 00087373 抽出中！！ ***'
        SNDMSG    ?MSGX,TO-XCTL
        SORTF     INFILE-SETGKFP.TOKFLIB,
                  OUTFILE-SETGKFP.OSKFLIB,
                  KEY-'F01!A',
                  SELECT-'F01!EQ!#00087373',
                  RCDL-@DSP
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4 :=  '【８７３７３　抽出中！！】'
                  GOTO ABEND END
    END
    IF  ?P1 = '00087372'  THEN
        ?MSGX :=  '*** 取CD = 00087372 抽出中！！ ***'
        SNDMSG    ?MSGX,TO-XCTL
        SORTF     INFILE-SETGKFP.TOKFLIB,
                  OUTFILE-SETGKFP.OSKFLIB,
                  KEY-'F01!A',
                  SELECT-'F01!EQ!#00087372',
                  RCDL-@DSP
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4 :=  '【８７３７２　抽出中！！】'
                  GOTO ABEND END
    END
    IF  ?P1 = '00087374'  THEN
        ?MSGX :=  '*** 取CD = 00087374 抽出中！！ ***'
        SNDMSG    ?MSGX,TO-XCTL
        SORTF     INFILE-SETGKFP.TOKFLIB,
                  OUTFILE-SETGKFP.OSKFLIB,
                  KEY-'F01!A',
                  SELECT-'F01!EQ!#00087374',
                  RCDL-@DSP
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4 :=  '【８７３７４　抽出中！！】'
                  GOTO ABEND END
    END
    IF  ?P1 = '00087375'  THEN
        ?MSGX :=  '*** 取CD = 00087375 抽出中！！ ***'
        SNDMSG    ?MSGX,TO-XCTL
        SORTF     INFILE-SETGKFP.TOKFLIB,
                  OUTFILE-SETGKFP.OSKFLIB,
                  KEY-'F01!A',
                  SELECT-'F01!EQ!#00087375',
                  RCDL-@DSP
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4 :=  '【８７３７５　抽出中！！】'
                  GOTO ABEND END
    END
/.##2015/11/19 NAV ｲｵﾝﾘﾃｰﾙｽﾄｱ追加##./
    IF  ?P1 = '00087376'  THEN
        ?MSGX :=  '*** 取CD = 00087376 抽出中！！ ***'
        SNDMSG    ?MSGX,TO-XCTL
        SORTF     INFILE-SETGKFP.TOKFLIB,
                  OUTFILE-SETGKFP.OSKFLIB,
                  KEY-'F01!A',
                  SELECT-'F01!EQ!#00087376',
                  RCDL-@DSP
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4 :=  '【８７３７６　抽出中！！】'
                  GOTO ABEND END
    END
/.##2015/11/19 NAV ｲｵﾝﾘﾃｰﾙｽﾄｱ追加##./
    IF  ?P1 = '00033189'  THEN
        ?MSGX :=  '*** 取CD = 00033189 抽出中！！ ***'
        SNDMSG    ?MSGX,TO-XCTL
        SORTF     INFILE-SEIGK46.TOKFLIB,
                  OUTFILE-SETGKFP.OSKFLIB,
                  KEY-'F01!A',
                  SELECT-'F01!EQ!#00033189',
                  RCDL-@DSP
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4 :=  '【３３１８９　抽出中！！】'
                  GOTO ABEND END
    END
    IF  ?P1 = '00030402'  THEN
        ?MSGX :=  '*** 取CD = 00030402 抽出中！！ ***'
        SNDMSG    ?MSGX,TO-XCTL
        SORTF     INFILE-SETGKFS.TOKKLIB,
                  OUTFILE-SETGKFP.OSKFLIB,
                  KEY-'F01!A',
                  SELECT-'F01!EQ!#00030402',
                  RCDL-@DSP
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4 :=  '【３０４０２　抽出中！！】'
                  GOTO ABEND END
    END
    IF  ?P1 = '00304021'  THEN
        ?MSGX :=  '*** 取CD = 00304021 抽出中！！ ***'
        SNDMSG    ?MSGX,TO-XCTL
        SORTF     INFILE-SETGKFS.TOKKLIB,
                  OUTFILE-SETGKFP.OSKFLIB,
                  KEY-'F01!A',
                  SELECT-'F01!EQ!#00304021',
                  RCDL-@DSP
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4 :=  '【３０４０２１抽出中！！】'
                  GOTO ABEND END
    END

/.##不照合リスト発行##./
SBM0230L:

    ?STEP :=   'SBM0230L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?BUMON = '2920'  THEN
        ?MSGX :=  '##本社ﾚｰｻﾞｰへ出力##'
        SNDMSG    ?MSGX,TO-XCTL
        OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    ELSE
        IF  ?BUMON = '2940'  THEN
            ?MSGX :=  '##九州ﾚｰｻﾞｰへ出力##'
            SNDMSG    ?MSGX,TO-XCTL
            OVRPRTF FILE-XU04LP,TOFILE-AMAGILBP.XUCL
        END
    END
    OVRF      FILE-BMSSIHW2,TOFILE-BMSSIHW2.BMSFLIB
    OVRF      FILE-SETGKFP3,TOFILE-SETGKFP3.OSKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SBM0230T.OSKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '不照合リスト発行'
              GOTO ABEND END


RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '出力内容を確認して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
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

    RETURN    PGMEC-@PGMEC
