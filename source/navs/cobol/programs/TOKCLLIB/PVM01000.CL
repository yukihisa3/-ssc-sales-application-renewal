/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    マスタメンテ（内部統制対応）         *  ./
/. *   JOB-ID      :    PVM01000                             *  ./
/. *   JOB-NAME    :    新．商品変換ＴＢＬメンテ             *  ./
/. *   CRT-DATE    :    2020.03.24                           *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PVM01000'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門名./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?NYURYOKU ,STRING*8,VALUE-'99999999' /.入力日./
    VAR       ?STIME    ,STRING*4,VALUE-'    '     /.時刻./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?P1       ,STRING*34                 /.ﾊﾟﾗﾒﾀ./
    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKDTLIB/D365DLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WS,?BUMON)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##担当者CD指定##./
SKY6101I:

    ?STEP :=   'SKY6101I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SKY6101I.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0  THEN
              IF  @PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  GOTO  ABEND
              END
    END

/.##商品変換ＴＢＬメンテ##./
NVM0100I:

    ?STEP :=   'NVM0100I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-SUBTBLL1,TOFILE-SUBTBLL1.TOKDTLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SUBMEIL1,TOFILE-SUBMEIL1.TOKDTLIB
    OVRF      FILE-HSHOTBR1,TOFILE-HSHOTBR1.D365DLIB
    CALL      PGM-NVM0100I.TOKSOLIB,PARA-(?BUMON,?TANCD,?NYURYOKU,
                                         ?STIME)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '担当者 = ' && ?TANCD
    SNDMSG ?MSGX,TO-XCTL
    ?MSGX := '部門　 = ' && ?BUMON
    SNDMSG ?MSGX,TO-XCTL
    ?MSGX := '入力日 = ' && ?NYURYOKU
    SNDMSG ?MSGX,TO-XCTL
    ?MSGX := '時刻   = ' && ?STIME
    SNDMSG ?MSGX,TO-XCTL
    ?P1 := '1' && ?BUMON && ?TANCD && ?TANCD && ?NYURYOKU &&
           ?NYURYOKU && ' ' && ?STIME && ?STIME
    SNDMSG ?P1,TO-XCTL
/.##実行判定##./
    IF  ?NYURYOKU = '99999999'  THEN
         SNDMSG MSG-'##途中ｷｬﾝｾﾙ ##',TO-XCTL
         SNDMSG MSG-'##自動出力ﾅｼ##',TO-XCTL
    END


/.##確認画面##./
STEP010:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    ?OPR1  :=  '【自動出力確認画面】'
    ?OPR2  :=  '商品変換テーブルリストを自動で出力するか'
    ?OPR3  :=  'しないか選択して下さい。'
    ?OPR4  :=  'ＰＦ９　　：自動出力を行なわない。'
    ?OPR5  :=  'ＥＮＴＥＲ：自動出力を行なう。'
    CALL      PAUSE.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##商品変換テーブルリスト##./
NVM0110L:

    ?STEP :=   'NVM0110L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##本社ﾚｰｻﾞｰへ出力##./
  /. 実機に合わせてください ./
    IF ?BUMON = '2920' THEN
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    ELSE
       OVRPRTF FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
    END
 /. OVRPRTF FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
 ./
    OVRF     FILE-HSHOTBR2,TOFILE-HSHOTBR2.D365DLIB
    OVRF     FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF     FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF     FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF     FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL     PGM-NVM0110L.TOKSOLIB,PARA-(?P1)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

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

    RETURN    PGMEC-?PGMEC
