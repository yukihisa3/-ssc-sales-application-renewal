/. ***********************************************************  ./
/. *     サカタのタネ　営業第２部　ＮＡＶＳ　　　　          *  ./
/. *   SYSTEM-NAME :    ＨＨＴ_卸機能　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PTN00210                             *  ./
/. *   JOB-NAME    :    倉庫_卸関係データ取得　　　　　　   *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PTN00210'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*2,VALUE-'  '          /.倉庫ＣＤ  ./
    VAR ?P2       ,STRING*2,VALUE-'  '          /.倉庫(代表)./
    VAR ?P3       ,STRING*1,VALUE-' '           /.出力区分./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(開始)./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(終了)./
    VAR ?P6       ,STRING*1,VALUE-'1'           /.帳票巣別./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.      2    ./
    VAR ?OPR3     ,STRING*50                    /.      3    ./
    VAR ?OPR4     ,STRING*50                    /.      4    ./
    VAR ?OPR5     ,STRING*50                    /.      5    ./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNM1   ,STRING*6,VALUE-'YTTANA'
    VAR ?FILNM2   ,STRING*6,VALUE-'HHTSYO'
    VAR ?FILNM3   ,STRING*6,VALUE-'HHTTAN'
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKDTLIB'
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?YTTANAF  ,NAME!MOD
    VAR ?YTTANAFN ,STRING*17
    VAR ?HHTSYOF  ,NAME!MOD
    VAR ?HHTSYOFN ,STRING*17
    VAR ?HHTTANF  ,NAME!MOD
    VAR ?HHTTANFN ,STRING*17
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '_卸予定データ作成'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKMDLIB/ZAIEXCEL/TOKFLIB/TOKKLIB
           /TOKDTLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P1,?P2)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ＣＤ = ' && ?P1 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNM1 && ?P1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?YTTANAF  :=    %NCAT(?FILID,?LIBID)
    ?YTTANAFN :=    %STRING(?YTTANAF)
    ?MSGX     :=    '## _卸予定Ｆ = ' && ?YTTANAFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNM2 && ?P1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HHTSYOF  :=    %NCAT(?FILID,?LIBID)
    ?HHTSYOFN :=    %STRING(?HHTSYOF)
    ?MSGX     :=    '## _卸商品Ｆ = ' && ?HHTSYOFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNM3 && ?P1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HHTTANF  :=    %NCAT(?FILID,?LIBID)
    ?HHTTANFN :=    %STRING(?HHTTANF)
    ?MSGX     :=    '## _卸予定Ｆ = ' && ?HHTTANFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##_卸関係データ取得確認##./
PPAUSE:

    ?STEP :=   'PPAUSE  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＜倉庫_卸関係データ取得＞＃＃'
    ?OPR2  :=  '　ＨＨＴ_卸に必要なデータの取得を'
    ?OPR3  :=  '　行ないます。'
    ?OPR4  :=  '　確認後、実行して下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##_卸予定ＣＳＶデータ作成##./
STN0021B:

    ?STEP :=   'STN0021B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-YTTANACS,TOFILE-?YTTANAF
    CALL      PGM-STN0021B.TOKSOLIB,PARA-(?P1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸予定ＣＳＶデータ作成'
              GOTO ABEND END

/.##_卸予定ＣＳＶデータ転送##./
PFIMPOR1:

    ?STEP :=   'PFIMPOR1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-?YTTANAF,PARA-HHTTANA,UNIT-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸予定ＣＳＶデータ転送'
              GOTO ABEND END

/.##_卸商品ＣＳＶデータ作成##./
STN0061B:

    ?STEP :=   'STN0060B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-HHTSYOCS,TOFILE-?HHTSYOF
    CALL      PGM-STN0061B.TOKSOLIB,PARA-(?P1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸商品ＣＳＶデータ作成'
              GOTO ABEND END

/.##_卸商品ＣＳＶデータ転送##./
PFIMPOR2:

    ?STEP :=   'PFIMPOR2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-?HHTSYOF,PARA-HHTTANA,UNIT-2
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸商品ＣＳＶデータ転送'
              GOTO ABEND END

/.##_卸担当者ＣＳＶデータ作成##./
STN0071B:

    ?STEP :=   'STN0071B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-HHTTANCS,TOFILE-?HHTTANF
    CALL      PGM-STN0071B.TOKSOLIB,PARA-(?P1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸担当者ＣＳＶデータ作成'
              GOTO ABEND END

/.##_卸担当者ＣＳＶデータ転送##./
PFIMPOR3:

    ?STEP :=   'PFIMPOR2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-?HHTTANF,PARA-HHTTANA,UNIT-3
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸担当者ＣＳＶデータ転送'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '＜倉庫_卸関係データ取得＞'
    ?KEKA2 :=  '倉庫簗卸関係データ取得処理が正常終了'
    ?KEKA3 :=  'しました。作成結果の確認をお願い致し'
    ?KEKA4 :=  'ます。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '＜倉庫_卸関係データ取得＞'
    ?KEKA2 :=  '処理が異常終了しました。ＬＯＧを採取し'
    ?KEKA3 :=  'ＮＡＶへ連絡して下さい'
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