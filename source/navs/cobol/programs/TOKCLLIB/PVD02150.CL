/. ***********************************************************  ./
/. *     サカタのタネ　新基幹システム                        *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携（入荷予定管理）         *  ./
/. *   JOB-ID      :    PVD02150                             *  ./
/. *   JOB-NAME    :    入荷消込データＣＳＶ出力             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PVD02150'
    VAR       ?STEP     ,STRING*8
    VAR       ?SOKO     ,STRING*2
    VAR       ?JISOKO   ,STRING*2
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門名./
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./
/.##ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?NWKSTN   :=        @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '入荷消込データＣＳＶ出力'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL    D365DLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKMDLIB/TOKELIBO/
               TOKDTLIB

/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WKSTN,?BUMON)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0
          THEN
              ?KEKA4 :=  '【部門コード取得】'
              GOTO ABEND
    END

/.##倉庫ＣＤ取得##./
SKY1601B:
    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WKSTN,?SOKO,?JISOKO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【倉庫コード取得】'
              GOTO ABEND END

/.##入荷消込データＣＳＶ出力##./
NVD0215V:
    ?STEP :=   'NVD0215V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-NYKFILL2,TOFILE-NYKFILL2.D365DLIB
    OVRF      FILE-HACHEDL1,TOFILE-HACHEDL1.D365DLIB
    OVRF      FILE-HACMEIL1,TOFILE-HACMEIL1.D365DLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-NYKESICS,TOFILE-NYKESICS.D365DLIB
    CALL      PGM-NVD0215V.TOKSOLIB,PARA-(?SOKO,?JISOKO,?BUMON)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              IF    ?PGMEC     =   4010 THEN
                    SNDMSG    '## 取 消 ##',TO-XCTL
                    GOTO RTN
              ELSE
                    ?KEKA4 :=  '【入荷消込データＣＳＶ出力】'
                    GOTO ABEND
              END
    END

/.##データＰＣ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-NYKESICS.D365DLIB,PARA-NYKESICS,UNIT-1
    ?PGMEC    :=   @PGMEC
    ?PGMEM    :=   @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【データＰＣ転送】'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
  /.?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM  ./
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