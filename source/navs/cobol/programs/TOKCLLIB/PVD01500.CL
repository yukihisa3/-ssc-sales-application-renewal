/. ***********************************************************  ./
/. *     サカタのタネ　新基幹システム                        *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携（入荷予定管理）         *  ./
/. *   JOB-ID      :    PVD01500                             *  ./
/. *   JOB-NAME    :    入荷予定リスト　                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PVD01500'
    VAR       ?STEP     ,STRING*8
    VAR       ?SOKO     ,STRING*2
    VAR       ?JISOKO   ,STRING*2
    VAR       ?OUTKBN   ,STRING*1,VALUE-' '
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./
    ?NWKSTN    :=       @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '新入荷予定リスト発行'
/.ﾗｲﾌﾞﾗﾘﾘｽﾄ登録./
    DEFLIBL    D365DLIB/TOKFLIB/TOKELIB/TOKELIBO/TOKSOLIB/TOKMDLIB/
               TOKDTLIB
/.入荷予定ワーク創成./
    CRTFILE FILE-NYYOTEIF.D365DLIB,ORG-@SF,BF-6,USED-@YES,
            RL-150,RECN-500,ERECN-500
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入庫予定ワーク創成】'
              GOTO ABEND END

/.倉庫ＣＤ取得./
SKY1601B:
    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WKSTN,?SOKO,?JISOKO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【倉庫ＣＤ取得】'
              GOTO ABEND END


/.新入荷予定リスト抽出./
NVD0150I:
    ?STEP :=   'NVD0150I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-HACHEDL2,TOFILE-HACHEDL2.D365DLIB
    OVRF      FILE-HACMEIL1,TOFILE-HACMEIL1.D365DLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-SUBMEIL1,TOFILE-SUBMEIL1.TOKDTLIB
    OVRF      FILE-NYYOTEIF,TOFILE-NYYOTEIF.D365DLIB
    CALL      PGM-NVD0150I.TOKSOLIB,PARA-(?SOKO,?JISOKO,?OUTKBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?MSGX :=  '**  出力区分 = ' && ?OUTKBN && '      **'
    SNDMSG    ?MSGX,TO-XCTL
    IF        ?PGMEC    ^=   0    THEN
              IF  ?PGMEC     =   4050 THEN
                  GOTO RTN
              ELSE
                  ?KEKA4 :=  '【新入荷予定リスト範囲指定】'
                  GOTO ABEND
              END
    END

/.ＳＯＲＴ./
SORT01:
    ?STEP :=   'SORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.ＳＯＲＴキー    ./
/.　_倉庫        ./
/.　_データ種別  ./
/.　_商品カテゴリ./
/.　_仕入先ＣＤ  ./
/.　_発注ＮＯ    ./
/.　_行ＮＯ      ./

    SORT       INFILE-NYYOTEIF.D365DLIB,INRL-150,INBF-6,
               OUTFILE-NYYOTEIF.D365DLIB,OUTBF-6,RCD-4,
               KEY-1!2!CA,
               KEY1-105!2!CA,
               KEY2-127!2!CA,
               KEY3-3!8!CA,
               KEY4-11!7!CA,
               KEY5-18!2!CA,
               RCDL-@DSP
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ＳＯＲＴ】'
              GOTO ABEND END

    IF        ?OUTKBN = '1'
        THEN
              GOTO   NVD0152V
    END

/.入荷予定リスト出力./
NVD0151L:
    ?STEP :=   'NVD0151L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYYOTEIF,TOFILE-NYYOTEIF.D365DLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
  /.OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB  ./
    OVRF      FILE-SUBMEIL1,TOFILE-SUBMEIL1.TOKDTLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-NVD0151L.TOKSOLIB,PARA-(?SOKO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷予定リスト出力】'
              GOTO ABEND END
    GOTO RTN

/.入荷予定リストCSV出力./
NVD0152V:
    ?STEP :=   'NVD0152V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYYOTEIF,TOFILE-NYYOTEIF.D365DLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-SUBMEIL1,TOFILE-SUBMEIL1.TOKDTLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-NYYOTECV,TOFILE-NYYOTECV.D365DLIB
    CALL      PGM-NVD0152V.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷予定リストＣＳＶ出力】'
              GOTO ABEND END

/.##データＰＣ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-NYYOTECV.D365DLIB,PARA-NYYOTEIF,UNIT-1
    ?PGMEC    :=   @PGMEC
    ?PGMEM    :=   @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷予定リストＣＳＶ転送】'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    DLTFILE   NYYOTEIF.D365DLIB
    RETURN    PGMEC-?PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
  /.?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM   ./
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

    DLTFILE   NYYOTEIF.D365DLIB
    RETURN    PGMEC-?PGMEC
