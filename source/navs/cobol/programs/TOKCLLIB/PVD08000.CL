/. ***********************************************************  ./
/. *     サカタのタネ　新基幹システム                        *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携（入荷予定管理）         *  ./
/. *   JOB-ID      :    PVD08000                             *  ./
/. *   JOB-NAME    :    入荷予定エラーリスト発行             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PVD08000'
    VAR       ?STEP     ,STRING*8
    VAR       ?SOKO     ,STRING*2
    VAR       ?JISOKO   ,STRING*2
    VAR       ?SELKBN   ,STRING*1,VALUE-' '
    VAR       ?ONLNO    ,STRING*10,VALUE-'          '
    VAR       ?TDATE    ,STRING*8,VALUE-'        '
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
    ?PGNM :=  '入荷予定エラーリスト発行'
/.ﾗｲﾌﾞﾗﾘﾘｽﾄ登録./
    DEFLIBL    D365DLIB/TOKFLIB/TOKELIB/TOKELIBO/TOKSOLIB/TOKMDLIB/
               TOKDTLIB

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


/.入荷予定エラーリスト発行指示./
NVD0800I:
    ?STEP :=   'NVD0800I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-DNDTKNL1,TOFILE-DNDTKNL1.D365DLIB
    OVRF      FILE-DNDTKNL3,TOFILE-DNDTKNL3.D365DLIB
    CALL      PGM-NVD0800I.TOKSOLIB,
              PARA-(?SELKBN,?ONLNO,?TDATE,?OUTKBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?MSGX :=  '**  指定区分 = ' && ?SELKBN && '      **'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '**  送受信_ = ' && ?ONLNO  && '      **'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '**  取込日付 = ' && ?TDATE  && '      **'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '**  出力区分 = ' && ?OUTKBN && '      **'
    SNDMSG    ?MSGX,TO-XCTL
    IF        ?PGMEC    ^=   0    THEN
              IF  ?PGMEC     =   4050 THEN
                  SNDMSG     '【取消終了】',TO-XCTL
                  GOTO RTN
              ELSE
                  ?KEKA4 :=  '【入荷予定エラーリスト発行指示】'
                  GOTO ABEND
              END
    END

    IF        ?OUTKBN = ' '
        THEN
              GOTO   NVD0810V
    END

/.入荷予定エラーリスト出力./
NVD0810L:
    ?STEP :=   'NVD0810L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DNDTKNL1,TOFILE-DNDTKNL1.D365DLIB
    OVRF      FILE-DNDTKNL3,TOFILE-DNDTKNL3.D365DLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-NVD0810L.TOKSOLIB,
              PARA-(?SELKBN,?ONLNO,?TDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷予定エラーリスト出力】'
              GOTO ABEND END
    GOTO RTN

/.入荷予定エラーリストCSV出力./
NVD0810V:
    ?STEP :=   'NVD0810V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DNDTKNL1,TOFILE-DNDTKNL1.D365DLIB
    OVRF      FILE-DNDTKNL3,TOFILE-DNDTKNL3.D365DLIB
    OVRF      FILE-SUBMEIL1,TOFILE-SUBMEIL1.TOKDTLIB
    OVRF      FILE-NYYOTEER,TOFILE-NYYOTEER.D365DLIB
    CALL      PGM-NVD0810V.TOKSOLIB,
              PARA-(?SELKBN,?ONLNO,?TDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
      IF        ?PGMEC    =   4011    THEN
                ?KEKA4 :=  '【出力対象データが存在しません】'
                GOTO DATANASI
      ELSE
                ?KEKA4 :=  '【入荷予定エラーＣＳＶ出力】'
                GOTO ABEND
      END
    END

/.##データＰＣ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-NYYOTEER.D365DLIB,PARA-NYYOTEER,UNIT-1
    ?PGMEC    :=   @PGMEC
    ?PGMEM    :=   @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷予定エラーリストＣＳＶ転送】'
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
    ?PGMEM    :=    @PGMEM./
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

DATANASI:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '出力対象のエラーデータが存在しません！'
    ?KEKA2 :=  '確認お願い致します。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
