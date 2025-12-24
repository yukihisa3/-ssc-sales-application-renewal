/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    マスタメンテ                         *  ./
/. *   JOB-ID      :    PZA08400                             *  ./
/. *   JOB-NAME    :    商品在庫マスタリスト（_番順）       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZA08400'
    VAR       ?STEP     ,STRING*8

    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./

    VAR       ?SOKCD    ,STRING*2
    VAR       ?DSOKCD   ,STRING*2
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN   :=        @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKFLIB/TOKELIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '在庫マスタリスト出力（返品_順）'

/.  倉庫ＣＤ取得                                                ./
SKY1601B:
    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WKSTN,?SOKCD,?DSOKCD)
    ?PGMEC    :=   @PGMEC
    ?PGMEM    :=   @PGMEM
  /.IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END   ./
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【倉庫コード取得】'
              GOTO ABEND
    END

/.  商品在庫マスタリスト                                        ./
SZA0230L:
    ?STEP :=   'SZA0230L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-ZAMZAIL4,TOFILE-ZAMZAIL4.TOKFLIB
    OVRF      FILE-ZAMJISL1,TOFILE-ZAMJISL1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-SUBMEIL1,TOFILE-SUBMEIL1.TOKDTLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SZA0840L.TOKSOLIB,PARA-(?SOKCD,?DSOKCD)
    ?PGMEC    :=   @PGMEC
    ?PGMEM    :=   @PGMEM
  /.IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END ./
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【在庫マスタリスト出力】'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '在庫マスタリスト出力　　が異常終了し'
    ?KEKA2 :=  'ました。ログリストを採取しＮＡＶへ連絡'
    ?KEKA3 :=  'して下さい。'
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