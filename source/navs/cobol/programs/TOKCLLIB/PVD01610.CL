/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携（入荷予定管理）　　     *  ./
/. *   JOB-ID      :    PVD01600                             *  ./
/. *   JOB-NAME    :    入荷予定照会                         *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PVD01600'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '
    VAR ?WKSTN    ,STRING*8,VALUE-'        '
    VAR ?NWKSTN   ,NAME
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?FILNMN   ,STRING*4,VALUE-'NZAN'
    VAR ?LIBNM    ,STRING*8,VALUE-'D365DLIB'
    VAR ?NZANWKF  ,NAME!MOD
    VAR ?NZANWKFN ,STRING*17
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME

/.##WKSTN名取得##./
    ?NWKSTN   :=       @ORGWS
    ?WKSTN    :=       %STRING(?NWKSTN)

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '新入荷予定照会'

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.###ﾗｲﾌﾞﾗﾘﾘｽﾄ登録###./
    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKMDLIB/TOKSOLIB/TOKELIBO

/.###倉庫ＣＤ取得###./
SKY1601B:
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WKSTN,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【倉庫ＣＤ取得】'
              GOTO ABEND END


/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 倉庫CD = ' && ?SOKCD && '-' && ?DSOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMN && ?SOKCD && 'L1'
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?NZANWKF  :=    %NCAT(?FILID,?LIBID)
    ?NZANWKFN :=    %STRING(?NZANWKF)
    ?MSGX     :=    '## 照会ﾌｧｲﾙ名 = ' && ?NZANWKFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.###新入荷予定照会###./
NVD0160I:
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NZANWKL1,TOFILE-?NZANWKF
    OVRF      FILE-HACHEDL3,TOFILE-HACHEDL3.D365DLIB
    OVRF      FILE-HACMEIL1,TOFILE-HACMEIL1.D365DLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      PGM-NVD0160I.TOKSOLIB,PARA-(?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【新入荷予定照会】'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
/.  ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
./  ?PGMECX   :=    %STRING(?PGMEC)
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