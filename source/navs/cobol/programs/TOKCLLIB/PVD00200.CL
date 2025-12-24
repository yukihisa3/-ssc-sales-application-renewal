/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    入庫管理システム・Ｄ３６５連携       *  ./
/. *   JOB-ID      :    PVD00200                             *  ./
/. *   JOB-NAME    :    発注消込入力（入荷入力）             *  ./
/. ***********************************************************  ./
    PGM
/.##ﾜｰｸ定義##./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PVD00200'
    VAR       ?STEP     ,STRING*8
    VAR       ?TANCD    ,STRING*2,VALUE-'  '
    VAR       ?BUMON    ,STRING*4,VALUE-'    '
    VAR       ?TANBUM   ,STRING*6
    VAR       ?WS       ,STRING*8
    VAR       ?WKSTN    ,NAME
    VAR       ?PARAIN01 ,STRING*14
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '発注消込入力（入荷入力）'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   D365DLIB/TOKELIB/TOKELIBO/TOKSOLIB/TOKDLIB/TOKFLIB/
              TOKKLIB/HGWORKLB/TOKDTLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

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

/.##発注消込入力##./
NVD0020I:
    ?STEP :=   'NVD0020I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾌｧｲﾙ置き換え##./
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-TOKMS1,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL4,TOFILE-SHOTBL4.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-HACHEDL1,TOFILE-HACHEDL1.D365DLIB
    OVRF      FILE-HACMEIL1,TOFILE-HACMEIL1.D365DLIB
    OVRF      FILE-NYKFILL1,TOFILE-NYKFILL1.D365DLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB

    CALL      PGM-NVD0020I.TOKSOLIB,PARA-(?WS,?TANCD,?BUMON)
/.##ﾌﾟﾛｸﾞﾗﾑ終了ｺｰﾄﾞ判定##./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '送信データ作成処理が異常終了しました。'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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